;;; perspective.el --- switch between named "perspectives" of the editor  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2020 Natalie Weizenbaum <nex342@gmail.com>
;;
;; Licensed under the same terms as Emacs and under the MIT license.

;; Author: Natalie Weizenbaum <nex342@gmail.com>
;; URL: http://github.com/nex3/perspective-el
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Version: 2.18
;; Created: 2008-03-05
;; By: Natalie Weizenbaum <nex342@gmail.com>
;; Keywords: workspace, convenience, frames

;;; Commentary:

;; This package provides tagged workspaces in Emacs, similar to
;; workspaces in windows managers such as Awesome and XMonad (and
;; somewhat similar to multiple desktops in Gnome or Spaces in OS X).

;; Perspective provides multiple workspaces (or "perspectives") for each Emacs
;; frame. This makes it easy to work on many separate projects without getting
;; lost in all the buffers.

;; Each perspective is composed of a window configuration and a set of
;; buffers.  Switching to a perspective activates its window
;; configuration, and when in a perspective only its buffers are
;; available by default.

;;; Code:

(require 'cl-lib)
(require 'ido)
(require 'rx)
(require 'subr-x)
(require 'thingatpt)


;;; --- customization

(defgroup perspective-mode 'nil
  "Customization for Perspective mode"
  :group 'frames)

(defcustom persp-initial-frame-name "main"
  "Name used for the initial perspective when enabling `persp-mode'."
  :type 'string
  :group 'perspective-mode)

(defcustom persp-show-modestring t
  "Determines if the list of perspectives is shown in the modeline.
If the value is 'header, the list of perspectives is shown in the
header line instead."
  :group 'perspective-mode
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Modeline" t)
                 (const :tag "Header" header)))

(defcustom persp-modestring-dividers '("[" "]" "|")
  "Plist of strings used to create the string shown in the modeline.
First string is the start of the modestring, second is the
closing of the mode string, and the last is the divider between
perspectives."
  :group 'perspective-mode
  :type '(list (string :tag "Open")
               (string :tag "Close")
               (string :tag "Divider")))

(defcustom persp-modestring-short nil
  "When t, show a shortened modeline string.
A shortened modeline string only displays the current perspective
instead of the full perspective list."
  :group 'perspective-mode
  :type 'boolean)

(defcustom persp-mode-prefix-key (if (version< emacs-version "28.0") (kbd "C-x x") nil)
  "Prefix key to activate perspective-map."
  :group 'perspective-mode
  :set (lambda (sym value)
         (when (and (bound-and-true-p persp-mode-map)
                    (bound-and-true-p perspective-map))
           (persp-mode-set-prefix-key value))
         (set-default sym value))
  :type '(choice (const :tag "None" nil)
                 key-sequence))

(defcustom persp-interactive-completion-function
  (if ido-mode 'ido-completing-read 'completing-read)
  "Function used by Perspective to interactively complete user input."
  :group 'perspective-mode
  :type 'function)

(defcustom persp-switch-wrap t
  "Whether `persp-next' and `persp-prev' should wrap."
  :group 'perspective-mode
  :type 'boolean)

(defcustom persp-sort 'name
  "What order to sort perspectives.
If 'name, then sort alphabetically.
If 'access, then sort by last time accessed (latest first).
If 'created, then sort by time created (latest first)."
  :group 'perspective-mode
  :type '(choice (const :tag "By Name"          name)
                 (const :tag "By Time Accessed" access)
                 (const :tag "By Time Created"  created)))

(defcustom persp-frame-global-perspective-name "GLOBAL"
  "The name for a frames global perspective."
  :group 'perspective-mode
  :type 'string)

(defcustom persp-frame-global-perspective-include-scratch-buffer nil
  "If non-nil include `persp-frame-global-perspective-name's scratch buffer to
buffer switch options."
  :group 'perspective-mode
  :type 'boolean)

(defcustom persp-state-default-file nil
  "When non-nil, it provides a default argument for `persp-state-save` and `persp-state-load` to work with.

`persp-state-save` overwrites this file without prompting, which
makes it easy to use in, e.g., `kill-emacs-hook` to automatically
save state when exiting Emacs."
  :group 'perspective-mode
  :type 'file)

(defcustom persp-suppress-no-prefix-key-warning nil
  "When non-nil, do not warn the user about `persp-mode-prefix-key' not being set."
  :group 'perspective-mode
  :type 'boolean)

(defcustom persp-avoid-killing-last-buffer-in-perspective t
  "Avoid killing the last buffer in a perspective.

This should not be set to nil unless there's a bug. This was
formerly a feature flag (persp-feature-flag-prevent-killing-last-buffer-in-perspective),
but it seems likely to stick around as a just-in-case for a while. It makes sense
to upgrade this from an experimental feature flag to a toggle.
TODO: Eventually eliminate this setting?"
  :group 'perspective-mode
  :type 'boolean)
(defalias 'persp-avoid-killing-last-buffer-in-perspective
  'persp-feature-flag-prevent-killing-last-buffer-in-perspective)

(defcustom persp-purge-initial-persp-on-save nil
  "When non-nil, kills all the buffers in the initial perspective upon state save.

When calling `persp-state-save`, all the buffers in the initial
perspective (\"main\" by default) are killed, expect the buffers
whose name match the regexes in
`persp-purge-initial-persp-on-save-exceptions'."
  :group 'perspective-mode
  :type 'boolean)

(defcustom persp-purge-initial-persp-on-save-exceptions nil
  "Buffer whose name match with any regexp of this list
won't be killed upon state save if persp-purge-initial-persp-on-save is t"
  :group 'perspective-mode
  :type '(repeat regexp))


;;; --- implementation

;;; XXX: Nasty kludge to deal with the byte compiler, eager macroexpansion, and
;;; frame parameters being already set when this file is being compiled during a
;;; package upgrade. This enumerates all frame-parameters starting with
;;; persp--*, saves them in persp--kludge-save-frame-params, and then blanks
;;; them out of the frame parameters. They will be restored in the matching
;;; eval-when-compile form at the bottom of this source file. See
;;; https://github.com/nex3/perspective-el/issues/93.
(eval-when-compile
  (defvar persp--kludge-save-frame-params)
  (setq persp--kludge-save-frame-params
        (cl-loop for kv in (frame-parameters nil)
                 if (string-prefix-p "persp--" (symbol-name (car kv)))
                 collect kv))
  (modify-frame-parameters
   nil
   ;; Set persp-- frame parameters to nil. The expression below creates an alist
   ;; where the keys are the relevant frame parameters and the values are nil.
   (mapcar (lambda (x) (list (car x))) persp--kludge-save-frame-params)))

(defmacro persp-let-frame-parameters (bindings &rest body)
  "Like `let', but for frame parameters.
Temporariliy set frame parameters according to BINDINGS then eval BODY.
After BODY is evaluated, frame parameters are reset to their original values."
  (declare (indent 1))
  (let ((current-frame-parameters (mapcar (lambda (binding) (cons (car binding) (frame-parameter nil (car binding)))) bindings)))
    `(unwind-protect
         (progn ,@(mapcar (lambda (binding) `(set-frame-parameter nil (quote ,(car binding)) ,(cadr binding))) bindings)
                ,@body)
       ;; Revert the frame-parameters
       (modify-frame-parameters nil (quote ,current-frame-parameters)))))

(cl-defstruct (perspective
               (:conc-name persp-)
               (:constructor make-persp-internal))
  name buffers killed local-variables
  (last-switch-time (current-time))
  (created-time (current-time))
  (window-configuration (current-window-configuration))
  (point-marker (point-marker)))

(defmacro with-current-perspective (&rest body)
  "Operate on BODY when we are in a perspective."
  (declare (indent 0))
  `(when (persp-curr)
     ,@body))

(defmacro with-perspective (name &rest body)
  "Switch to the perspective given by NAME while evaluating BODY."
  (declare (indent 1))
  (let ((old (cl-gensym)))
    `(progn
       (let ((,old (with-current-perspective (persp-current-name)))
             (last-persp-cache (persp-last))
             (result))
         (unwind-protect
             (progn
               (persp-switch ,name 'norecord)
               (setq result (progn ,@body)))
           (when ,old (persp-switch ,old 'norecord)))
         (set-frame-parameter nil 'persp--last last-persp-cache)
         result))))

(defun persp--make-ignore-buffer-rx ()
  (defvar ido-ignore-buffers)
  (if ido-ignore-buffers
      ;; convert a list of regexps to one
      (rx-to-string (append (list 'or)
                            (mapcar (lambda (rx) `(regexp ,rx))
                                    ido-ignore-buffers)))
    ;; return a regex which matches nothing, and therefore should ignore nothing
    "$^"))

;; NOTE: This macro is used as a place for setf expressions so be careful with
;; how you modify it as you may break things in surprising ways.
(defmacro persp-current-buffers ()
  "Return a list of all buffers in the current perspective."
  `(persp-buffers (persp-curr)))

(defun persp-current-buffers* (&optional include-global)
  "Same as `persp-current-buffers' but if INCLUDE-GLOBAL include buffers from
the frame global perspective."
  (if (not include-global)
      (persp-current-buffers)
    (delete-dups
     (append (persp-current-buffers)
             (when (member persp-frame-global-perspective-name (persp-names))
               (with-perspective persp-frame-global-perspective-name
                 (if persp-frame-global-perspective-include-scratch-buffer
                     (persp-current-buffers)
                   (remove (persp-get-scratch-buffer) (persp-current-buffers)))))))))

(defun persp-current-buffer-names (&optional include-global)
  "Return a list of names of all living buffers in the current perspective.
Include the names of the buffers in the frame global perspective when
INCLUDE-GLOBAL."
  (let ((ignore-rx (persp--make-ignore-buffer-rx)))
    (cl-loop for buf in (persp-current-buffers* include-global)
             if (and (buffer-live-p buf)
                     (not (string-match-p ignore-rx (buffer-name buf))))
             collect (buffer-name buf))))

(defun persp-is-current-buffer (buf &optional include-global)
  "Return T if BUF is in the current perspective. When INCLUDE-GLOBAL, also
return T if BUF is in the frame global perspective."
  (memq buf (persp-current-buffers* include-global)))

(defun persp-buffer-filter (buf &optional include-global)
  "Return F if BUF is in the current perspective. When INCLUDE-GLOBAL, also
return F if BUF is in the frame global perspective. Used for filtering in buffer
display modes like ibuffer."
  (not (persp-is-current-buffer buf include-global)))

(defun persp-buffer-list-filter (bufs &optional include-global)
  "Return the subset of BUFS which is in the current perspective. When
EXCLUDE-GLOBAL include buffers that are members of the frame global perspective."
  (cl-loop for buf in bufs
           if (persp-is-current-buffer (get-buffer buf) include-global)
           collect buf))

(defun persp-valid-name-p (name)
  "Return T if NAME is a valid perspective name."
  (and (not (null name))
       (not (string= "" name))))

(defun persp-current-name ()
  "Get the name of the current perspective."
  (persp-name (persp-curr)))

(defun persp-scratch-buffer (&optional name)
  (let* ((current-name  (persp-current-name))
         (name          (or name current-name))
         (initial-persp (equal name persp-initial-frame-name)))
    (concat "*scratch*"
            (unless initial-persp
              (format " (%s)" name)))))

(defun persp-get-scratch-buffer (&optional name)
  "Return the \"*scratch* (NAME)\" buffer.
Create it if the current perspective doesn't have one yet."
  (let* ((scratch-buffer-name (persp-scratch-buffer name))
         (scratch-buffer (get-buffer scratch-buffer-name)))
    ;; Do not interfere with an existing scratch buffer's status.
    (unless scratch-buffer
      (setq scratch-buffer (get-buffer-create scratch-buffer-name))
      (with-current-buffer scratch-buffer
        (when (eq major-mode 'fundamental-mode)
          (funcall initial-major-mode))
        (when (and (zerop (buffer-size))
                   initial-scratch-message)
          (insert (substitute-command-keys initial-scratch-message))
          (set-buffer-modified-p nil))))
    scratch-buffer))

(defun persp-switch-to-scratch-buffer ()
  "Switch to the current perspective's scratch buffer.
Create the scratch buffer if there isn't one yet."
  (interactive)
  (switch-to-buffer (persp-get-scratch-buffer)))

(defalias 'persp-killed-p 'persp-killed
  "Return whether the perspective CL-X has been killed.")

(defvar persp-started-after-server-mode nil
  "XXX: A nasty workaround for a strange timing bug which occurs
  if the Emacs server was started before Perspective initialized.
  For some reason, persp-delete-frame gets called multiple times
  in unexpected ways. To reproduce: (0) make sure server-start is
  called before persp-mode is turned on and comment out the use
  of persp-started-after-server-mode, (1) get a session going
  with a main frame, (2) switch perspectives a couple of
  times, (3) use emacsclient -c to edit a file in a new
  frame, (4) C-x 5 0 to kill that frame. This will cause an
  unintended perspective switch in the primary frame, and mark
  the previous perspective as deleted. There is also a note in
  the *Messages* buffer. TODO: It would be good to get to the
  bottom of this problem, rather than just paper over it.")

(defvar persp-before-switch-hook nil
  "A hook that's run before `persp-switch'.
Run with the previous perspective as `persp-curr'.")

(defvar persp-switch-hook nil
  "A hook that's run after `persp-switch'.
Run with the newly created perspective as `persp-curr'.")

(defvar persp-mode-hook nil
  "A hook that's run after `persp-mode' has been activated.")

(defvar persp-created-hook nil
  "A hook that's run after a perspective has been created.
Run with the newly created perspective as `persp-curr'.")

(defvar persp-killed-hook nil
  "A hook that's run just before a perspective is destroyed.
Run with the perspective to be destroyed as `persp-curr'.")

(defvar persp-activated-hook nil
  "A hook that's run after a perspective has been activated.
Run with the activated perspective active.")

(defvar persp-before-rename-hook nil
  "A hook run immediately before renaming a perspective.")

(defvar persp-after-rename-hook nil
  "A hook run immediately after renaming a perspective.")

(defvar persp-state-before-save-hook nil
  "A hook run immediately before saving persp state to disk.")

(defvar persp-state-after-save-hook nil
  "A hook run immediately after saving persp state to disk.")

(defvar persp-state-before-load-hook nil
  "A hook run immediately before loading persp state from disk.")

(defvar persp-state-after-load-hook nil
  "A hook run immediately after loading persp state from disk.")

(defvar persp-mode-map (make-sparse-keymap)
  "Keymap for perspective-mode.")

(defvar perspective-map nil
  "Sub-keymap for perspective-mode")

(define-prefix-command 'perspective-map)
(when persp-mode-prefix-key
  (define-key persp-mode-map persp-mode-prefix-key 'perspective-map))

(define-key perspective-map (kbd "s") 'persp-switch)
(define-key perspective-map (kbd "k") 'persp-remove-buffer)
(define-key perspective-map (kbd "c") 'persp-kill)
(define-key perspective-map (kbd "r") 'persp-rename)
(define-key perspective-map (kbd "a") 'persp-add-buffer)
(define-key perspective-map (kbd "A") 'persp-set-buffer)
(define-key perspective-map (kbd "b") 'persp-switch-to-buffer)
(define-key perspective-map (kbd "B") 'persp-switch-to-scratch-buffer)
(define-key perspective-map (kbd "i") 'persp-import)
(define-key perspective-map (kbd "n") 'persp-next)
(define-key perspective-map (kbd "<right>") 'persp-next)
(define-key perspective-map (kbd "p") 'persp-prev)
(define-key perspective-map (kbd "<left>") 'persp-prev)
(define-key perspective-map (kbd "m") 'persp-merge)
(define-key perspective-map (kbd "u") 'persp-unmerge)
(define-key perspective-map (kbd "g") 'persp-add-buffer-to-frame-global)
(define-key perspective-map (kbd "C-s") 'persp-state-save)
(define-key perspective-map (kbd "C-l") 'persp-state-load)
(define-key perspective-map (kbd "`") 'persp-switch-by-number)

(define-key perspective-map (kbd "1") (lambda () (interactive) (persp-switch-by-number 1)))
(define-key perspective-map (kbd "2") (lambda () (interactive) (persp-switch-by-number 2)))
(define-key perspective-map (kbd "3") (lambda () (interactive) (persp-switch-by-number 3)))
(define-key perspective-map (kbd "4") (lambda () (interactive) (persp-switch-by-number 4)))
(define-key perspective-map (kbd "5") (lambda () (interactive) (persp-switch-by-number 5)))
(define-key perspective-map (kbd "6") (lambda () (interactive) (persp-switch-by-number 6)))
(define-key perspective-map (kbd "7") (lambda () (interactive) (persp-switch-by-number 7)))
(define-key perspective-map (kbd "8") (lambda () (interactive) (persp-switch-by-number 8)))
(define-key perspective-map (kbd "9") (lambda () (interactive) (persp-switch-by-number 9)))
(define-key perspective-map (kbd "0") (lambda () (interactive) (persp-switch-by-number 10)))

(with-eval-after-load 'which-key
  (declare-function which-key-add-keymap-based-replacements "which-key.el")
  (when (fboundp 'which-key-add-keymap-based-replacements)
    (which-key-add-keymap-based-replacements perspective-map
      "1" "switch to 1"
      "2" "switch to 2"
      "3" "switch to 3"
      "4" "switch to 4"
      "5" "switch to 5"
      "6" "switch to 6"
      "7" "switch to 7"
      "8" "switch to 8"
      "9" "switch to 9"
      "0" "switch to 10")))

(defun perspectives-hash (&optional frame)
  "Return a hash containing all perspectives in FRAME.
FRAME defaults to the currently selected frame. The keys are the
perspectives' names. The values are persp structs, with the
fields NAME, WINDOW-CONFIGURATION, BUFFERS, KILLED, POINT-MARKER,
and LOCAL-VARIABLES.

NAME is the name of the perspective.

WINDOW-CONFIGURATION is the configuration given by
`current-window-configuration' last time the perspective was
saved (if this isn't the current perspective, this is when the
perspective was last active).

BUFFERS is a list of buffer objects that are associated with this
perspective.

KILLED is non-nil if the perspective has been killed.

POINT-MARKER is the point position in the active buffer.
Otherwise, when multiple windows are visiting the same buffer,
all but one of their points will be overwritten.

LOCAL-VARIABLES is an alist from variable names to their
perspective-local values."
  ;; XXX: This must return a non-nil value to avoid breaking frames initialized
  ;; with after-make-frame-functions bound to nil.
  (or (frame-parameter frame 'persp--hash)
      (make-hash-table)))

(defun persp-mode-guard ()
  (unless (bound-and-true-p persp-mode)
    (persp-error "persp-mode is not active")))

(defun persp-curr (&optional frame)
  "Get the current perspective in FRAME.
FRAME defaults to the currently selected frame."
  ;; XXX: This must return a non-nil value to avoid breaking frames initialized
  ;; with after-make-frame-functions bound to nil.
  (persp-mode-guard)
  (or (frame-parameter frame 'persp--curr)
      (make-persp-internal)))

(defun persp-last (&optional frame)
  "Get the last active perspective in FRAME.
FRAME defaults to the currently selected frame."
  ;; XXX: Unlike persp-curr, it is unsafe to return a default value of
  ;; (make-persp-internal) here, since some code assumes (persp-last) can return
  ;; nil.
  (frame-parameter frame 'persp--last))

(defun persp-mode-set-prefix-key (newkey)
  "Set NEWKEY as the prefix key to activate persp-mode."
  (substitute-key-definition 'perspective-map nil persp-mode-map)
  (when newkey
    (define-key persp-mode-map newkey 'perspective-map)))

(defvar persp-protected nil
  "Whether a perspective error should cause persp-mode to be disabled.
Dynamically bound by `persp-protect'.")

(defface persp-selected-face
  '((t (:weight bold :foreground "Blue")))
  "The face used to highlight the current perspective on the modeline.")

(defmacro persp-protect (&rest body)
  "Wrap BODY to disable persp-mode when it errors out.
This prevents the persp-mode from completely breaking Emacs."
  (declare (indent 0))
  (let ((persp-protected t))
    `(condition-case err
         (progn ,@body)
       (persp-error
        (message "Fatal persp-mode error: %S" err)
        (persp-mode -1)))))

(defun persp-error (&rest args)
  "Like `error', but mark it as a persp-specific error.
Used along with `persp-protect' to ensure that persp-mode doesn't
bring down Emacs.

ARGS will be interpreted by `format-message'."
  (if persp-protected
      (signal 'persp-error (list (apply 'format args)))
    (apply 'error args)))

(defun check-persp (persp)
  "Raise an error if PERSP has been killed."
  (cond
   ((not persp)
    (persp-error "Expected perspective, was nil"))
   ((persp-killed-p persp)
    (persp-error "Using killed perspective `%s'" (persp-name persp)))))

(defmacro make-persp (&rest args)
  "Create a new perspective struct and put it in `perspectives-hash'.

ARGS is a list of keyword arguments followed by an optional BODY.
The keyword arguments set the fields of the perspective struct.
If BODY is given, it is executed to set the window configuration
for the perspective.

Save point, and current buffer before executing BODY, and then
restore them after.  If the current buffer is changed in BODY,
that change is lost when getting out, hence the current buffer
will need to be changed again after calling `make-persp'."
  (declare (indent defun))
  (let ((keywords))
    (while (keywordp (car args))
      (dotimes (_ 2) (push (pop args) keywords)))
    (setq keywords (reverse keywords))
    `(let ((persp (make-persp-internal ,@keywords)))
       (with-current-perspective
         (setf (persp-local-variables persp) (persp-local-variables (persp-curr))))
       (puthash (persp-name persp) persp (perspectives-hash))
       (with-perspective (persp-name persp)
         ,(when args
            ;; Body form given
            `(save-excursion ,@args))
         ;; If the `current-buffer' changes while in `save-excursion',
         ;; that change isn't kept when getting out, since the current
         ;; buffer is saved before executing BODY and restored after.
         (run-hooks 'persp-created-hook))
       persp)))

(defun persp-save ()
  "Save the current perspective state.
Specifically, save the current window configuration and
perspective-local variables to `persp-curr'"
  (with-current-perspective
    (setf (persp-local-variables (persp-curr))
          (mapcar
           (lambda (c)
             (let ((name (car c)))
               (list name (symbol-value name))))
           (persp-local-variables (persp-curr))))
    (setf (persp-window-configuration (persp-curr)) (current-window-configuration))
    (setf (persp-point-marker (persp-curr)) (point-marker))))

(defun persp-names ()
  "Return a list of the names of all perspectives on the `selected-frame'.

If `persp-sort' is 'name (the default), then return them sorted
alphabetically. If `persp-sort' is 'access, then return them
sorted by the last time the perspective was switched to, the
current perspective being the first. If `persp-sort' is 'created,
then return them in the order they were created, with the newest
first."
  (let ((persps (hash-table-values (perspectives-hash))))
    (cond ((eq persp-sort 'name)
           (sort (mapcar 'persp-name persps) 'string<))
          ((eq persp-sort 'access)
           (mapcar 'persp-name
                   (sort persps (lambda (a b)
                                  (time-less-p (persp-last-switch-time b)
                                               (persp-last-switch-time a))))))
          ((eq persp-sort 'created)
           (mapcar 'persp-name
                   (sort persps (lambda (a b)
                                  (time-less-p (persp-created-time b)
                                               (persp-created-time a)))))))))

(defun persp-all-names (&optional not-frame)
  "Return a list of the perspective names for all frames.
Excludes NOT-FRAME, if given."
  (cl-reduce 'cl-union
             (mapcar
              (lambda (frame)
                (unless (equal frame not-frame)
                  (with-selected-frame frame (persp-names))))
              (frame-list))))

(defun persp-prompt (&optional default require-match)
  "Prompt for the name of a perspective.

DEFAULT is a default value for the prompt.

REQUIRE-MATCH can take the same values as in `completing-read'."
  (funcall persp-interactive-completion-function
           (concat "Perspective name"
                   (if default (concat " (default " default ")") "")
                   ": ")
           (persp-names)
           nil require-match nil nil default))

(defun persp-reset-windows ()
  "Remove all windows, ensure the remaining one has no window parameters.
This prevents the propagation of reserved window parameters like
window-side creating perspectives."
  (let ((ignore-window-parameters t)
        ;; Required up to Emacs 27.2 to prevent `delete-window' from
        ;; updating `window-prev-buffers' for all windows.  Allowing
        ;; to create a fresh window (aka `split-window'), with empty
        ;; `window-prev-buffers'.  If the latter is not empty, other
        ;; perspectives may pull in buffers of the current one, as a
        ;; side effect when `persp-reactivate-buffers' is called and
        ;; the perspective is then switched.
        (switch-to-buffer-preserve-window-point nil))
    (delete-other-windows
     ;; XXX: Ugly workaround for problems related to
     ;; https://github.com/nex3/perspective-el/issues/163 and
     ;; https://github.com/nex3/perspective-el/issues/167
     (when (eq (minibuffer-window) (selected-window))
       (previous-window (minibuffer-window))))
    (when (ignore-errors
            ;; Create a fresh window without any window parameters, the
            ;; selected window is still in a window that may have window
            ;; parameters we don't want.
            (split-window))
      ;; Delete the selected window so that the only window left has no window
      ;; parameters.
      (delete-window))))

(defun persp-new (name)
  "Return a perspective named NAME, or create a new one if missing.
The new perspective will start with only an `initial-major-mode'
buffer called \"*scratch* (NAME)\"."
  (or (gethash name (perspectives-hash))
      (make-persp :name name
        (switch-to-buffer (persp-get-scratch-buffer name))
        (persp-reset-windows))))

(defun persp-reactivate-buffers (buffers)
  "Raise BUFFERS to the top of the most-recently-selected list.
Returns BUFFERS with all non-living buffers removed.

See also `other-buffer'."
  (cl-loop for buf in (reverse buffers)
           when (buffer-live-p buf)
           collect buf into living-buffers
           and do (switch-to-buffer buf)
           finally return (nreverse living-buffers)))

(defun persp-set-local-variables (vars)
  "Set the local variables given in VARS.
VARS should be an alist of variable names to values."
  (dolist (var vars) (apply 'set var)))

(defun persp-intersperse (list interspersed-val)
  "Intersperse a value into a list.
Return a new list made from taking LIST and inserting
INTERSPERSED-VAL between every pair of items.

For example, (persp-intersperse '(1 2 3) 'a) gives '(1 a 2 a 3)."
  (reverse
   (cl-reduce
    (lambda (list el) (if list (cl-list* el interspersed-val list) (list el)))
    list :initial-value nil)))

(defconst persp-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'persp-mode-line-click)
    map))

(defconst persp-header-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line down-mouse-1] 'persp-mode-line-click)
    map))

(defun persp-mode-line-click (event)
  "Select the clicked perspective.
EVENT is the click event triggering this function call."
  (interactive "e")
  (persp-switch (format "%s" (car (posn-string (event-start event)))))
  ;; XXX: Force update of modestring because otherwise it's inconsistent with
  ;; the order of perspectives maintained by persp-sort. The call to
  ;; persp-update-modestring inside persp-switch happens too early.
  (persp-update-modestring))

(defun persp-mode-line ()
  "Return the string displayed in the modeline representing the perspectives."
  (frame-parameter nil 'persp--modestring))

(defun persp-update-modestring ()
  "Update the string to reflect the current perspectives.
Has no effect when `persp-show-modestring' is nil."
  (when persp-show-modestring
    (let ((open (list (nth 0 persp-modestring-dividers)))
          (close (list (nth 1 persp-modestring-dividers)))
          (sep (nth 2 persp-modestring-dividers)))
      (set-frame-parameter nil 'persp--modestring
           (append open
                   (if persp-modestring-short
                       (list (persp-current-name))
                     (persp-intersperse (mapcar 'persp-format-name
                                                (persp-names)) sep))
                   close)))))

(defun persp-format-name (name)
  "Format the perspective name given by NAME for display in the mode line or header line."
  (let ((string-name (format "%s" name)))
    (if (equal name (persp-current-name))
        (propertize string-name 'face 'persp-selected-face)
      (cond ((eq persp-show-modestring 'header)
             (propertize string-name
                         'local-map persp-header-line-map
                         'mouse-face 'header-line-highlight))
            ((eq persp-show-modestring t)
             (propertize string-name
                         'local-map persp-mode-line-map
                         'mouse-face 'mode-line-highlight))))))

(defun persp-get-quick (char &optional prev)
  "Return the name of the first perspective that begins with CHAR.
Perspectives are sorted alphabetically.

PREV can be the name of a perspective.  If it's passed,
this will try to return the perspective alphabetically after PREV.
This is used for cycling between perspectives."
  (persp-get-quick-helper char prev (persp-names)))

(defun persp-get-quick-helper (char prev names)
  "Helper for `persp-get-quick' using CHAR, PREV, and NAMES."
  (if (null names) nil
    (let ((name (car names)))
      (cond
       ((and (null prev) (eq (string-to-char name) char)) name)
       ((equal name prev)
        (if (and (not (null (cdr names))) (eq (string-to-char (cadr names)) char))
            (cadr names)
          (persp-get-quick char)))
       (t (persp-get-quick-helper char prev (cdr names)))))))

(defun persp-switch-last ()
  "Switch to the perspective accessed before the current one."
  (interactive)
  (unless (persp-last)
    (persp-error "There is no last perspective"))
  (persp-switch (persp-name (persp-last))))

(defun persp-switch (name &optional norecord)
  "Switch to the perspective given by NAME.
If it doesn't exist, create a new perspective and switch to that.

Switching to a perspective means that all buffers associated with
that perspective are reactivated (see `persp-reactivate-buffers'),
the perspective's window configuration is restored, and the
perspective's local variables are set.

If NORECORD is non-nil, do not update the
`persp-last-switch-time' for the switched perspective."
  (interactive "i")
  (unless (persp-valid-name-p name)
    (setq name (persp-prompt (and (persp-last) (persp-name (persp-last))))))
  (if (and (persp-curr) (equal name (persp-current-name))) name
    (let ((persp (persp-new name)))
      (set-frame-parameter nil 'persp--last (persp-curr))
      (unless norecord
        (run-hooks 'persp-before-switch-hook))
      (persp-activate persp)
      (when (fboundp 'persp--set-xref-marker-ring) (persp--set-xref-marker-ring))
      (unless norecord
        (setf (persp-last-switch-time persp) (current-time))
        (run-hooks 'persp-switch-hook))
      name)))

(defun persp-switch-by-number (num)
  "Switch to the perspective given by NUMBER."
  (interactive "NSwitch to perspective number: ")
  (let* ((persps (persp-names))
         (max-persps (length persps)))
    (if (<= num max-persps)
        (persp-switch (nth (- num 1) persps))
      (message "Perspective number %s not available, only %s exist%s"
               num
               max-persps
               (if (= 1 max-persps) "s" ""))))
  ;; XXX: Have to force the modestring to update in this case, since the call
  ;; inside persp-switch happens too early. Otherwise, it may be inconsistent
  ;; with persp-sort.
  (persp-update-modestring))

(defun persp-activate (persp)
  "Activate the perspective given by the persp struct PERSP."
  (check-persp persp)
  (persp-save)
  (set-frame-parameter nil 'persp--curr persp)
  (persp-reset-windows)
  (persp-set-local-variables (persp-local-variables persp))
  (setf (persp-buffers persp) (persp-reactivate-buffers (persp-buffers persp)))
  (set-window-configuration (persp-window-configuration persp))
  (when (marker-position (persp-point-marker persp))
    (goto-char (persp-point-marker persp)))
  (persp-update-modestring)
  ;; force update of `current-buffer'
  (set-buffer (window-buffer))
  (run-hooks 'persp-activated-hook))

(defun persp-switch-quick (char)
  "Switch to the first perspective, alphabetically, that begins with CHAR.

Sets `this-command' (and thus `last-command') to (persp-switch-quick . CHAR).

See `persp-switch', `persp-get-quick'."
  (interactive "c")
  (let ((persp (if (and (consp last-command) (eq (car last-command) this-command))
                   (persp-get-quick char (cdr last-command))
                 (persp-get-quick char))))
    (setq this-command (cons this-command persp))
    (if persp (persp-switch persp)
      (persp-error (concat "No perspective name begins with " (string char))))))

(defun persp-next ()
  "Switch to next perspective (to the right)."
  (interactive)
  (let* ((names (persp-names))
         (pos (cl-position (persp-current-name) names)))
    (cond
     ((null pos) (persp-find-some))
     ((= pos (1- (length names)))
      (if persp-switch-wrap (persp-switch (nth 0 names))))
     (t (persp-switch (nth (1+ pos) names))))))

(defun persp-prev ()
  "Switch to previous perspective (to the left)."
  (interactive)
  (let* ((names (persp-names))
         (pos (cl-position (persp-current-name) names)))
    (cond
     ((null pos) (persp-find-some))
     ((= pos 0)
      (if persp-switch-wrap (persp-switch (nth (1- (length names)) names))))
     (t (persp-switch (nth (1- pos) names))))))

(defun persp-find-some ()
  "Return the name of a valid perspective.

This function tries to return the \"most appropriate\"
perspective to switch to.  It tries:

  * The perspective given by `persp-last'.
  * The \"first\" perspective, based on the ordering of persp-names.
  * The main perspective.
  * The first existing perspective, alphabetically.

If none of these perspectives can be found, this function will
create a new main perspective and return \"main\"."
  (cond
   ((persp-last) (persp-name (persp-last)))
   ((> (length (persp-names)) 1) (car (persp-names)))
   ((gethash persp-initial-frame-name (perspectives-hash)) persp-initial-frame-name)
   ;; TODO: redundant?
   ((> (hash-table-count (perspectives-hash)) 0) (car (persp-names)))
   (t (persp-activate
       (make-persp :name persp-initial-frame-name :buffers (buffer-list)
         :window-configuration (current-window-configuration)
         :point-marker (point-marker)))
      persp-initial-frame-name)))

(defun persp-add-buffer (buffer-or-name)
  "Associate BUFFER-OR-NAME with the current perspective.

See also `persp-switch' and `persp-remove-buffer'."
  (interactive
   (list
    (let ((read-buffer-function nil))
      (read-buffer "Add buffer to perspective: "))))
  (let ((buffer (get-buffer buffer-or-name)))
    (if (not (buffer-live-p buffer))
        (message "buffer %s doesn't exist" buffer-or-name)
      (unless (persp-is-current-buffer buffer)
        (push buffer (persp-current-buffers))))))

(defun persp-add-buffer-to-frame-global (buffer-or-name)
  "Associate BUFFER-OR-NAME with the frame global perspective.

See also `persp-add-buffer'."
  (interactive
   (list
    (let ((read-buffer-function nil))
      (read-buffer "Add buffer to frame global perspective: "))))
  (with-perspective persp-frame-global-perspective-name
    (persp-add-buffer buffer-or-name)))

(defun persp-set-buffer (buffer-or-name)
  "Associate BUFFER-OR-NAME with the current perspective and remove it from any other."
  (interactive
   (list
    (let ((read-buffer-function nil))
      (read-buffer "Set buffer to perspective: "))))
  (let ((buffer (get-buffer buffer-or-name)))
    (if (not (buffer-live-p buffer))
        (message "buffer %s doesn't exist" buffer-or-name)
      (persp-add-buffer buffer)
      ;; Do not use the combination "while `persp-buffer-in-other-p'",
      ;; if the buffer is not removed from other perspectives, it will
      ;; go into an infinite loop.
      (cl-loop for other-persp in (remove (persp-current-name) (persp-all-names))
               do (with-perspective other-persp
                    (persp-forget-buffer buffer))))))

(defun persp-set-frame-global-perspective (buffer-or-name)
  "Associate BUFFER-OR-NAME with the frame global perspective and remove it from
any other.

See also `persp-set-buffer'."
  (list
   (let ((read-buffer-function nil))
     (read-buffer "Set buffer to frame global perspective: ")))
  (with-perspective persp-frame-global-perspective-name
    (persp-set-buffer buffer-or-name)))

(cl-defun persp-buffer-in-other-p (buffer)
  "Returns nil if BUFFER is only in the current perspective.
Otherwise, returns (FRAME . NAME), the frame and name of another
perspective that has the buffer.

Prefers perspectives in the selected frame."
  (cl-loop for frame in (sort (frame-list) (lambda (_frame1 frame2) (eq frame2 (selected-frame))))
           do (cl-loop for persp being the hash-values of (perspectives-hash frame)
                       if (and (not (and (equal frame (selected-frame))
                                         (equal (persp-name persp) (persp-name (persp-curr frame)))))
                               (memq buffer (persp-buffers persp)))
                       do (cl-return-from persp-buffer-in-other-p
                            (cons frame (persp-name persp)))))
  nil)

(defun persp-switch-to-buffer (buffer-or-name)
  "Like `switch-to-buffer', but switches to another perspective if necessary."
  (interactive
   (list
    (let ((read-buffer-function nil))
      (read-buffer-to-switch "Switch to buffer: "))))
  (let ((buffer (window-normalize-buffer-to-switch-to buffer-or-name)))
    (if (persp-is-current-buffer buffer)
        (switch-to-buffer buffer)
      (let ((other-persp (persp-buffer-in-other-p buffer)))
        (when (eq (car-safe other-persp) (selected-frame))
          (persp-switch (cdr other-persp)))
        (switch-to-buffer buffer)))))

(cl-defun persp-maybe-kill-buffer ()
  "Don't kill a buffer if it's the only buffer in a perspective.

This is the default behaviour of `kill-buffer'.  Perspectives
with only one buffer should keep it alive to prevent adding a
buffer from another perspective, replacing the killed buffer.

Will also cleanup killed buffers form each perspective's list
of buffers containing the buffer to be killed.

This is a hook for `kill-buffer-query-functions'.  Don't call
this directly, otherwise the current buffer may be removed or
killed from perspectives.

See also `persp-remove-buffer'."
  ;; List candidates where the buffer to be killed should be removed
  ;; instead, whom are perspectives with more than one buffer.  This
  ;; is to allow the buffer to live for perspectives that have it as
  ;; their only buffer.
  (persp-protect
    (let* ((buffer (current-buffer))
           (bufstr (buffer-name buffer))
           candidates-for-removal candidates-for-keeping)
      ;; XXX: For performance reasons, always allow killing off obviously
      ;; temporary buffers. According to Emacs convention, these buffers' names
      ;; start with a space.
      (when (string-match-p (rx string-start (one-or-more blank)) bufstr)
        (cl-return-from persp-maybe-kill-buffer t))
      (dolist (name (persp-names))
        (let ((buffer-names (persp-get-buffer-names name)))
          (when (member bufstr buffer-names)
            (if (cdr buffer-names)
                (push name candidates-for-removal)
              ;; We use a list for debugging purposes, a simple bool
              ;; can suffice for what we are doing here.
              (push name candidates-for-keeping)))))
      (cond
       ;; When there aren't perspectives with the buffer as the only
       ;; buffer, it can be killed safely.  Also cleanup killed ones
       ;; found in perspectives listing the buffer to be killed.
       ((not candidates-for-keeping)
        ;; Switching to a perspective that isn't the current, should
        ;; automatically cleanup previously killed buffers which are
        ;; still in the perspective's list of buffers.  Removing the
        ;; buffer to be killed should also keep the list clean.
        (dolist (name candidates-for-removal)
          (with-perspective name
            ;; remove the buffer that has to be killed from the list
            (setf (persp-current-buffers) (remq buffer (persp-current-buffers)))))
        t)
       ;; When a perspective have the buffer as the only buffer, the
       ;; buffer should not be killed, but removed from perspectives
       ;; that have more than one buffer.  Those perspectives should
       ;; forget about the buffer.
       (candidates-for-removal
        (dolist (name candidates-for-removal)
          (with-perspective name
            (persp-forget-buffer buffer)))
        nil)))))

(defun persp-forget-buffer (buffer)
  "Disassociate BUFFER with the current perspective.
If BUFFER isn't in any perspective, then it is in limbo.

See also `persp-add-buffer' and `persp-remove-buffer'."
  (interactive
   (list (funcall persp-interactive-completion-function "Disassociate buffer with perspective: " (persp-current-buffer-names))))
  (setq buffer (when buffer (get-buffer buffer)))
  (cond ((not (buffer-live-p buffer)))
        ;; Do not disassociate a perspective's last left buffer or one
        ;; that's not part of the current perspective.
        ((or (not (persp-is-current-buffer buffer))
             (and (memq 'persp-maybe-kill-buffer kill-buffer-query-functions)
                  (not (remove (buffer-name buffer) (persp-current-buffer-names)))))
         (setq buffer nil))
        ;; Make the buffer go away if we can see it.
        ((let (buffer-in-any-window)
           (walk-windows (lambda (window)
                           (when (eq buffer (window-buffer window))
                             (setq buffer-in-any-window t)
                             ;; Burying the current buffer should also
                             ;; act as an `unrecord-window-buffer'.
                             (with-selected-window window (bury-buffer)))))
           (let ((window (get-buffer-window buffer)))
             (when window
               (error "Buried buffer %s found in window %s, but it shouldn't"
                      buffer window)))
           ;; `with-selected-window' restores the `current-buffer'.
           ;; If the current buffer is buried, it should not be the
           ;; next current buffer.  Remember to fix it later.
           buffer-in-any-window))
        (t (bury-buffer buffer)))
  ;; If the `current-buffer' was buried in `with-selected-window', set
  ;; the real current buffer, since `with-selected-window' restored it
  ;; as the next current buffer after processing its body.
  (set-buffer (window-buffer))
  (setf (persp-current-buffers) (remq buffer (persp-current-buffers))))

(defun persp-forget-frame-global-buffer (buffer)
  "Disassociate BUFFER from the frame global perspective.
If BUFFER isn't in any perspective, then it is in limbo.

See also `persp-forget-buffer'."
  (interactive
   (list (funcall persp-interactive-completion-function "Disassociate buffer from frame global perspective: "
                  (with-perspective persp-frame-global-perspective-name
                    (persp-current-buffer-names)))))
  (with-perspective persp-frame-global-perspective-name
    (persp-forget-buffer buffer)))

(defun persp-remove-buffer (buffer)
  "Remove BUFFER from the current perspective.
Kill BUFFER if it falls into limbo (not in any perspective).

To disassociate BUFFER without the chance of killing it, see
`persp-forget-buffer'.

See also `persp-switch' and `persp-add-buffer'."
  (interactive
   (list (funcall persp-interactive-completion-function "Remove buffer from perspective: " (persp-current-buffer-names))))
  (setq buffer (when buffer (get-buffer buffer)))
  (cond ((not (buffer-live-p buffer)))
        ;; Do not kill or remove a buffer if the perspective will then
        ;; switch to the buffer of another perspective.  It may happen
        ;; when the buffer is the perspective's last left buffer or if
        ;; the next candidate is a perspective's special buffer.  This
        ;; could not be enforced when a perspective is killed.
        ((and (persp-is-current-buffer buffer)
              (memq 'persp-maybe-kill-buffer kill-buffer-query-functions)
              (not (remove (buffer-name buffer) (persp-current-buffer-names)))))
        ;; Only kill the buffer if no other perspectives are using it.
        ((not (persp-buffer-in-other-p buffer))
         (kill-buffer buffer))
        ;; Make the buffer go away if we can see it.
        ((persp-forget-buffer buffer))))

(defun persp-remove-frame-global-buffer (buffer)
  "Remove BUFFER from the frame global perspective.

See also `persp-remove-buffer'."
  (interactive
   (list (funcall persp-interactive-completion-function "Remove buffer from frame global perspective: "
                  (with-perspective persp-frame-global-perspective-name
                    (persp-current-buffer-names)))))
  (with-perspective persp-frame-global-perspective-name
    (persp-remove-buffer buffer)))

(defun persp-kill (name)
  "Kill the perspective given by NAME.

Killing a perspective means that all buffers associated with that
perspective and no others are killed."
  (interactive "i")
  (if (null name) (setq name (persp-prompt (persp-current-name) t)))
  (remove-hook 'kill-buffer-query-functions 'persp-maybe-kill-buffer)
  (with-perspective name
    (run-hooks 'persp-killed-hook)
    (mapc 'persp-remove-buffer (persp-current-buffers))
    (setf (persp-killed (persp-curr)) t))
  (when persp-avoid-killing-last-buffer-in-perspective
    (add-hook 'kill-buffer-query-functions 'persp-maybe-kill-buffer))
  (remhash name (perspectives-hash))
  (when (boundp 'persp--xref-marker-ring) (remhash name persp--xref-marker-ring))
  (persp-update-modestring)
  (when (and (persp-last) (equal name (persp-name (persp-last))))
    (set-frame-parameter
     nil 'persp--last
     (let* ((persp-sort 'access)
            (names (persp-names))
            (last (nth 1 names)))
       (when last
         (gethash last (perspectives-hash))))))
  (when (or (not (persp-curr)) (equal name (persp-current-name)))
    ;; Don't let persp-last get set to the deleted persp.
    (persp-let-frame-parameters ((persp--last (persp-last)))
      (persp-switch (persp-find-some)))))

(defun persp-kill-others ()
  "Kill all perspectives except the current one."
  (interactive)
  (let ((self (persp-current-name)))
    (when (yes-or-no-p (concat "Really kill all perspectives other than `" self "'? "))
      (cl-loop for p in (persp-names)
               when (not (string-equal p self)) do
               (persp-kill p)))))

(defun persp-rename (name)
  "Rename the current perspective to NAME."
  (interactive "sNew name: ")
  (unless (persp-valid-name-p name)
    (persp-error "Invalid perspective name"))
  (if (gethash name (perspectives-hash))
      (persp-error "Perspective `%s' already exists" name)
    ;; before hook
    (run-hooks 'persp-before-rename-hook)
    ;; rename the perspective-specific *scratch* buffer
    (let* ((old-scratch-name (persp-scratch-buffer))
           (new-scratch-name (persp-scratch-buffer name))
           (scratch-buffer (get-buffer old-scratch-name)))
      (when scratch-buffer
        (if (get-buffer new-scratch-name)
            ;; https://github.com/nex3/perspective-el/issues/128
            ;; Buffer already exists, probably on another frame. Pull it into
            ;; the current perspective; they'll be shared.
            (persp-add-buffer new-scratch-name)
          ;; Buffer with new-scratch-name does not exist, so just rename it.
          (with-current-buffer scratch-buffer
            (rename-buffer new-scratch-name)))))
    ;; rewire the rest of the perspective inside its data structures
    (remhash (persp-current-name) (perspectives-hash))
    (puthash name (persp-curr) (perspectives-hash))
    (setf (persp-name (persp-curr)) name)
    (persp-update-modestring)
    ;; after hook
    (run-hooks 'persp-after-rename-hook)))

(cl-defun persp-all-get (name not-frame)
  "Returns the list of buffers for a perspective named NAME from any
frame other than NOT-FRAME.

This doesn't return the window configuration because those can't be
copied across frames."
  (dolist (frame (frame-list))
    (unless (equal frame not-frame)
      (with-selected-frame frame
        (let ((persp (gethash name (perspectives-hash))))
          (if persp (cl-return-from persp-all-get (persp-buffers persp))))))))

(defun persp-get-buffers (&optional persp-or-name frame)
  "Return the list of PERSP-OR-NAME buffers in FRAME.
If PERSP-OR-NAME isn't given or nil use the current perspective.
If FRAME isn't nil, fetch PERSP-OR-NAME in FRAME, otherwise stay
in the selected frame.

Uses `persp-current-buffers' as backhand.

See also `persp-get-buffer-names' to get only live buffers.  See
`persp-all-get' to get buffers from all frames."
  (let ((name (if (stringp persp-or-name)
                  persp-or-name
                (persp-name (or persp-or-name (persp-curr)))))
        buffers)
    (with-selected-frame (or frame (selected-frame))
      (when (member name (persp-names))
        (with-perspective name
          (setq buffers (persp-current-buffers)))))
    buffers))

(defun persp-get-buffer-names (&optional persp-or-name frame)
  "Return the list of PERSP-OR-NAME live buffers in FRAME.
If PERSP-OR-NAME isn't given or nil use the current perspective.
If FRAME isn't nil, fetch PERSP-OR-NAME in FRAME, otherwise stay
in the selected frame.

Uses `persp-current-buffer-names' as backhand.

See also `persp-get-buffers' to get all buffers."
  (let ((name (if (stringp persp-or-name)
                  persp-or-name
                (persp-name (or persp-or-name (persp-curr)))))
        buffers)
    (with-selected-frame (or frame (selected-frame))
      (when (member name (persp-names))
        (with-perspective name
          (setq buffers (persp-current-buffer-names)))))
    buffers))

(defun persp-read-buffer (prompt &optional def require-match predicate)
  "A replacement for the built-in `read-buffer', meant to be used with `read-buffer-function'.
Return the name of the buffer selected, only selecting from buffers
within the current perspective.

PROMPT, DEF, and REQUIRE-MATCH documented in `read-buffer'.

With a prefix arg, uses the old `read-buffer' instead."
  (persp-protect
    (let ((read-buffer-function nil))
      (if current-prefix-arg
          (read-buffer prompt def require-match predicate)
        ;; Most of this is taken from `minibuffer-with-setup-hook',
        ;; slightly modified because it's not a macro.
        ;; The only functional difference is that the append argument
        ;; to add-hook is t, so that it'll be run after the hook added
        ;; by `read-buffer-to-switch'.
        (let ((rb-completion-table (persp-complete-buffer))
              (persp-read-buffer-hook))
          (setq persp-read-buffer-hook
                (lambda ()
                  (remove-hook 'minibuffer-setup-hook persp-read-buffer-hook)
                  (setq minibuffer-completion-table rb-completion-table)))
          (unwind-protect
              (progn
                (add-hook 'minibuffer-setup-hook persp-read-buffer-hook t)
                (read-buffer prompt def require-match predicate))
            (remove-hook 'minibuffer-setup-hook persp-read-buffer-hook)))))))

(defun persp-complete-buffer ()
  "Perform completion on all buffers within the current perspective."
  (let ((persp-names (mapcar 'buffer-name (persp-current-buffers))))
    (apply-partially 'completion-table-with-predicate
                     (or minibuffer-completion-table 'internal-complete-buffer)
                     (lambda (name)
                       (member (if (consp name) (car name) name) persp-names))
                     nil)))

(cl-defun persp-import (name &optional dont-switch)
  "Import a perspective named NAME from another frame.  If DONT-SWITCH
is non-nil or with prefix arg, don't switch to the new perspective."
  ;; TODO: Have some way of selecting which frame the perspective is imported from.
  (interactive "i\nP")
  (unless name
    (setq name (funcall persp-interactive-completion-function
                        "Import perspective: " (persp-all-names (selected-frame)) nil t)))
  (if (and (gethash name (perspectives-hash))
           (not (yes-or-no-p (concat "Perspective `" name "' already exits. Continue? "))))
      (cl-return-from persp-import))
  (let ((buffers (persp-all-get name (selected-frame)))
        persp)
    (if (null buffers)
        (persp-error "Perspective `%s' doesn't exist in another frame" name))
    (setq persp (make-persp :name name :buffers buffers
                  (switch-to-buffer (cl-loop for buffer in buffers
                                             if (buffer-live-p buffer)
                                             return buffer))
                  (persp-reset-windows)))
    (if dont-switch
        (persp-update-modestring)
      (persp-activate persp))))

(defadvice switch-to-buffer (after persp-add-buffer-adv)
  "Add BUFFER to the current perspective.

See also `persp-add-buffer'."
  (persp-protect
    (let ((buf (ad-get-arg 0)))
      (when buf
        (persp-add-buffer buf)))))

(defadvice display-buffer (after persp-add-buffer-adv)
  "Add BUFFER to the perspective for the frame on which it's displayed.

See also `persp-add-buffer'."
  (persp-protect
    (when ad-return-value
      (let ((buf (ad-get-arg 0))
            (frame (window-frame ad-return-value)))
        (when (and buf frame)
          (with-selected-frame frame
            (persp-add-buffer buf)))))))

(defadvice set-window-buffer (after persp-add-buffer-adv)
  "Add BUFFER to the perspective for window's frame.

See also `persp-add-buffer'."
  (persp-protect
    (let ((buf (ad-get-arg 1))
          (frame (window-frame (ad-get-arg 0))))
      (when (and buf frame)
        (with-selected-frame frame
          (persp-add-buffer buf))))))

(defadvice switch-to-prev-buffer (around persp-ensure-buffer-in-persp)
  "Ensure that the selected buffer is in WINDOW's perspective."
  (let* ((window (window-normalize-window window t))
         (frame (window-frame window))
         (old-buffer (window-buffer window)))
    ad-do-it
    (let ((buffer (window-buffer window)))
      (with-selected-frame frame
        (unless (persp-is-current-buffer buffer)
          ;; If a buffer from outside this perspective was selected, it's because
          ;; this perspective is out of buffers. For lack of any better option, we
          ;; recreate the scratch buffer.
          ;;
          ;; If we were just in a scratch buffer, change the name slightly.
          ;; Otherwise our new buffer will get deleted too.
          (let ((name (persp-scratch-buffer)))
            (when (and bury-or-kill (equal name (buffer-name old-buffer)))
              (setq name (persp-scratch-buffer)))
            (with-selected-window window
              (switch-to-buffer name)
              (funcall initial-major-mode))))))))

(defadvice recursive-edit (around persp-preserve-for-recursive-edit)
  "Preserve the current perspective when entering a recursive edit."
  (persp-protect
    (persp-save)
    (persp-let-frame-parameters ((persp--recursive (persp-curr)))
      (let ((old-hash (copy-hash-table (perspectives-hash))))
        ad-do-it
        ;; We want the buffer lists that were created in the recursive edit,
        ;; but not the window configurations
        (maphash (lambda (key new-persp)
                   (let ((persp (gethash key old-hash)))
                     (when persp
                       (setf (persp-buffers persp) (persp-buffers new-persp)))))
                 (perspectives-hash))
        (set-frame-parameter nil 'persp--hash old-hash)))))

(defadvice exit-recursive-edit (before persp-restore-after-recursive-edit)
  "Restore the old perspective when exiting a recursive edit."
  (persp-protect
    (if (frame-parameter nil 'persp--recursive) (persp-switch (persp-name (frame-parameter nil 'persp--recursive))))))


;;;###autoload
(define-minor-mode persp-mode
  "Toggle perspective mode.
When active, keeps track of multiple 'perspectives',
named collections of buffers and window configurations."
  :global t
  :keymap persp-mode-map
  (if persp-mode
      ;; activate persp-mode, preferably in an idempotent manner: the presence
      ;; of a non-nil 'persp--hash parameter in (selected-frame) should be a
      ;; good proxy for whether the mode is actually active...
      (unless (frame-parameter nil 'persp--hash)
        (persp-protect
          (when (bound-and-true-p server-process)
            (setq persp-started-after-server-mode t))
          ;; TODO: Convert to nadvice, which has been available since 24.4 and is
          ;; the earliest Emacs version Perspective supports.
          (ad-activate 'switch-to-buffer)
          (ad-activate 'display-buffer)
          (ad-activate 'set-window-buffer)
          (ad-activate 'switch-to-prev-buffer)
          (ad-activate 'recursive-edit)
          (ad-activate 'exit-recursive-edit)
          (persp--helm-enable)
          (add-hook 'after-make-frame-functions 'persp-init-frame)
          (add-hook 'delete-frame-functions 'persp-delete-frame)
          (add-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
          (when persp-avoid-killing-last-buffer-in-perspective
            (add-hook 'kill-buffer-query-functions 'persp-maybe-kill-buffer))
          (setq read-buffer-function 'persp-read-buffer)
          (mapc 'persp-init-frame (frame-list))
          (setf (persp-current-buffers) (buffer-list))
          (unless (or persp-mode-prefix-key persp-suppress-no-prefix-key-warning)
            (display-warning
             'perspective
             (format-message "persp-mode-prefix-key is not set! If you see this warning, you are using Emacs 28 or later, and have not customized persp-mode-prefix-key. Please refer to the Perspective documentation for further information (https://github.com/nex3/perspective-el). To suppress this warning without choosing a prefix key, set persp-suppress-no-prefix-key-warning to `t'.")
             :warning))
          (run-hooks 'persp-mode-hook)))
    ;; deactivate persp-mode
    (persp--helm-disable)
    (ad-deactivate-regexp "^persp-.*")
    (remove-hook 'delete-frame-functions 'persp-delete-frame)
    (remove-hook 'after-make-frame-functions 'persp-init-frame)
    (remove-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
    (remove-hook 'kill-buffer-query-functions 'persp-maybe-kill-buffer)
    (setq read-buffer-function nil)
    (set-frame-parameter nil 'persp--hash nil)
    (setq global-mode-string (delete '(:eval (persp-mode-line)) global-mode-string))
    (let ((default-header-line-format (default-value 'header-line-format)))
      (set-default 'header-line-format (delete '(:eval (persp-mode-line)) default-header-line-format))
      (unless (delete "" default-header-line-format)
        ;; need to set header-line-format to nil to completely remove the header from the buffer
        (set-default 'header-line-format nil)))))

(defun persp-init-frame (frame)
  "Initialize the perspectives system in FRAME.
By default, this uses the current frame."
  (with-selected-frame frame
    (modify-frame-parameters
     frame
     '((persp--hash) (persp--curr) (persp--last) (persp--recursive) (persp--modestring)))
    ;; Don't set these variables in modify-frame-parameters
    ;; because that won't do anything if they've already been accessed
    (set-frame-parameter frame 'persp--hash (make-hash-table :test 'equal :size 10))
    (when persp-show-modestring
      (if (eq persp-show-modestring 'header)
          (let ((val (or (default-value 'header-line-format) '(""))))
            (unless (member '(:eval (persp-mode-line)) val)
              (set-default 'header-line-format (append val '((:eval (persp-mode-line)))))))
        (setq global-mode-string (or global-mode-string '("")))
        (unless (member '(:eval (persp-mode-line)) global-mode-string)
          (setq global-mode-string (append global-mode-string '((:eval (persp-mode-line)))))))
      (persp-update-modestring))
    ;; A frame must open with a reasonable initial buffer in its main
    ;; perspective. This behaves differently from an emacsclient invocation, but
    ;; should respect `initial-buffer-choice'.
    (when (frame-parameter frame 'client)
      (let* ((scratch-buf (persp-scratch-buffer persp-initial-frame-name))
             (init-buf (cond ((stringp initial-buffer-choice) initial-buffer-choice)
                             ((functionp initial-buffer-choice) (or (funcall initial-buffer-choice)
                                                                    scratch-buf))
                             (t scratch-buf))))
        (switch-to-buffer init-buf t)))
    (persp-activate
     (make-persp :name persp-initial-frame-name :buffers (list (current-buffer))
       :window-configuration (current-window-configuration)
       :point-marker (point-marker)))))

(defun persp-delete-frame (frame)
  "Clean up perspectives in FRAME.
By default this uses the current frame."
  (with-selected-frame frame
    (unless persp-started-after-server-mode
      (mapcar #'persp-kill (persp-names)))))

(defun persp-make-variable-persp-local (variable)
  "Make VARIABLE become perspective-local.
This means that whenever a new perspective is switched into, the
variable will take on its local value for that perspective.  When
a new perspective is created, the variable will inherit its value
from the current perspective at time of creation."
  (unless (assq variable (persp-local-variables (persp-curr)))
    (let ((entry (list variable (symbol-value variable))))
      (dolist (frame (frame-list))
        (cl-loop for persp being the hash-values of (perspectives-hash frame)
                 do (push entry (persp-local-variables persp)))))))

(defmacro persp-setup-for (name &rest body)
  "Add code that should be run to set up the perspective named NAME.
Whenever a new perspective named NAME is created, runs BODY in
it. In addition, if one exists already, runs BODY in it immediately."
  (declare (indent 1))
  `(progn
     (add-hook 'persp-created-hook
               (lambda ()
                 (when (string= (persp-current-name) ,name)
                   ,@body))
               'append)
     (when (gethash ,name (perspectives-hash))
       (with-perspective ,name ,@body))))

(defun persp-set-ido-buffers ()
  "Restrict the ido buffer to the current perspective."
  (defvar ido-temp-list)
  (let ((persp-names
         (remq nil (mapcar 'buffer-name (persp-current-buffers* t))))
        (indices (make-hash-table :test 'equal)))
    (cl-loop for elt in ido-temp-list
             for i upfrom 0
             do (puthash elt i indices))
    (setq ido-temp-list
          (sort (cl-intersection persp-names ido-temp-list)
                (lambda (a b)
                  (< (gethash a indices)
                     (gethash b indices)))))))

(defun quick-perspective-keys ()
  "Bind quick key commands to switch to perspectives.
All C-S-letter key combinations are bound to switch to the first
perspective beginning with the given letter."
  (cl-loop for c from ?a to ?z
           do (define-key persp-mode-map
                (read-kbd-macro (concat "C-S-" (string c)))
                `(lambda ()
                   (interactive)
                   (persp-switch-quick ,c)))))

(defun persp-turn-off-modestring ()
  "Deactivate the perspective modestring."
  (interactive)
  (set-frame-parameter nil 'persp--modestring nil)
  (setq persp-show-modestring nil))

(defun persp-turn-on-modestring ()
  "Activate the perspective modestring."
  (interactive)
  (setq persp-show-modestring t)
  (persp-update-modestring))

(cl-defun persp-other-buffer (&optional skip-buffer _visible-ok frame)
  "A version of `other-buffer' which respects perspectives.
This respects ido-ignore-buffers.
TODO: The VISIBLE-OK parameter is currently ignored."
  (let ((ignore-rx (persp--make-ignore-buffer-rx)))
    (cl-loop for b in (buffer-list frame) do
             (let ((name (buffer-name b)))
               (when (and (not (and (buffer-live-p skip-buffer) (equal skip-buffer b)))
                          (not (string-prefix-p " " name))
                          (not (string-match-p ignore-rx name))
                          (member b (persp-current-buffers)))
                 (cl-return-from persp-other-buffer b)))))
  ;; fallback:
  (persp-get-scratch-buffer))


;;; --- perspective-aware buffer switchers

;; Buffer switching integration: useful for frameworks which enhance the
;; built-in completing-read (e.g., Selectrum).
;;;###autoload
(defun persp-switch-to-buffer* (buffer-or-name)
  "Like `switch-to-buffer', restricted to the current perspective.
This respects ido-ignore-buffers, since we automatically add
buffer filtering to ido-mode already (see use of
PERSP-SET-IDO-BUFFERS)."
  (interactive
   (list
    (if (or current-prefix-arg (not persp-mode))
        (let ((read-buffer-function nil))
          (read-buffer-to-switch "Switch to buffer"))
      (let* ((candidates (persp-current-buffer-names t))
             (other (buffer-name (persp-other-buffer (current-buffer)))))
        ;; NB: This intentionally calls completing-read instead of
        ;; persp-interactive-completion-function, since it is expected to have
        ;; been replaced by a completion framework.
        (completing-read (format "Switch to buffer%s: "
                                 (if other
                                     (format " (default %s)" other)
                                   ""))
                         (lambda (string predicate action)
                           (if (eq 'metadata action)
                               '(metadata (category . buffer))
                             (complete-with-action action candidates string predicate)))
                         nil nil nil nil
                         other)))))
  (let ((buffer (window-normalize-buffer-to-switch-to buffer-or-name)))
    (switch-to-buffer buffer)))

;; Buffer killing integration: useful for frameworks which enhance the
;; built-in completing-read (e.g., Selectrum).
;;;###autoload
(defun persp-kill-buffer* (buffer-or-name)
  "Like `kill-buffer', restricted to the current perspective.
This respects ido-ignore-buffers, since we automatically add
buffer filtering to ido-mode already (see use of
PERSP-SET-IDO-BUFFERS)."
  (interactive
   (list
    (if (or current-prefix-arg (not persp-mode))
        (let ((read-buffer-function nil))
          (read-buffer "Kill buffer: " (current-buffer)))
      ;; NB: This intentionally calls completing-read instead of
      ;; persp-interactive-completion-function, since it is expected to have
      ;; been replaced by a completion framework.
      (completing-read (format "Kill buffer (default %s): " (buffer-name (current-buffer)))
                       (lambda (string predicate action)
                         (if (eq 'metadata action)
                             '(metadata (category . buffer))
                           (complete-with-action action (persp-current-buffer-names) string predicate)))
                       nil nil nil nil
                       (buffer-name (current-buffer))))))
  (kill-buffer buffer-or-name))

;; Buffer switching integration: buffer-menu.
;;;###autoload
(defun persp-buffer-menu (arg)
  "Like the default C-x C-b, but filters for the current perspective's buffers."
  (interactive "P")
  (if (and persp-mode (null arg))
      (switch-to-buffer
       (list-buffers-noselect nil (seq-filter 'buffer-live-p (persp-current-buffers* t))))
    (switch-to-buffer (list-buffers-noselect))))

;; Buffer switching integration: list-buffers.
;;;###autoload
(defun persp-list-buffers (arg)
  "Like the default C-x C-b, but filters for the current perspective's buffers."
  (interactive "P")
  (if (and persp-mode (null arg))
      (display-buffer
       (list-buffers-noselect nil (seq-filter 'buffer-live-p (persp-current-buffers* t))))
    (display-buffer (list-buffers-noselect))))

;; Buffer switching integration: bs.el.
;;;###autoload
(defun persp-bs-show (arg)
  "Invoke BS-SHOW with a configuration enabled for Perspective.
With a prefix arg, show buffers in all perspectives.
This respects ido-ignore-buffers, since we automatically add
buffer filtering to ido-mode already (see use of
PERSP-SET-IDO-BUFFERS)."
  (interactive "P")
  (unless (featurep 'bs)
    (user-error "bs not loaded"))
  (defvar bs-configurations)
  (declare-function bs--show-with-configuration "bs.el")
  (let* ((ignore-rx (persp--make-ignore-buffer-rx))
         (bs-configurations (append bs-configurations
                                    (list `("perspective" nil nil
                                            ,ignore-rx (lambda (buf) (persp-buffer-filter buf t)) nil))
                                    (list `("all-perspectives" nil nil
                                            ,ignore-rx nil nil)))))
    (if (and persp-mode (null arg))
        (bs--show-with-configuration "perspective")
      (bs--show-with-configuration "all-perspectives"))))

;; Buffer switching integration: IBuffer.
;;;###autoload
(defun persp-ibuffer (arg)
  "Invoke IBUFFER with a configuration enabled for Perspective.
With a prefix arg, show buffers in all perspectives.
This respects ido-ignore-buffers, since we automatically add
buffer filtering to ido-mode already (see use of
PERSP-SET-IDO-BUFFERS)."
  (interactive "P")
  (unless (featurep 'ibuffer)
    (user-error "IBuffer not loaded"))
  (defvar ido-ignore-buffers)
  (defvar ibuffer-maybe-show-predicates)
  (if (and persp-mode (null arg))
      (let ((ibuffer-maybe-show-predicates (append ibuffer-maybe-show-predicates
                                                   (list #'(lambda (buf) (persp-buffer-filter buf t)))
                                                   ido-ignore-buffers)))
        (ibuffer))
    (ibuffer)))

;; Buffer switching integration: Consult
(with-eval-after-load 'consult
  (declare-function consult--buffer-state "consult.el")
  (declare-function consult--buffer-query "consult.el")

  (defvar persp-consult-source
    (list :name     "Perspective"
          :narrow   ?s
          :category 'buffer
          :state    #'consult--buffer-state
          :history  'buffer-name-history
          :default  t
          :items
          #'(lambda () (consult--buffer-query :sort 'visibility
                                              :predicate '(lambda (buf) (persp-is-current-buffer buf t))
                                              :as #'buffer-name)))))

;; Buffer switching integration: Ivy.
;;
;; An alternative implementation, which has the drawback of not allowing a
;; prefix argument to list all buffers:
;;
;; (defun persp-ivy-read-advice (args)
;;   (append args
;;           (list :predicate
;;                 (lambda (b) (persp-is-current-buffer (cdr b) t)))))
;; (advice-add 'ivy-read :filter-args #'persp-ivy-read-advice)
;; (advice-remove 'ivy-read #'persp-ivy-read-advice)

(defun persp--switch-buffer-ivy-counsel-helper (arg fallback)
  (unless (featurep 'ivy)
    (user-error "Ivy not loaded"))
  (declare-function ivy-read "ivy.el")
  (if (and persp-mode (null arg))
      (let ((real-ivy-read (symbol-function 'ivy-read))
            (current-bufs (persp-current-buffers* t)))
        (cl-letf (((symbol-function 'ivy-read)
                   (lambda (&rest args)
                     (apply real-ivy-read
                            (append args
                                    (list :predicate
                                          (lambda (b)
                                            (memq (cdr b) current-bufs))))))))
          (funcall fallback)))
    (funcall fallback)))

;;;###autoload
(defun persp-ivy-switch-buffer (arg)
  "A version of `ivy-switch-buffer' which respects perspectives."
  (interactive "P")
  (declare-function ivy-switch-buffer "ivy.el")
  (persp--switch-buffer-ivy-counsel-helper arg #'ivy-switch-buffer))

;;;###autoload
(defun persp-counsel-switch-buffer (arg)
  "A version of `counsel-switch-buffer' which respects perspectives."
  (interactive "P")
  (unless (featurep 'counsel)
    (user-error "Counsel not loaded"))
  (declare-function counsel-switch-buffer "counsel.el")
  (persp--switch-buffer-ivy-counsel-helper arg #'counsel-switch-buffer))


;;; --- Helm integration

(defun persp--helm-buffer-list-filter (bufs)
  (if current-prefix-arg
      bufs
    (persp-buffer-list-filter bufs t)))

(defun persp--helm-remove-buffers-from-perspective (_arg)
  (interactive)
  (declare-function helm-marked-candidates "helm.el")
  (cl-loop for candidate in (helm-marked-candidates) do
           (persp-remove-buffer candidate)))

(defun persp--helm-add-buffers-to-perspective (_arg)
  (declare-function helm-marked-candidates "helm.el")
  (cl-loop for candidate in (helm-marked-candidates) do
           (persp-add-buffer candidate)))

(defun persp--helm-activate (&rest _args)
  (defvar helm-source-buffers-list)
  (declare-function helm-make-source "helm-source.el")
  (declare-function helm-add-action-to-source "helm.el")
  ;; XXX: Ugly Helm initialization, works around the way
  ;; helm-source-buffers-list is lazily initialized in helm-buffers.el
  ;; helm-buffers-list and helm-mini (copypasta code).
  (require 'helm-buffers)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  ;; actually activate things
  (advice-add 'helm-buffer-list-1 :filter-return #'persp--helm-buffer-list-filter)
  (helm-add-action-to-source
   "Perspective: Add buffer to current perspective"
   #'persp--helm-add-buffers-to-perspective helm-source-buffers-list)
  (helm-add-action-to-source
   "Perspective: Remove buffer from current perspective"
   #'persp--helm-remove-buffers-from-perspective helm-source-buffers-list)
  ;; remove persp--helm-activate advice once it has run
  (advice-remove 'helm-initial-setup #'persp--helm-activate))

(defun persp--helm-enable ()
  ;; We do not know if Helm has been loaded before Perspective is activated, so
  ;; we need a way to activate Perspective-Helm integration once we know for
  ;; certain that Helm is ready. An advice functino should do the trick, which
  ;; will remove itself once it does its job.
  (advice-add 'helm-initial-setup :before #'persp--helm-activate))

(defun persp--helm-disable ()
  (defvar helm-source-buffers-list)
  (declare-function helm-delete-action-from-source "helm.el")
  (if (not (featurep 'helm))
      (advice-remove 'helm-initial-setup #'persp--helm-activate)
    ;; actual cleanup if Helm-perspective integration has loaded:
    (helm-delete-action-from-source
     #'persp--helm-remove-buffers-from-perspective helm-source-buffers-list)
    (helm-delete-action-from-source
     #'persp--helm-add-buffers-to-perspective helm-source-buffers-list)
    (advice-remove 'helm-buffer-list-1 #'persp--helm-buffer-list-filter)))


;;; --- durability implementation (persp-state-save and persp-state-load)

;; Symbols namespaced by persp--state (internal) and persp-state (user
;; functions) provide functionality which allows saving perspective state on
;; disk, and loading it into another Emacs session.
;;
;; The relevant commands are persp-state-save and persp-state-load (aliased to
;; persp-state-restore).
;;
;; The (on-disk) data structure looks like this:
;;
;; {
;;   :files [...]
;;   :frames [
;;     {
;;       :persps {
;;         "persp1" {
;;           :buffers [...]
;;           :windows [...]
;;         }
;;       }
;;       :order [...]
;;       :merge-list
;;     }
;;   ]
;; }

(cl-defstruct persp--state-complete
  files
  frames)

;; Keep around old version to maintain backwards compatibility.
(cl-defstruct persp--state-frame
  persps
  order)

(cl-defstruct persp--state-frame-v2
  persps
  order
  merge-list)

(cl-defstruct persp--state-single
  buffers
  windows)

(defun persp--state-complete-v2 (state-complete)
  "Apply this function to persp--state-complete structs to be guaranteed a
persp--state-complete that is compatible with merge-list saving. Useful for
maintaining backwards compatibility."
  (let* ((state-frames (persp--state-complete-frames state-complete))
         (state-frames-v2
          (mapcar (lambda (state-frame)
                    (if (persp--state-frame-v2-p state-frame)
                        state-frame
                      (make-persp--state-frame-v2
                       :persps (persp--state-frame-persps state-frame)
                       :order (persp--state-frame-order state-frame)
                       :merge-list nil)))
                  state-frames)))
    (make-persp--state-complete
     :files (persp--state-complete-files state-complete)
     :frames state-frames-v2)))

(defun persp--state-interesting-buffer-p (buffer)
  (and (buffer-name buffer)
       (not (string-match "^[[:space:]]*\\*" (buffer-name buffer)))
       (or (buffer-file-name buffer)
           (with-current-buffer buffer (equal major-mode 'dired-mode)))))

(defun persp--state-file-data ()
  (cl-loop for buffer in (buffer-list)
        if (persp--state-interesting-buffer-p buffer)
        collect (or (buffer-file-name buffer)
                    (with-current-buffer buffer ; dired special case
                      default-directory))))

(defun persp--state-window-state-massage (entry persp valid-buffers)
  "This is a primitive code walker. It removes references to
potentially problematic buffers from the data structure created
by window-state-get and replaces them with references to the
perspective-specific *scratch* buffer. Buffers are considered
'problematic' when they have no underlying file, or are otherwise
transient.

The need for a recursive walk, and the consequent complexity of
this function, arises from the nature of the data structure
returned by window-state-get. That data structure is essentially
a tree represented as a Lisp list. It can contain several kinds
of nodes, including properties, nested trees representing window
splits, and windows (referred to internally as leaf nodes).

For the purposes of preserving window state, we only care about
nodes in this data structure which refer to buffers, i.e., lists
with the symbol 'buffer in the first element. These 'buffer lists
can be deeply buried inside the data structure, because it
recursively describes the layout of all windows in the given
frame. They are always nested in lists with the symbol 'leaf in
the first element.

And so, the walker descends the data structure and preserves
everything it finds. When it notices a 'leaf, it iterates over
its properties until it finds a 'buffer. If the 'buffer points to
a buffer which can be reasonably saved, it leaves it alone.
Otherwise, it replaces that buffer's node with one which points
to the perspective's *scratch* buffer."
  (cond
    ;; base case 1
    ((not (consp entry))
     entry)
    ;; base case 2
    ((atom (cdr entry))
     entry)
    ;; leaf: modify this
    ((eq 'leaf (car entry))
     (let ((leaf-props (cdr entry)))
       (cons 'leaf
             (cl-loop for prop in leaf-props
                   collect (if (not (eq 'buffer (car prop)))
                               prop
                             (let ((bn (cadr prop)))
                               (if (member bn valid-buffers)
                                   prop
                                 (cons 'buffer
                                       (cons (persp-scratch-buffer persp)
                                             (cddr prop))))))))))
    ;; recurse
    (t (cons (car entry) (cl-loop for e in (cdr entry)
                               collect (persp--state-window-state-massage e persp valid-buffers))))))

(defun persp--state-frame-data ()
  (cl-loop for frame in (frame-list)
           if (frame-parameter frame 'persp--hash) ; XXX: filter non-perspective-enabled frames
           collect (with-selected-frame frame
                     (let ((persps-in-frame (make-hash-table :test 'equal))
                           (persp-names-in-order (persp-names)))
                       (cl-loop for persp in persp-names-in-order do
                                (unless (persp-killed-p (gethash persp (perspectives-hash)))
                                  (with-perspective persp
                                    (let* ((buffers
                                            (cl-loop for buffer in (persp-current-buffers)
                                                     if (persp--state-interesting-buffer-p buffer)
                                                     collect (buffer-name buffer)))
                                           (windows
                                            (cl-loop for entry in (window-state-get (frame-root-window) t)
                                                     collect (persp--state-window-state-massage entry persp buffers))))
                                      (puthash persp
                                               (make-persp--state-single
                                                :buffers buffers
                                                :windows windows)
                                               persps-in-frame)))))
                       (make-persp--state-frame-v2
                        :persps persps-in-frame
                        :order persp-names-in-order
                        :merge-list (frame-parameter nil 'persp-merge-list))))))

(defun persp-purge-exception-p (buffer)
  (if (buffer-live-p buffer)
      (let (result)
        (dolist (exception persp-purge-initial-persp-on-save-exceptions result)
          (setq result (or result (string-match-p exception (buffer-name buffer))))))
    nil))

;;;###autoload
(cl-defun persp-state-save (&optional file interactive?)
  "Save the current perspective state to FILE.

FILE defaults to the value of persp-state-default-file if it is
set.

Each perspective's buffer list and window layout will be saved.
Frames and their associated perspectives will also be saved,
but not the original frame sizes.

Buffers with * characters in their names, as well as buffers without
associated files will be ignored. If such buffers are currently
visible in a perspective as windows, they will be saved as
'*scratch* (persp)' buffers."
  (interactive (list
                (read-file-name "Save perspective state to file: "
                                persp-state-default-file
                                persp-state-default-file)
                t))
  (unless persp-mode
    (message "persp-mode not enabled, nothing to save")
    (cl-return-from persp-state-save))
  (let ((target-file (if (and file (not (string-equal "" file)))
                         ;; file provided as argument, just use it
                         (expand-file-name file)
                       ;; no file provided as argument
                       (if interactive?
                           ;; return nil in interactive call mode, since
                           ;; read-file-name should have provided a reasonable
                           ;; default
                           nil
                         ;; in non-interactive call mode, we want to fall back to
                         ;; the default, but only if it is set
                         (if (and persp-state-default-file
                                  (not (string-equal "" persp-state-default-file)))
                             (expand-file-name persp-state-default-file)
                           nil)))))
    (unless target-file
      (user-error "No target file specified"))
    ;; overwrite the target file if:
    ;; - the file does not exist, or
    ;; - the file is not the one set in persp-state-default-file, or
    ;; - the user called this function with a prefix argument, or
    ;; - the user approves overwriting the file when prompted
    (when (and (file-exists-p target-file)
               (not (string-equal (if (and persp-state-default-file
                                           (not (string-equal "" persp-state-default-file)))
                                      (expand-file-name persp-state-default-file)
                                    "")
                                  target-file))
               (not (or current-prefix-arg
                        (yes-or-no-p "Target file exists. Overwrite? "))))
      (user-error "Cancelled persp-state-save"))
    ;; before hook
    (run-hooks 'persp-state-before-save-hook)
    ;; optionally purge initial perspective of entries
    (when persp-purge-initial-persp-on-save
      (mapc 'kill-buffer (cl-remove-if #'persp-purge-exception-p (persp-all-get persp-initial-frame-name nil))))
    ;; actually save
    (persp-save)
    (let ((state-complete (make-persp--state-complete
                           :files (persp--state-file-data)
                           :frames (persp--state-frame-data))))
      ;; create or overwrite target-file:
      (with-temp-file target-file (prin1 state-complete (current-buffer))))
    ;; after hook
    (run-hooks 'persp-state-after-save-hook)))

;;;###autoload
(defun persp-state-load (file)
  "Restore the perspective state saved in FILE.

FILE defaults to the value of persp-state-default-file if it is
set.

Frames are restored, along with each frame's perspective list and merge list.
Each perspective's buffer list and window layout are also
restored."
  (interactive (list
                (read-file-name "Restore perspective state from file: "
                                persp-state-default-file
                                persp-state-default-file)))
  (unless (file-exists-p file)
    (user-error "File not found: %s" file))
  (persp-mode 1)
  ;; before hook
  (run-hooks 'persp-state-before-load-hook)
  ;; actually load
  (let ((tmp-persp-name (format "%04x%04x" (random (expt 16 4)) (random (expt 16 4))))
        (frame-count 0)
        (state-complete (persp--state-complete-v2
                         (read
                          (with-temp-buffer
                            (insert-file-contents file)
                            (buffer-string))))))
    ;; open all files in a temporary perspective to avoid polluting "main"
    (persp-switch tmp-persp-name)
    (cl-loop for file in (persp--state-complete-files state-complete) do
             (when (file-exists-p file)
               (find-file file)))
    ;; iterate over the frames
    (cl-loop for frame in (persp--state-complete-frames state-complete) do
             (cl-incf frame-count)
             (let ((emacs-frame (if (> frame-count 1) (make-frame-command) (selected-frame)))
                   (frame-persp-table (persp--state-frame-v2-persps frame))
                   (frame-persp-order (reverse (persp--state-frame-v2-order frame)))
                   (frame-persp-merge-list (persp--state-frame-v2-merge-list frame)))
               (with-selected-frame emacs-frame
                 ;; restore the merge list
                 (set-frame-parameter emacs-frame 'persp-merge-list frame-persp-merge-list)
                 ;; iterate over the perspectives in the frame in the appropriate order
                 (cl-loop for persp in frame-persp-order do
                          (let ((state-single (gethash persp frame-persp-table)))
                            (persp-switch persp)
                            (set-frame-parameter nil 'persp-merge-list frame-persp-merge-list)
                            (cl-loop for buffer in (persp--state-single-buffers state-single) do
                                     (persp-add-buffer buffer))
                            ;; XXX: split-window-horizontally is necessary for
                            ;; window-state-put to succeed? Something goes haywire with root
                            ;; windows without it.
                            (split-window-horizontally)
                            (window-state-put (persp--state-single-windows state-single)
                                              (frame-root-window emacs-frame)
                                              'safe))))))
    ;; cleanup
    (persp-kill tmp-persp-name))
  ;; after hook
  (run-hooks 'persp-state-after-load-hook))

(defalias 'persp-state-restore 'persp-state-load)


;;; --- perspective merging

(defun persp-get-merge (base-name merged-name &optional frame)
  "Return a merge in FRAME with :base-perspective BASE-NAME and
:merged-perspective MERGED-NAME."
  (cl-find-if
   (lambda (m)
     (and (string= base-name (plist-get m :base-perspective))
          (string= merged-name (plist-get m :merged-perspective))))
   (frame-parameter frame 'persp-merge-list)))

(defun persp-merges-with-base (&optional name frame)
  "Return a list of all merges in FRAME with base perspective NAME."
  (if (null name) (setq name (persp-current-name)))
  (cl-remove-if-not
   (lambda (m)
     (string= name (plist-get m :base-perspective)))
   (frame-parameter frame 'persp-merge-list)))

(defun persp-perspectives-merged-with-base (&optional name frame)
  "Return a list of all perspectives in FRAME that are merged to NAME."
  (if (null name) (setq name (persp-current-name)))
  (mapcar (lambda (m) (plist-get m :merged-perspective))
          (persp-merges-with-base name frame)))

(defun persp-merge (base-persp-name to-merge-persp-name)
  "Merge the buffer list of TO-MERGE-PERSP-NAME into the buffer list for
BASE-PERSP-NAME."
  (interactive
   (list (persp-current-name)
         (funcall persp-interactive-completion-function
                  "Perspective name: "
                  (remove (persp-current-name) (persp-names)) nil t)))
  (cl-assert (member base-persp-name (persp-names)))
  (cl-assert (member to-merge-persp-name (persp-names)))
  (let* ((merge (persp-get-merge base-persp-name to-merge-persp-name))
         (all-to-merge-persp-buffers (persp-get-buffer-names to-merge-persp-name))
         (merged-into-to-merge-persp-buffers (cl-loop for m in (persp-merges-with-base to-merge-persp-name)
                                                      append (plist-get m :merged-buffers)))
         (buffers-to-merge (delete-dups
                            (cl-remove-if
                             (lambda (buf)
                               (or (member buf merged-into-to-merge-persp-buffers)
                                   (string= buf (persp-scratch-buffer to-merge-persp-name))))
                             all-to-merge-persp-buffers))))
    (with-perspective base-persp-name
      (if merge
          ;; update an existing merge
          (let ((merged-buffers (plist-get merge :merged-buffers)))
            (dolist (buf buffers-to-merge)
              (unless (persp-is-current-buffer (get-buffer buf))
                (persp-add-buffer buf)
                (push buf merged-buffers)))
            (set-frame-parameter
             nil
             'persp-merge-list
             (cl-nsubstitute-if (list :base-perspective base-persp-name
                                      :merged-perspective to-merge-persp-name
                                      :merged-buffers merged-buffers)
                                (lambda (m) (equal merge m))
                                (frame-parameter nil 'persp-merge-list))))
        ;; create a new merge
        (let ((merged-buffers))
          (dolist (buf buffers-to-merge)
            (unless (persp-is-current-buffer (get-buffer buf))
              (persp-add-buffer buf)
              (push buf merged-buffers)))
          (set-frame-parameter
           nil
           'persp-merge-list
           (push (list :base-perspective base-persp-name
                       :merged-perspective to-merge-persp-name
                       :merged-buffers merged-buffers)
                 (frame-parameter nil 'persp-merge-list))))))))

(defun persp-unmerge (base-persp-name to-unmerge-persp-name)
  "Unmerge the buffers from TO-UNMERGE-PERSP-NAME from BASE-PERSP-NAME that were
were merged in from a previous call to `persp-merge'."
  (interactive
   (let* ((base-persp-name (persp-current-name))
          (persps-merged-with-base (persp-perspectives-merged-with-base base-persp-name))
          (to-unmerge-persp-name
           (when persps-merged-with-base
             (funcall persp-interactive-completion-function
                      "Perspective name: "
                      persps-merged-with-base nil t))))
     (list base-persp-name to-unmerge-persp-name)))
  (let ((merge (persp-get-merge base-persp-name to-unmerge-persp-name)))
    (cond ((null to-unmerge-persp-name)
           (message "No perspectives merged to \"%s\"" base-persp-name))
          ((null merge)
           (message "\"%s\" is not merged to \"%s\"" to-unmerge-persp-name base-persp-name))
          (t (with-perspective base-persp-name
               (dolist (buf (plist-get merge :merged-buffers))
                 (persp-remove-buffer buf))
               (set-frame-parameter
                nil
                'persp-merge-list
                (remove merge (frame-parameter nil 'persp-merge-list))))))))


;;; --- ibuffer filter group code

(with-eval-after-load 'ibuffer
  (defvar ibuffer-filtering-alist nil)
  (define-ibuffer-filter persp-name
      "Toggle current view to buffers with persp name QUALIFIER."
    (:description "persp-name"
                  :reader (read-regexp "Filter by persp name (regexp): "))
    (ibuffer-awhen (persp-ibuffer-name buf)
      (if (stringp qualifier)
          (or (string-match-p qualifier (car it))
              (string-match-p qualifier (cdr-safe it)))
        (equal qualifier it)))))

(defun persp-ibuffer-default-group-name (persp-name)
  "Produce an ibuffer group name string for PERSP-NAME."
  (format "%s" persp-name))

(defun persp-ibuffer-name (buf)
  "Return a PERSP-NAME of BUF."
  (let ((persp-names (cl-loop for persp-name in (persp-all-names)
                              if (memq buf (persp-all-get persp-name nil))
                              collect persp-name)))
    (list (car persp-names))))

;;;###autoload
(defun persp-ibuffer-generate-filter-groups ()
  "Create a set of ibuffer filter groups based on the persp name of buffers."
  (declare-function ibuffer-remove-duplicates "ibuf-ext.el")
  (declare-function ibuffer-push-filter "ibuf-ext.el")
  (declare-function ibuffer-pop-filter "ibuf-ext.el")
  (let ((persp-names (ibuffer-remove-duplicates
                      (delq nil (mapcar 'persp-ibuffer-name (buffer-list))))))
    (mapcar (lambda (persp-name)
              (cons (persp-ibuffer-default-group-name (car persp-name))
                    `((persp-name . ,persp-name))))
            persp-names)))

;;;###autoload
(defun persp-ibuffer-set-filter-groups ()
  "Set the current filter groups to filter by persp name."
  (interactive)
  (unless (featurep 'ibuffer)
    (user-error "IBuffer not loaded"))
  (defvar ibuffer-filter-groups)
  (declare-function ibuffer-update "ibuffer.el")
  (setq ibuffer-filter-groups (persp-ibuffer-generate-filter-groups))
  (message "persp-ibuffer: groups set")
  (let ((ibuf (get-buffer "*Ibuffer*")))
    (when ibuf
      (with-current-buffer ibuf
        (pop-to-buffer ibuf)
        (ibuffer-update nil t)))))


;;; --- xref code

;; xref is not available in Emacs 24, so be careful:
(with-eval-after-load 'xref
  (defvar persp--xref-marker-ring (make-hash-table :test 'equal))
  (if (boundp 'xref--history)
      ;; Emacs 29:
      (defun persp--set-xref-marker-ring ()
        "Set xref--history per persp."
        (defvar xref--history)
        (let ((persp-curr-name (persp-name (persp-curr))))
          (unless (gethash persp-curr-name persp--xref-marker-ring)
            (puthash persp-curr-name (cons nil nil)
                     persp--xref-marker-ring))
          (setq xref--history (gethash persp-curr-name persp--xref-marker-ring))))
    ;; Emacs 28 and earlier:
    (defun persp--set-xref-marker-ring ()
      "Set xref--marker-ring per persp."
      (defvar xref-marker-ring-length)
      (defvar xref--marker-ring)
      (let ((persp-curr-name (persp-name (persp-curr))))
        (unless (gethash persp-curr-name persp--xref-marker-ring)
          (puthash persp-curr-name (make-ring xref-marker-ring-length)
                   persp--xref-marker-ring))
        (setq xref--marker-ring (gethash persp-curr-name persp--xref-marker-ring))))))


;;; --- done

;;; XXX: Undo nasty kludge necessary for cleanly compiling this source file by
;;; restoring saved frame parameters, and removing the variable used to save
;;; them.
(eval-when-compile
  (when (boundp 'persp--kludge-save-frame-params)
    (modify-frame-parameters nil persp--kludge-save-frame-params)
    (makunbound 'persp--kludge-save-frame-params)
    (fmakunbound 'persp--kludge-save-frame-params)
    (unintern 'persp--kludge-save-frame-params nil)))

(provide 'perspective)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; perspective.el ends here
