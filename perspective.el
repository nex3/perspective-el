;;; perspective.el --- switch between named "perspectives" of the editor  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2020 Natalie Weizenbaum <nex342@gmail.com>
;;
;; Licensed under the same terms as Emacs and under the MIT license.

;; Author: Natalie Weizenbaum <nex342@gmail.com>
;; URL: http://github.com/nex3/perspective-el
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Version: 2.15
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
                 (const :tag "Header" 'header)))

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

(defcustom persp-mode-prefix-key (kbd "C-x x")
  "Prefix key to activate perspective-map."
  :group 'perspective-mode
  :set (lambda (sym value)
         (when (and (bound-and-true-p persp-mode-map)
                    (bound-and-true-p perspective-map))
           (persp-mode-set-prefix-key value))
         (set-default sym value))
  :type 'key-sequence)

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

(defcustom persp-state-default-file nil
  "When non-nil, it provides a default argument for `persp-state-save` and `persp-state-load` to work with.

`persp-state-save` overwrites this file without prompting, which
makes it easy to use in, e.g., `kill-emacs-hook` to automatically
save state when exiting Emacs."
  :group 'perspective-mode
  :type 'file)


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

(defun persp--make-ignore-buffer-rx ()
  (defvar ido-ignore-buffers)
  (if ido-ignore-buffers
      ;; convert a list of regexps to one
      (rx-to-string (append (list 'or)
                            (mapcar (lambda (rx) `(regexp ,rx))
                                    ido-ignore-buffers)))
    ;; return a regex which matches nothing, and therefore should ignore nothing
    "$^"))

(defmacro persp-current-buffers ()
  "Return a list of all buffers in the current perspective."
  `(persp-buffers (persp-curr)))

(defun persp-current-buffer-names ()
  "Return a list of names of all living buffers in the current perspective."
  (let ((ignore-rx (persp--make-ignore-buffer-rx)))
    (cl-loop for buf in (persp-current-buffers)
             if (and (buffer-live-p buf)
                     (not (string-match-p ignore-rx (buffer-name buf))))
             collect (buffer-name buf))))

(defun persp-is-current-buffer (buf)
  "Return T if BUF is in the current perspective."
  (memq buf (persp-current-buffers)))

(defun persp-buffer-filter (buf)
  "Return F if BUF is in the current perspective. Used for
filtering in buffer display modes like ibuffer."
  (not (persp-is-current-buffer buf)))

(defun persp-buffer-list-filter (bufs)
  "Return the subset of BUFS which is in the current perspective."
  (cl-loop for buf in bufs
           if (persp-is-current-buffer (get-buffer buf))
           collect buf))

(defun persp-valid-name-p (name)
  "Return T if NAME is a valid perspective name."
  (and (not (null name))
       (not (string= "" name))))

(defmacro with-current-perspective (&rest body)
  "Operate on BODY when we are in a perspective."
  (declare (indent 0))
  `(when (persp-curr)
     ,@body))

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
(define-key persp-mode-map persp-mode-prefix-key 'perspective-map)

(define-key perspective-map (kbd "s") 'persp-switch)
(define-key perspective-map (kbd "k") 'persp-remove-buffer)
(define-key perspective-map (kbd "c") 'persp-kill)
(define-key perspective-map (kbd "r") 'persp-rename)
(define-key perspective-map (kbd "a") 'persp-add-buffer)
(define-key perspective-map (kbd "A") 'persp-set-buffer)
(define-key perspective-map (kbd "b") 'persp-switch-to-buffer)
(define-key perspective-map (kbd "i") 'persp-import)
(define-key perspective-map (kbd "n") 'persp-next)
(define-key perspective-map (kbd "<right>") 'persp-next)
(define-key perspective-map (kbd "p") 'persp-prev)
(define-key perspective-map (kbd "<left>") 'persp-prev)
(define-key perspective-map persp-mode-prefix-key 'persp-switch-last)
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

(declare-function which-key-mode "which-key.el")
(when (fboundp 'which-key-mode)
  (require 'which-key)
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
  (define-key persp-mode-map newkey 'perspective-map))

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
for the perspective."
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

(defmacro with-perspective (name &rest body)
  "Switch to the perspective given by NAME while evaluating BODY."
  (declare (indent 1))
  (let ((old (cl-gensym)))
    `(progn
       (let ((,old (with-current-perspective (persp-current-name)))
             (last-persp-cache (persp-last)))
         (unwind-protect
             (progn
               (persp-switch ,name 'norecord)
               ,@body)
           (when ,old (persp-switch ,old 'norecord)))
         (set-frame-parameter nil 'persp--last last-persp-cache)))))

(defun persp-reset-windows ()
  "Remove all windows, ensure the remaining one has no window parameters.
This prevents the propagation of reserved window parameters like
window-side creating perspectives."
  (let ((ignore-window-parameters t))
    (delete-other-windows)
    (when (ignore-errors
            ;; Create a fresh window without any window parameters, the
            ;; selected window is still in a window that may have window
            ;; parameters we don't want.
            (split-window))
      ;; Delete the selected window so that the only window left has no window
      ;; parameters.
      (delete-window))))

(defun persp-new (name)
  "Return a new perspective with name NAME.
The new perspective will start with only an `initial-major-mode'
buffer called \"*scratch* (NAME)\"."
  (make-persp :name name
    (switch-to-buffer (persp-scratch-buffer name))
    (funcall initial-major-mode)
    (when initial-scratch-message
      (insert initial-scratch-message))
    (persp-reset-windows)))

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
    (let ((persp (gethash name (perspectives-hash))))
      (set-frame-parameter nil 'persp--last (persp-curr))
      (when (null persp)
        (setq persp (persp-new name)))
      (unless norecord
        (run-hooks 'persp-before-switch-hook))
      (persp-activate persp)
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

(defun persp-add-buffer (buffer)
  "Associate BUFFER with the current perspective.

See also `persp-switch' and `persp-remove-buffer'."
  (interactive
   (list
    (let ((read-buffer-function nil))
      (read-buffer "Add buffer to perspective: "))))
  (let ((buffer (get-buffer buffer)))
    (unless (persp-is-current-buffer buffer)
      (push buffer (persp-current-buffers)))))

(defun persp-set-buffer (buffer-name)
  "Associate BUFFER-NAME with the current perspective and remove it from any other."
  (interactive
   (list
    (let ((read-buffer-function nil))
      (read-buffer "Set buffer to perspective: "))))
  (cond ((get-buffer buffer-name)
         (persp-add-buffer buffer-name)
         (cl-loop for other-persp = (persp-buffer-in-other-p (get-buffer buffer-name))
                  while other-persp
                  do (with-perspective (cdr other-persp)
                                       (persp-remove-buffer buffer-name))))
        (t (message "buffer %s doesn't exist" buffer-name))))

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

(defun persp-remove-buffer (buffer)
  "Disassociate BUFFER with the current perspective.

See also `persp-switch' and `persp-add-buffer'."
  (interactive
   (list (funcall persp-interactive-completion-function "Remove buffer from perspective: " (persp-current-buffer-names))))
  (setq buffer (when buffer (get-buffer buffer)))
  (cond ((not (buffer-live-p buffer)))
        ;; Only kill the buffer if no other perspectives are using it
        ((not (persp-buffer-in-other-p buffer))
         (kill-buffer buffer))
        ;; Make the buffer go away if we can see it.
        ((get-buffer-window buffer)
         (let ((window (get-buffer-window buffer)))
           (while window
             (with-selected-window window (bury-buffer))
             (let ((new-window (get-buffer-window buffer)))
               ;; If `window' is still selected even after being buried, exit
               ;; the loop because otherwise it will go on infinitely.
               (setq window (unless (eq window new-window) new-window))))))
        (t (bury-buffer buffer)))
  (setf (persp-current-buffers) (remq buffer (persp-current-buffers))))

(defun persp-kill (name)
  "Kill the perspective given by NAME.

Killing a perspective means that all buffers associated with that
perspective and no others are killed."
  (interactive "i")
  (if (null name) (setq name (persp-prompt (persp-current-name) t)))
  (with-perspective name
    (run-hooks 'persp-killed-hook)
    (mapc 'persp-remove-buffer (persp-current-buffers))
    (setf (persp-killed (persp-curr)) t))
  (remhash name (perspectives-hash))
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
        (advice-add 'helm-buffer-list-1 :filter-return #'persp-buffer-list-filter)
        (add-hook 'after-make-frame-functions 'persp-init-frame)
        (add-hook 'delete-frame-functions 'persp-delete-frame)
        (add-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
        (setq read-buffer-function 'persp-read-buffer)
        (mapc 'persp-init-frame (frame-list))
        (setf (persp-current-buffers) (buffer-list))
        (run-hooks 'persp-mode-hook))
    (advice-remove 'helm-buffer-list-1 #'persp-buffer-list-filter)
    (ad-deactivate-regexp "^persp-.*")
    (remove-hook 'delete-frame-functions 'persp-delete-frame)
    (remove-hook 'after-make-frame-functions 'persp-init-frame)
    (remove-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
    (setq read-buffer-function nil)
    (set-frame-parameter nil 'persp--hash nil)
    (setq global-mode-string (delete '(:eval (persp-mode-line)) global-mode-string))
    (set-default 'header-line-format (delete '(:eval (persp-mode-line)) header-line-format))
    (unless (delete "" header-line-format)
      ;; need to set header-line-format to nil to completely remove the header from the buffer
      (set-default 'header-line-format nil))))

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
         (remq nil (mapcar 'buffer-name (persp-current-buffers))))
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

(defun persp-other-buffer (&optional buffer visible-ok frame)
  "A version of `other-buffer' which respects perspectives."
  (let ((other (other-buffer buffer visible-ok frame)))
    (if (member other (persp-current-buffers))
	other
      ;; In cases where `other-buffer' produces a buffer that is not
      ;; part of the current perspective, select the current
      ;; perspective's *scratch* buffer, similar to the behaviour of
      ;; `other-buffer'.
      (get-buffer-create (persp-scratch-buffer)))))


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
      (let* ((candidates (persp-current-buffer-names))
             (other (buffer-name (persp-other-buffer))))
        ;; NB: This intentionally calls completing-read instead of
        ;; persp-interactive-completion-function, since it is expected to have
        ;; been replaced by a completion framework.
        (completing-read (format "Switch to buffer%s: "
                                 (if other
                                     (format " (default %s)" other)
                                   ""))
                         candidates
                         nil nil nil nil
                         (buffer-name (persp-other-buffer)))))))
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
                       (persp-current-buffer-names)
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
       (list-buffers-noselect nil (seq-filter 'buffer-live-p (persp-current-buffers))))
    (switch-to-buffer (list-buffers-noselect))))

;; Buffer switching integration: list-buffers.
;;;###autoload
(defun persp-list-buffers (arg)
  "Like the default C-x C-b, but filters for the current perspective's buffers."
  (interactive "P")
  (if (and persp-mode (null arg))
      (display-buffer
       (list-buffers-noselect nil (seq-filter 'buffer-live-p (persp-current-buffers))))
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
                                            ,ignore-rx persp-buffer-filter nil))
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
                                                   (list #'persp-buffer-filter)
                                                   ido-ignore-buffers)))
        (ibuffer))
    (ibuffer)))

(defun persp--switch-buffer-ivy-counsel-helper (arg ivy-params fallback)
  (unless (featurep 'ivy)
    (user-error "Ivy not loaded"))
  (defvar ivy-switch-buffer-map)
  (declare-function ivy-read "ivy.el")
  (declare-function ivy-switch-buffer "ivy.el")
  (declare-function ivy--switch-buffer-matcher "ivy.el")
  (declare-function ivy--switch-buffer-action "ivy.el")
  (if (and persp-mode (null arg))
      (apply #'ivy-read
             (append
              (list
               (format "Switch to buffer (%s): " (persp-current-name))
               (cl-remove-if #'null (mapcar #'buffer-name
                                            ;; buffer-list is ordered by access time
                                            ;; seq-intersection keeps the order
                                            (seq-intersection (buffer-list)
                                                              (persp-current-buffers))))
               :preselect (buffer-name (persp-other-buffer (current-buffer)))
               :keymap ivy-switch-buffer-map
               :caller #'ivy-switch-buffer
               :action #'ivy--switch-buffer-action
               :matcher #'ivy--switch-buffer-matcher)
              ivy-params))
    (funcall fallback)))

;; Buffer switching integration: Ivy.
;;;###autoload
(defun persp-ivy-switch-buffer (arg)
  "A version of `ivy-switch-buffer' which respects perspectives."
  (interactive "P")
  (declare-function ivy-switch-buffer "ivy.el")
  (declare-function ivy--buffer-list "ivy.el")
  (let ((saved-ivy-buffer-list (symbol-function 'ivy--buffer-list))
        (temp-ivy-buffer-list (lambda (_str &optional _virtual _predicate)
                                (persp-current-buffer-names))))
    (unwind-protect
        (progn
          (when (and persp-mode (null arg))
            (setf (symbol-function 'ivy--buffer-list) temp-ivy-buffer-list))
          (persp--switch-buffer-ivy-counsel-helper arg nil #'ivy-switch-buffer))
      (setf (symbol-function 'ivy--buffer-list) saved-ivy-buffer-list))))

;; Buffer switching integration: Counsel.
;;;###autoload
(defun persp-counsel-switch-buffer (arg)
  "A version of `counsel-switch-buffer' which respects perspectives."
  (interactive "P")
  (unless (featurep 'counsel)
    (user-error "Counsel not loaded"))
  (declare-function counsel-switch-buffer "counsel.el")
  (declare-function counsel--switch-buffer-unwind "counsel.el")
  (declare-function counsel--switch-buffer-update-fn "counsel.el")
  (persp--switch-buffer-ivy-counsel-helper arg
                                           (list :unwind #'counsel--switch-buffer-unwind
                                                 :update-fn #'counsel--switch-buffer-update-fn)
                                           #'counsel-switch-buffer))


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
;;     }
;;   ]
;; }

(cl-defstruct persp--state-complete
  files
  frames)

(cl-defstruct persp--state-frame
  persps
  order)

(cl-defstruct persp--state-single
  buffers
  windows)

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
                       (make-persp--state-frame
                        :persps persps-in-frame
                        :order persp-names-in-order)))))

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

Frames are restored, along with each frame's perspective list.
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
        (state-complete (read
                         (with-temp-buffer
                           (insert-file-contents-literally file)
                           (buffer-string)))))
    ;; open all files in a temporary perspective to avoid polluting "main"
    (persp-switch tmp-persp-name)
    (cl-loop for file in (persp--state-complete-files state-complete) do
             (when (file-exists-p file)
               (find-file file)))
    ;; iterate over the frames
    (cl-loop for frame in (persp--state-complete-frames state-complete) do
             (cl-incf frame-count)
             (when (> frame-count 1)
               (make-frame-command))
             (let* ((frame-persp-table (persp--state-frame-persps frame))
                    (frame-persp-order (reverse (persp--state-frame-order frame))))
               ;; iterate over the perspectives in the frame in the appropriate order
               (cl-loop for persp in frame-persp-order do
                        (let ((state-single (gethash persp frame-persp-table)))
                          (persp-switch persp)
                          (cl-loop for buffer in (persp--state-single-buffers state-single) do
                                   (persp-add-buffer buffer))
                          ;; XXX: split-window-horizontally is necessary for
                          ;; window-state-put to succeed? Something goes haywire with root
                          ;; windows without it.
                          (split-window-horizontally)
                          (window-state-put (persp--state-single-windows state-single)
                                            (frame-root-window (selected-frame))
                                            'safe)))))
    ;; cleanup
    (persp-kill tmp-persp-name))
  ;; after hook
  (run-hooks 'persp-state-after-load-hook))

(defalias 'persp-state-restore 'persp-state-load)


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
