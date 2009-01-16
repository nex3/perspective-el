;; perspective.el --- switch between named "perspectives" of the editor
;; Copyright (C) 2008 Nathan Weizenbaum <nex342@gmail.com>
;;
;; Licensed under the same terms as Emacs.

(require 'cl)

;; This is only available in Emacs >23,
;; so we redefine it here for compatibility.
(unless (fboundp 'with-selected-frame)
  (defmacro with-selected-frame (frame &rest body)
    "Execute the forms in BODY with FRAME as the selected frame.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
    (declare (indent 1) (debug t))
    (let ((old-frame (make-symbol "old-frame"))
          (old-buffer (make-symbol "old-buffer")))
      `(let ((,old-frame (selected-frame))
             (,old-buffer (current-buffer)))
         (unwind-protect
             (progn (select-frame ,frame)
                    ,@body)
           (if (frame-live-p ,old-frame)
               (select-frame ,old-frame))
           (if (buffer-live-p ,old-buffer)
               (set-buffer ,old-buffer)))))))

(defstruct (perspective
            (:conc-name persp-)
            (:constructor make-persp-internal))
  name window-configuration buffers buffer-history)

(defvar persp-mode-map (make-sparse-keymap)
  "Keymap for perspective-mode.")

(define-prefix-command 'perspective 'perspective-map)
(define-key persp-mode-map (kbd "C-x x") perspective-map)

(define-key persp-mode-map (kbd "C-x x n") 'persp-new)
(define-key persp-mode-map (kbd "C-x x s") 'persp-switch)
(define-key persp-mode-map (kbd "C-x x k") 'persp-remove-buffer)
(define-key persp-mode-map (kbd "C-x x c") 'persp-kill)
(define-key persp-mode-map (kbd "C-x x r") 'persp-rename)
(define-key persp-mode-map (kbd "C-x x a") 'persp-add-buffer)
(define-key persp-mode-map (kbd "C-x x i") 'persp-import)

;; make-variable-frame-local is obsolete according to the docs,
;; but I don't want to have to manually munge frame-parameters
;; all the time so I'm using it anyway.
(make-variable-frame-local
 (defvar perspectives-hash nil
   "A hash containing all perspectives. The keys are the
perspetives' names. The values are persp structs,
with the fields NAME, WINDOW-CONFIGURATION, BUFFERS,
and BUFFER-HISTORY.

NAME is the name of the perspective.

WINDOW-CONFIGURATION is the configuration given by
`current-window-configuration' last time the perspective was
saved (if this isn't the current perspective, this is when the
perspective was last active).

BUFFERS is a list of buffer objects that are associated with this
perspective.

BUFFER-HISTORY is the list of buffer history values for this
perspective."))

(make-variable-frame-local
 (defvar persp-curr nil
   "The current perspective."))

(make-variable-frame-local
 (defvar persp-recursive nil
   "The current perspective before beginning a recursive edit."))

(make-variable-frame-local
 (defvar persp-last nil
   "The last perspective accessed before the current perspective."))

(make-variable-frame-local
 (defvar persp-modestring nil
   "The string displayed in the modeline representing the perspectives."))
(put 'persp-modestring 'risky-local-variable t)

(defvar persp-show-modestring t
  "Determines if `persp-modestring' is shown in the modeline.")

(defface persp-selected-face
  '((t (:weight bold :foreground "Blue")))
  "The face used to highlight the current perspective on the modeline.")

(defun make-persp (&rest args)
  "Create a new perspective struct and put it in `perspectives-hash'."
  (let ((persp (apply 'make-persp-internal args)))
    (puthash (persp-name persp) persp perspectives-hash)
    persp))

(defun persp-save ()
  "Save the current window configuration to `persp-curr'"
  (when persp-curr
    (setf (persp-buffer-history persp-curr) buffer-name-history)
    (setf (persp-window-configuration persp-curr) (current-window-configuration))))

(defun persp-names ()
  "Return a list of the names of all perspectives, sorted alphabetically."
  (sort
   (loop for name being the hash-keys of perspectives-hash
         collect name)
   'string<))

(defun persp-all-names (&optional not-frame)
  "Return a list of the perspective names for all frames
except NOT-FRAME (if passed)."
  (reduce 'union
          (mapcar
           (lambda (frame)
             (unless (equal frame not-frame)
               (with-selected-frame frame (persp-names))))
           (frame-list))))

(defun persp-prompt (&optional default require-match)
  "Prompt for the name of a perspective.

DEFAULT is a default value for the prompt.

REQUIRE-MATCH can take the same values as in `completing-read'."
  (completing-read (concat "Perspective name"
                           (if default (concat " (default " default ")") "")
                           ": ")
                   (persp-names)
                   nil require-match nil nil default))

(defmacro with-perspective (name &rest body)
  "Evaluate BODY with the perspective given by NAME as the current perspective."
  (declare (indent 1))
  `(let ((persp-curr (gethash ,name perspectives-hash)))
     ,@body))

(defun persp-new (name)
  "Save the current perspective, create a new perspective with
name NAME, and switch to the new perspective.

The new perspective initially has only one buffer: a
`initial-major-mode' buffer called \"*scratch* (NAME)\"."
  (interactive "sNew perspective: \n")
  (persp-save)
  (setq persp-curr (make-persp :name name))
  (switch-to-buffer (concat "*scratch* (" name ")"))
  (funcall initial-major-mode)
  (delete-other-windows)
  (persp-update-modestring))

(defun persp-reactivate-buffers (buffers)
  "\"Reactivate\" BUFFERS by raising them to the top of the
most-recently-selected list. The result is BUFFERS with all
non-living buffers removed.

See also `other-buffer'."
  (loop for buf in (reverse buffers)
        if (not (null (buffer-name buf)))
          collect buf into living-buffers
          and do (switch-to-buffer buf)
        finally return (reverse living-buffers)))

(defun persp-intersperse (list interspersed-val)
  "Insert VAL between every pair of items in LIST and return the resulting list.

For example, (persp-intersperse '(1 2 3) 'a) gives '(1 a 2 a 3)."
  (reverse
   (reduce
    (lambda (list el) (if list (list* el interspersed-val list) (list el)))
    list :initial-value nil)))

(defconst persp-mode-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] 'persp-mode-line-click)
    map))

(defun persp-mode-line-click (event)
  "Select the clicked perspective."
  (interactive "e")
  (persp-switch (format "%s" (car (posn-string (event-start event))))))

(defun persp-update-modestring ()
  "Update `persp-modestring' to reflect the current
perspectives. Has no effect when `persp-show-modestring' is nil."
  (when persp-show-modestring
    (setq persp-modestring
          (append '("[")
                  (persp-intersperse (mapcar 'persp-format-name (persp-names)) "|")
                  '("]")))))

(defun persp-format-name (name)
  "Format the perspective name given by NAME for display in `persp-modestring'."
  (let ((string-name (format "%s" name)))
    (if (equal name (persp-name persp-curr))
        (propertize string-name 'face 'persp-selected-face)
      (propertize string-name
                  'local-map persp-mode-line-map
                  'mouse-face 'mode-line-highlight))))

(defun persp-get-quick (char &optional prev)
  "Returns the name of the first perspective, alphabetically,
that begins with CHAR.

PREV can be the name of a perspective. If it's passed,
this will try to return the perspective alphabetically after PREV.
This is used for cycling between perspectives."
  (persp-get-quick-helper char prev (persp-names)))

(defun persp-get-quick-helper (char prev names)
  "A helper method for `persp-get-quick'."
  (if (null names) nil
    (let ((name (car names)))
      (cond
       ((and (null prev) (eq (string-to-char name) char)) name)
       ((equal name prev)
        (if (and (not (null (cdr names))) (eq (string-to-char (cadr names)) char))
            (cadr names)
          (persp-get-quick char)))
       (t (persp-get-quick-helper char prev (cdr names)))))))

(defun persp-switch (name)
  "Switch to the perspective given by NAME. If it doesn't exist,
create a new perspective and switch to that.

Switching to a perspective means that all buffers associated with
that perspective are reactivated (see `persp-reactivate-buffers')
and the perspective's window configuration is restored."
  (interactive "i")
  (if (null name) (setq name (persp-prompt (and persp-last (persp-name persp-last)))))
  (if (equal name (persp-name persp-curr)) name
    (let ((persp (gethash name perspectives-hash)))
      (setq persp-last persp-curr)
      (if (null persp) (persp-new name)
        (persp-save)
        (persp-activate persp))
      (persp-update-modestring)
      name)))

(defun persp-activate (persp)
  "Activate the perspective given by the persp struct PERSP."
  (persp-save)
  (setq persp-curr persp)
  (persp-reactivate-buffers (persp-buffers persp))
  (setq buffer-name-history (persp-buffer-history persp))
  (set-window-configuration (persp-window-configuration persp)))

(defun persp-switch-quick (char)
  "Switches to the first perspective, alphabetically, that begins with CHAR.

Sets `this-command' (and thus `last-command') to (persp-switch-quick . CHAR).

See `persp-switch', `persp-get-quick'."
  (interactive "c")
  (let ((persp (if (and (consp last-command) (eq (car last-command) this-command))
                   (persp-get-quick char (cdr last-command))
                 (persp-get-quick char))))
    (setq this-command (cons this-command persp))
    (if persp (persp-switch persp)
      (error (concat "No perspective name begins with " (string char))))))

(defun persp-find-some ()
  "Returns the name of a valid perspective.

This function tries to return the \"most appropriate\"
perspective to switch to. It tries:

  * The perspective given by `persp-last'.
  * The main perspective.
  * The first existing perspective, alphabetically.

If none of these perspectives can be found, this function will
create a new main perspective and return \"main\"."
  (cond
   (persp-last (persp-name persp-last))
   ((gethash "main" perspectives-hash) "main")
   ((> (hash-table-count perspectives-hash) 0) (car (persp-names)))
   (t (setq persp-curr (make-persp :name "main" :buffers (buffer-list)))
      (persp-save)
      (persp-update-modestring)
      "main")))

(defun persp-add-buffer (buffer)
  "Associate BUFFER with the current perspective.

See also `persp-switch' and `persp-remove-buffer'."
  (interactive
   (let ((read-buffer-function nil))
     (read-buffer "Add buffer to perspective: ")))
  (let ((buffer (get-buffer buffer)))
    (unless (memq buffer (persp-buffers persp-curr))
      (push buffer (persp-buffers persp-curr)))))

(defun* persp-buffer-in-other-p (buffer)
  "Returns nil if BUFFER is only in the current perspective.
Otherwise, returns (FRAME . NAME), the frame and name of another
perspective that has the buffer."
  (loop for frame in (frame-list)
        do (loop for persp being the hash-values of (with-selected-frame frame perspectives-hash)
                 if (and (not (and (equal frame (selected-frame))
                                   (equal (persp-name persp) (persp-name persp-curr))))
                         (memq buffer (persp-buffers persp)))
                   do (return-from persp-buffer-in-other-p
                        (cons frame (persp-name persp)))))
  nil)

(defun persp-remove-buffer (buffer)
  "Disassociate BUFFER with the current perspective.

See also `persp-switch' and `persp-add-buffer'."
  (interactive "bRemove buffer from perspective: \n")
  (setq buffer (get-buffer buffer))
  ; Only kill the buffer if no other perspectives are using it
  (cond ((not (persp-buffer-in-other-p buffer))
         (kill-buffer buffer))
        ;; Make the buffer go away if we can see it.
        ;; TODO: Is it possible to tell if it's visible at all,
        ;;       rather than just the current buffer?
        ((eq buffer (current-buffer)) (bury-buffer))
        (t (bury-buffer buffer)))
  (setf (persp-buffers persp-curr) (remq buffer (persp-buffers persp-curr))))

(defun persp-kill (name)
  "Kill the perspective given by NAME.

Killing a perspective means that all buffers associated with that
perspective and no others are killed."
  (interactive "i")
  (if (null name) (setq name (persp-prompt (persp-name persp-curr) t)))
  (with-perspective name
    (mapcar 'persp-remove-buffer (persp-buffers persp-curr)))
  (remhash name perspectives-hash)
  (persp-update-modestring)
  (when (equal name (persp-name persp-last))
    (setq persp-last nil))
  (when (equal name (persp-name persp-curr))
    (persp-switch (persp-find-some))))

(defun persp-rename (name)
  "Rename the current perspective to NAME."
  (interactive "sNew name: ")
  (if (gethash name perspectives-hash)
      (error "Perspective `%s' already exists" name)
    (remhash (persp-name persp-curr) perspectives-hash)
    (puthash name persp-curr perspectives-hash)
    (setf (persp-name persp-curr) name)
    (persp-update-modestring)))

(defun* persp-all-get (name not-frame)
  "Returns the list of buffers for a perspective named NAME from any
frame other than NOT-FRAME.

This doesn't return the window configuration because those can't be
copied across frames."
  (dolist (frame (frame-list))
    (unless (equal frame not-frame)
      (with-selected-frame frame
        (let ((persp (gethash name perspectives-hash)))
          (if persp (return-from persp-all-get (persp-buffers persp))))))))

(defun persp-read-buffer (prompt &optional def require-match)
  "A replacement for the built-in `read-buffer', meant to be used with
`read-buffer-function'. Return the name of the buffer selected, only
selecting from buffers within the current perspective.

With a prefix arg, uses the old `read-buffer' instead."
  (let ((read-buffer-function nil))
    (if current-prefix-arg
        (read-buffer prompt def require-match)
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
              (read-buffer prompt def require-match))
          (remove-hook 'minibuffer-setup-hook persp-read-buffer-hook))))))

(defun persp-complete-buffer ()
  "Perform completion on all buffers within the current perspective."
  (lexical-let ((persp-names (mapcar 'buffer-name (persp-buffers persp-curr))))
    (apply-partially 'completion-table-with-predicate
                     (or minibuffer-completion-table 'internal-complete-buffer)
                     (lambda (name)
                       (member (if (consp name) (car name) name) persp-names))
                     nil)))

(defun* persp-import (name &optional dont-switch)
  "Import a perspective named NAME from another frame.  If DONT-SWITCH
is non-nil or with prefix arg, don't switch to the new perspective."
  ;; TODO: Have some way of selecting which frame the perspective is imported from.
  (interactive "i\nP")
  (unless name
    (setq name (completing-read "Import perspective: " (persp-all-names (selected-frame)) nil t)))
  (if (and (gethash name perspectives-hash)
           (not (yes-or-no-p (concat "Perspective `" name "' already exits. Continue? "))))
      (return-from persp-import))
  (let ((buffers (persp-all-get name (selected-frame)))
        persp)
    (if (null buffers)
        (error "Perspective `%s' doesn't exist in another frame." name))
    (save-excursion
      (save-window-excursion
        (switch-to-buffer (car buffers))
        (delete-other-windows)
        (setq persp
              (make-persp :name name :buffers buffers
                          :window-configuration (current-window-configuration)))))
    (unless dont-switch
      (persp-save)
      (persp-activate persp))
    (persp-update-modestring)))

(defadvice switch-to-buffer (after persp-add-buffer-adv)
  "Add BUFFER to the current perspective.

See also `persp-add-buffer'."
  ;; The relevant argument is named BUFFER in Emacs <23 and BUFFER-OR-NAME in Emacs >23
  (persp-add-buffer (or (bound-and-true-p buffer) buffer-or-name)))

(defadvice recursive-edit (around persp-preserve-for-recursive-edit)
  "Preserve the current perspective when entering a recursive edit."
  (persp-save)
  (let ((persp-recursive-name (persp-name persp-curr))
        (old-hash (copy-hash-table perspectives-hash)))
    ad-do-it
    ;; We want the buffer lists that were created in the recursive edit,
    ;; but not the window configurations
    (maphash (lambda (key new-persp)
               (let ((persp (gethash key old-hash)))
                 (when persp
                   (setf (persp-buffers persp) (persp-buffers new-persp)))))
             perspectives-hash)
    (setq perspectives-hash old-hash)))

(defadvice exit-recursive-edit (before persp-restore-after-recursive-edit)
  "Restore the old perspective when exiting a recursive edit."
  (if persp-recursive-name (persp-switch (persp-name persp-recursive))))

;;;###autoload
(define-minor-mode persp-mode
  "Toggle perspective mode.
When active, keeps track of multiple 'perspectives',
named collections of buffers and window configurations."
  :global t
  :keymap persp-mode-map
  (if persp-mode
      (progn
        (ad-activate 'switch-to-buffer)
        (ad-activate 'recursive-edit)
        (ad-activate 'exit-recursive-edit)
        (add-hook 'after-make-frame-functions 'persp-init-frame)
        (add-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
        (setq read-buffer-function 'persp-read-buffer)

        (persp-init-frame (selected-frame))
        (setf (persp-buffers persp-curr) (buffer-list)))
    (ad-deactivate-regexp "^persp-.*")
    (remove-hook 'after-make-frame-functions 'persp-init-frame)
    (remove-hook 'ido-make-buffer-list-hook 'persp-set-ido-buffers)
    (setq read-buffer-function nil)
    (setq perspectives-hash nil)
    (setq global-mode-string (delq 'persp-modestring global-mode-string))))

(defun persp-init-frame (frame)
  "Initialize the perspectives system in FRAME
\(by default, the current frame)."
  (with-selected-frame frame
    (modify-frame-parameters
     frame
     '((perspectives-hash) (persp-curr) (persp-last) (persp-recursive) (persp-modestring)))

    ;; Don't set these variables in modify-frame-parameters
    ;; because that won't do anything if they've already been accessed
    (setq perspectives-hash (make-hash-table :test 'equal :size 10))
    (setq persp-curr (make-persp :name "main" :buffers (list (current-buffer))))

    (when persp-show-modestring
      (setq global-mode-string (or global-mode-string '("")))
      (unless (memq 'persp-modestring global-mode-string)
        (setq global-mode-string (append global-mode-string '(persp-modestring))))
      (persp-update-modestring))))

(defun persp-set-ido-buffers ()
  (setq ido-temp-list
        (let ((names (remq nil (mapcar 'buffer-name (persp-buffers persp-curr)))))
          (or (remove-if (lambda (name) (eq (string-to-char name) ? )) names) names))))

(defun quick-perspective-keys ()
  "Binds all C-S-letter key combinations to switch to the first
perspective beginning with the given letter."
  (loop for c from ?a to ?z
        do (define-key persp-mode-map
            (read-kbd-macro (concat "C-S-" (string c)))
            `(lambda ()
               (interactive)
               (persp-switch-quick ,c)))))

(defun persp-turn-off-modestring ()
  (interactive)
  (setq persp-modestring nil)
  (setq persp-show-modestring nil))

(defun persp-turn-on-modestring ()
  (interactive)
  (setq persp-show-modestring t)
  (persp-update-modestring))

(provide 'perspective)
