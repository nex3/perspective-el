;; perspective.el --- switch between named "perspectives" of the editor
;; Copyright (C) 2008 Nathan Weizenbaum <nex342@gmail.com>
;;
;; Licensed under the same terms as Emacs.

(eval-when-compile (require 'cl))

(defvar persp-initialized nil
  "Non-nil if the perspectives system has been initialized.")

;; make-variable-frame-local is obsolete according to the docs,
;; but I don't want to have to manually munge frame-parameters
;; all the time so I'm using it anyway.
(make-variable-frame-local
 (defvar perspectives-hash nil
   "A hash containing all perspectives. The keys are the
perspetives' names. The values are of the
form (WINDOW-CONFIGURATION BUFFERS).

WINDOW-CONFIGURATION is the configuration given by
`current-window-configuration' last time the perspective was
saved (if this isn't the current perspective, this is when the
perspective was last active).

BUFFERS is a list of buffer objects that are associated with this
perspective."))

(make-variable-frame-local
 (defvar persp-curr-name nil
   "The name of the current perspective."))

(make-variable-frame-local
 (defvar persp-recursive-name nil
   "The name of the current perspective before beginning a recursive edit."))

(make-variable-frame-local
 (defvar persp-curr-buffers nil
   "A list of buffers associated with the current perspective."))
(make-variable-frame-local (defvar persp-last-name nil))

(make-variable-frame-local
 (defvar persp-modestring nil
   "The string displayed in the modeline representing the perspectives."))
(put 'persp-modestring 'risky-local-variable t)

(defvar persp-show-modestring t
  "Determines if `persp-modestring' is shown in the modeline.")

(defface persp-selected-face
  '((t (:weight bold :foreground "Blue")))
  "The face used to highlight the current perspective on the modeline.")

(defun persp-save ()
  "Save the current perspective in `perspectives-hash'."
  (if persp-curr-name
      (puthash persp-curr-name
               (list (current-window-configuration) (persp-remove-dups persp-curr-buffers))
               perspectives-hash)))

(defun persp-names ()
  "Return a list of the names of all perspectives, sorted alphabetically."
  (sort
   (loop for name being the hash-keys of perspectives-hash
         collect name)
   'string<))

(defun persp-union (&rest lists)
  "Returns the union of each sublist of LISTS."
  (loop for l on lists
        append (if (null (cdr l)) (car l)
                 (let ((list1 (car l)) (list2 (cadr l)))
                   (loop for el in list1
                         unless (member el list2) collect el)))))

(defun persp-all-names (&optional not-frame)
  "Return a list of the perspective names for all frames
except NOT-FRAME (if passed)."
  (apply 'persp-union
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
  `(let ((persp-curr-name ,name)
         (persp-curr-buffers (cadr (gethash ,name perspectives-hash))))
     ,@body))

(defun persp-new (name)
  "Save the current perspective, create a new perspective with
name NAME, and switch to the new perspective.

The new perspective initially has only one buffer: a
Lisp-interaction buffer called \"*scratch* (NAME)\"."
  (interactive "sNew perspective: \n")
  (persp-save)
  (setq persp-curr-name name)
  (setq persp-curr-buffers nil)
  (let ((buffer (switch-to-buffer (concat "*scratch* (" name ")"))))
    (lisp-interaction-mode)
    (delete-other-windows))
  (persp-update-modestring))

(defun persp-remove-dups (list &optional test)
  "Remove duplicate items from LIST.

TEST is a hash table test used to determine if two elements are
equal. It defaults to `equal', but can also be set to `eq',
`eql', or a test defined by `define-hash-table-test'.

For example, (persp-remove-dups '(1 2 1 3 2 4 3 5)) gives '(1 2 3 4 5)."
  (let ((seen (make-hash-table :test (or test 'equal))))
    (loop for item in list
          if (not (gethash item seen))
            collect item into result
            and do (puthash item t seen)
          finally return result)))

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

(defun persp-intersperse (list val)
  "Insert VAL between every pair of items in LIST and return the resulting list.

For example, (persp-intersperse '(1 2 3) 'a) gives '(1 a 2 a 3)."
  (if (or (null list) (null (cdr list))) list
    (cons (car list)
          (cons val
                (persp-intersperse (cdr list) val)))))

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
    (if (equal name persp-curr-name)
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
  (if (null name) (setq name (persp-prompt persp-last-name)))
  (if (equal name persp-curr-name) name
    (let ((persp (gethash name perspectives-hash)))
      (setq persp-last-name persp-curr-name)
      (if (null persp) (persp-new name)
        (persp-save)
        (setq persp-curr-name name)
        (setq persp-curr-buffers (persp-reactivate-buffers (cadr persp)))
        (set-window-configuration (car persp)))
      (persp-update-modestring)
      name)))

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

  * The perspective given by `persp-last-name'.
  * The main perspective.
  * The first existing perspective, alphabetically.

If none of these perspectives can be found, this function will
create a new main perspective and return \"main\"."
  (cond
   (persp-last-name persp-last-name)
   ((gethash "main" perspectives-hash) "main")
   ((> (hash-table-count perspectives-hash) 0) (car (persp-names)))
   (t (progn
        (setq persp-curr-name "main")
        (setq persp-curr-buffers (buffer-list))
        (persp-save)
        (persp-update-modestring)
        "main"))))

(defun persp-add-buffer (buffer)
  "Associate BUFFER with the current perspective.

See also `persp-switch' and `persp-remove-buffer'."
  (interactive "bAdd buffer to perspective: \n")
  (push (get-buffer buffer) persp-curr-buffers)
  (persp-save))

(defun* persp-buffer-in-other-p (buffer)
  "Returns nil if BUFFER is only in the current perspective.
Otherwise, returns (FRAME . NAME), the frame and name of another
perspective that has the buffer."
  (loop for frame in (frame-list)
        do (loop for persp being the hash-values of (with-selected-frame frame perspectives-hash)
                   using (hash-keys name)
                 if (and (not (and (equal frame (selected-frame))
                                   (equal name persp-curr-name)))
                         (memq buffer (cadr persp)))
                   do (return-from persp-buffer-in-other-p (cons frame name))))
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
  (setq persp-curr-buffers (remq buffer persp-curr-buffers))
  (persp-save))

(defun persp-kill (name)
  "Kill the perspective given by NAME.

Killing a perspective means that all buffers associated with that
perspective and no others are killed."
  (interactive "i")
  (if (null name) (setq name (persp-prompt persp-curr-name t)))
  (with-perspective name
    (mapcar 'persp-remove-buffer persp-curr-buffers))
  (setq persp-curr-name nil)
  (setq persp-last-name nil)
  (remhash name perspectives-hash)
  (persp-switch (persp-find-some)))

(defun persp-rename (name)
  "Rename the current perspective to NAME."
  (interactive "sNew name: ")
  (if (gethash name perspectives-hash)
      (error (concat "Perspective " name " already exists"))
    (remhash persp-curr-name perspectives-hash)
    (setq persp-curr-name name)
    (persp-save)
    (persp-update-modestring)))

(defun* persp-all-get (name &optional not-frame)
  "Returns the list of buffers for a perspective named NAME from any
frame other than NOT-FRAME.

This doesn't return the window configuration because those can't be
copied across frames."
  (dolist (frame (frame-list))
    (unless (equal frame not-frame)
      (with-selected-frame frame
        (persp-save)
        (let ((persp (gethash name perspectives-hash)))
          (if persp (return-from persp-all-get (cadr persp))))))))

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
  (let ((buffers (persp-all-get name (selected-frame))))
    (if (null buffers)
        (error "Perspective `%s' doesn't exist in another frame." name))
    (save-excursion
      (save-window-excursion
        (switch-to-buffer (car buffers))
        (delete-other-windows)
        (puthash name (list (current-window-configuration) buffers) perspectives-hash)))
    (persp-update-modestring)
    (unless dont-switch (persp-switch name))))

(defadvice switch-to-buffer (after persp-add-buffer-adv)
  "Add BUFFER to the current perspective.

See also `persp-add-buffer'."
  ;; The relevant argument is named BUFFER in Emacs <23 and BUFFER-OR-NAME in Emacs >23
  (persp-add-buffer (or (bound-and-true-p buffer) buffer-or-name)))

(defadvice recursive-edit (around persp-preserve-for-recursive-edit)
  "Preserve the current perspective when entering a recursive edit."
  (persp-save)
  (let ((persp-recursive-name persp-curr-name) (old-hash (copy-hash-table perspectives-hash)))
    ad-do-it
    ;; We want the buffer lists that were created in the recursive edit,
    ;; but not the window configurations
    (maphash (lambda (key val) 
               (let ((persp (gethash key old-hash)))
                 (if (not persp) (setcdr persp (cdr val))
                   (puthash key val old-hash))))
             perspectives-hash)
    (setq perspectives-hash old-hash)))

(defadvice exit-recursive-edit (before persp-restore-after-recursive-edit)
  "Restore the old perspective when exiting a recursive edit."
  (if persp-recursive-name (persp-switch persp-recursive-name)))

(defun persp-init ()
  "Initialize the perspectives system."
  (interactive)
  (ad-activate 'switch-to-buffer)
  (ad-activate 'recursive-edit)
  (ad-activate 'exit-recursive-edit)
  (add-hook 'after-make-frame-functions 'persp-init-frame)

  (persp-init-frame (selected-frame))
  (setq persp-curr-buffers (buffer-list))
  (setq persp-initialized t))

(defun persp-init-frame (frame)
  "Initialize the perspectives system in FRAME
\(by default, the current frame)."
  (with-selected-frame frame
    (modify-frame-parameters
     frame
     `((perspectives-hash) (persp-curr-name) (persp-curr-buffers) (persp-recursive-name) (persp-modestring)))

    ;; Don't set these variables in modify-frame-parameters
    ;; because that won't do anything if they've already been accessed
    (setq perspectives-hash (make-hash-table :test 'equal :size 10))
    (setq persp-curr-name "main")
    (setq persp-curr-buffers (list (current-buffer)))
    (persp-save)

    (when persp-show-modestring
      (setq global-mode-string (or global-mode-string '("")))
      (unless (memq 'persp-modestring global-mode-string)
        (setq global-mode-string (append global-mode-string '(persp-modestring))))
      (persp-update-modestring))))

(defun quick-perspective-keys ()
  "Binds all C-S-letter key combinations to switch to the first
perspective beginning with the given letter."
  (loop for c from ?a to ?z
        do (global-set-key
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

(define-prefix-command 'perspective 'perspective-map)
(global-set-key (read-kbd-macro "C-x x") perspective-map)

(global-set-key (read-kbd-macro "C-x x n") 'persp-new)
(global-set-key (read-kbd-macro "C-x x s") 'persp-switch)
(global-set-key (read-kbd-macro "C-x x k") 'persp-remove-buffer)
(global-set-key (read-kbd-macro "C-x x c") 'persp-kill)
(global-set-key (read-kbd-macro "C-x x r") 'persp-rename)
(global-set-key (read-kbd-macro "C-x x i") 'persp-import)

(unless persp-initialized
  (persp-init))

(provide 'perspective)
