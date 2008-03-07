;; perspective.el --- switch between named "perspectives" of the editor
;; Copyright (C) 2008 Nathan Weizenbaum <nex342@gmail.com>
;; Licensed under the same terms as Emacs.

(eval-when-compile (require 'cl))

(defvar perspectives-hash (make-hash-table :test 'equal :size 10)
  "A hash containing all perspectives. The keys are the
perspetives' names. The values are of the
form (WINDOW-CONFIGURATION BUFFERS).

WINDOW-CONFIGURATION is the configuration given by
`current-window-configuration' last time the perspective was
saved (if this isn't the current perspective, this is when the
perspective was last active).

BUFFERS is a list of buffer objects that are associated with this
perspective.")

(defvar persp-curr-name nil
  "The name of the current perspective.")

(defvar persp-curr-buffers nil
  "A list of buffers associated with the current perspective.")
(defvar persp-last-name nil)

(defvar persp-modestring nil
  "The string displayed in the modeline representing the perspectives.")
(put 'persp-modestring 'risky-local-variable t)

(defface persp-selected-face
  '((default (:weight bold :foreground "Blue")))
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
  (let ((buffer (switch-to-buffer (concat "*scratch* (" name ")"))))
    (lisp-interaction-mode)
    (delete-other-windows)
    (setq persp-curr-name name)
    (setq persp-curr-buffers (list buffer))
    (persp-save)))

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

(defun persp-update-modestring ()
  "Update `persp-modestring' to reflect the current perspectives."
  (setq persp-modestring
        (append '("[")
                (persp-intersperse (mapcar 'persp-format-name (persp-names)) "|")
                '("]"))))

(defun persp-format-name (name)
  "Format the perspective name given by NAME for display in `persp-modestring'."
  (if (not (equal name persp-curr-name)) name
    (let ((name (concat name)))
      (add-text-properties 0 (length name) '(face persp-selected-face) name)
      name)))

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
  (push (get-buffer buffer) persp-curr-buffers))

(defun persp-remove-buffer (buffer)
  "Disassociate BUFFER with the current perspective.

See also `persp-switch' and `persp-add-buffer'."
  (interactive "bRemove buffer from perspective: \n")
  (setq buffer (get-buffer buffer))
  ; Only kill the buffer if no other perspectives are using it
  (cond ((loop for persp being the hash-values of perspectives-hash using (hash-keys name)
               unless (equal name persp-curr-name)
               if (memq buffer (cadr persp)) return nil
               finally return t)
         (kill-buffer buffer))
        ;; Make the buffer go away if we can see it.
        ;; TODO: Is it possible to tell if it's visible at all,
        ;;       rather than just the current buffer?
        ((eq buffer (current-buffer)) (bury-buffer))
        (t (bury-buffer buffer)))
  (setq persp-curr-buffers (remq buffer persp-curr-buffers)))

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

(defadvice switch-to-buffer (after persp-add-buffer-adv)
  "Add BUFFER to the current perspective.

See also `persp-add-buffer'."
  (persp-add-buffer buffer))

(defun persp-init ()
  "Initialize the perspectives system."
  (setq persp-curr-name "main")
  (setq persp-curr-buffers (buffer-list))
  (persp-save)
  (ad-activate 'switch-to-buffer)

  (setq global-mode-string (or global-mode-string '("")))
  (if (not (memq 'persp-modestring global-mode-string))
      (setq global-mode-string (append global-mode-string '(persp-modestring))))
  (persp-update-modestring))


(define-prefix-command 'perspective 'perspective-map)
(global-set-key (read-kbd-macro "C-x x") perspective-map)

(global-set-key (read-kbd-macro "C-x x n") 'persp-new)
(global-set-key (read-kbd-macro "C-x x s") 'persp-switch)
(global-set-key (read-kbd-macro "C-x x r") 'persp-remove-buffer)
(global-set-key (read-kbd-macro "C-x x k") 'persp-kill)

(if (null persp-curr-name)
    (persp-init))

(provide 'perspective)
