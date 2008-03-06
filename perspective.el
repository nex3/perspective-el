;; perspective.el --- switch between named "perspectives" of the editor
;; Copyright (C) 2008 Nathan Weizenbaum <nex342@gmail.com>
;; Licensed under the same terms as Emacs.

(eval-when-compile (require 'cl))

(defvar perspectives-hash (make-hash-table :test 'equal :size 10))

(defvar persp-curr-name "main")
(defvar persp-curr-buffers (buffer-list))

(defun persp-names ()
  (loop for name being the hash-keys of perspectives-hash
      collect name))

(defun persp-save ()
  (puthash persp-curr-name
           (list (current-window-configuration) (persp-remove-dups persp-curr-buffers))
           perspectives-hash))

(defun persp-new (name)
  (interactive "sNew perspective: \n")
  (persp-save)
  (let ((buffer (switch-to-buffer (concat "*scratch* (" name ")"))))
    (lisp-interaction-mode)
    (delete-other-windows)
    (setq persp-curr-name name)
    (setq persp-curr-buffers (list buffer))
    name))

(defun persp-remove-dups (list &optional test)
  (let ((seen (make-hash-table :test (or test 'equal))))
    (loop for item in list
          if (not (gethash item seen))
            collect item into result
            and do (puthash item t seen)
          finally return result)))

(defun persp-reactivate-buffers (buffers)
  (loop for buf in (reverse buffers)
        if (not (null (buffer-name buf)))
          collect buf into living-buffers
          and do (switch-to-buffer buf)
        finally return (reverse living-buffers)))

(defun persp-switch (name)
  (interactive "i")
  (if (null name)
      (setq name (completing-read "Perspective name: " (persp-names))))
  (if (equal name persp-curr-name) name
    (let ((persp (gethash name perspectives-hash)))
      (if (null persp) (persp-new name)
        (persp-save)
        (setq persp-curr-name name)
        (setq persp-curr-buffers (persp-reactivate-buffers (cadr persp)))
        (set-window-configuration (car persp))
        name))))

(defun persp-add-buffer (buffer)
  (interactive "bAdd buffer to perspective: \n")
  (push (get-buffer buffer) persp-curr-buffers))

(defadvice switch-to-buffer (after persp-add-buffer-adv)
  (persp-add-buffer buffer))
(ad-activate 'switch-to-buffer)

(define-prefix-command 'perspective 'perspective-map)
(global-set-key (read-kbd-macro "C-S-s") perspective-map)

(global-set-key (read-kbd-macro "C-S-s n") 'persp-new)
(global-set-key (read-kbd-macro "C-S-s s") 'persp-switch)
