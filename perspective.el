;; perspective.el --- switch between named "perspectives" of the editor
;; Copyright (C) 2008 Nathan Weizenbaum <nex342@gmail.com>
;; Licensed under the same terms as Emacs.

(eval-when-compile (require 'cl))

(defvar perspectives-hash (make-hash-table :test 'equal :size 10))

(defvar persp-curr-name "main")
(defvar persp-curr-buffers nil)
(defvar persp-last-name nil)

(defvar persp-modestring)
(put 'persp-modestring 'risky-local-variable t)

(defface persp-selected-face
  '((default (:weight bold :foreground "Blue")))
  "The face used to highlight the current perspective on the modeline.")

(defun persp-save ()
  (puthash persp-curr-name
           (list (current-window-configuration) (persp-remove-dups persp-curr-buffers))
           perspectives-hash))

(defun persp-names ()
  (sort
   (loop for name being the hash-keys of perspectives-hash
         collect name)
   'string<))

(defun persp-new (name)
  (interactive "sNew perspective: \n")
  (persp-save)
  (let ((buffer (switch-to-buffer (concat "*scratch* (" name ")"))))
    (lisp-interaction-mode)
    (delete-other-windows)
    (setq persp-curr-name name)
    (setq persp-curr-buffers (list buffer))
    (persp-save)))

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

(defun persp-intersperse (list val)
  (if (or (null list) (null (cdr list))) list
    (cons (car list)
          (cons val
                (persp-intersperse (cdr list) val)))))

(defun persp-update-modestring ()
  (setq persp-modestring
        (append '("[")
                (persp-intersperse (mapcar 'persp-format-name (persp-names)) "|")
                '("]"))))

(defun persp-format-name (name)
  (if (not (equal name persp-curr-name)) name
    (let ((name (concat name)))
      (add-text-properties 0 (length name) '(face persp-selected-face) name)
      name)))

(defun persp-switch (name)
  (interactive "i")
  (if (null name) (setq name (completing-read "Perspective name: " (persp-names)
                                              nil nil persp-last-name)))
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

(defun persp-add-buffer (buffer)
  (interactive "bAdd buffer to perspective: \n")
  (push (get-buffer buffer) persp-curr-buffers))

(defun persp-remove-buffer (buffer)
  (interactive "bRemove buffer from perspective: \n")
  (setq buffer (get-buffer buffer))
  ; Only kill the buffer if no other perspectives are using it
  (cond ((loop for persp being the hash-values of perspectives-hash
               unless (equal (car persp) persp-curr-name)
               if (memq buffer (cadr persp)) return nil
               finally return t)
         (kill-buffer buffer))
        ;; Make the buffer go away if we can see it.
        ;; TODO: Is it possible to tell if it's visible at all,
        ;;       rather than just the current buffer?
        ((eq buffer (current-buffer)) (bury-buffer))
        (t (bury-buffer buffer)))
  (setq persp-curr-buffers (remq buffer persp-curr-buffers)))

(defadvice switch-to-buffer (after persp-add-buffer-adv)
  (persp-add-buffer buffer))

(defun persp-init ()
  (setq persp-curr-buffers (buffer-list))
  (persp-save)
  (ad-activate 'switch-to-buffer)

  (setq global-mode-string (or global-mode-string '("")))
  (if (not (memq 'persp-modestring global-mode-string))
      (setq global-mode-string (append global-mode-string '(persp-modestring))))
  (persp-update-modestring))


(define-prefix-command 'perspective 'perspective-map)
(global-set-key (read-kbd-macro "C-x p") perspective-map)

(global-set-key (read-kbd-macro "C-x p n") 'persp-new)
(global-set-key (read-kbd-macro "C-x p s") 'persp-switch)
(global-set-key (read-kbd-macro "C-x p r") 'persp-remove-buffer)

(persp-init)
