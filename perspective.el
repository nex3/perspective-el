;; perspective.el --- switch between named "perspectives" of the editor
;; Copyright (C) 2008 Nathan Weizenbaum <nex342@gmail.com>
;; Licensed under the same terms as Emacs.

(defvar perspectives-hash (make-hash-table :test 'equal :size 10))

(defvar persp-current-name "main")

(defun persp-save ()
  (puthash persp-current-name (current-window-configuration) perspectives-hash))

(defun persp-new (name)
  (interactive "sNew perspective: \n")
  (persp-save)
  (setq persp-current-name name)
  (switch-to-buffer (concat "*scratch* (" name ")"))
  (lisp-interaction-mode)
  (delete-other-windows)
  (current-window-configuration))

(defun persp-switch (name)
  (interactive "sPerspective name: \n")
  (if (equal name persp-current-name) (current-window-configuration)
    (let ((conf (gethash name perspectives-hash)))
      (if (null conf) (persp-new name)
        (persp-save)
        (setq persp-current-name name)
        (set-window-configuration conf)))))

(define-prefix-command 'perspective 'perspective-map)
(global-set-key (read-kbd-macro "C-S-s") perspective-map)

(global-set-key (read-kbd-macro "C-S-s n") 'persp-new)
(global-set-key (read-kbd-macro "C-S-s s") 'persp-switch)
