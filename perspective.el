;; perspective.el --- switch between named "perspectives" of the editor
;; Copyright (C) 2008 Nathan Weizenbaum <nex342@gmail.com>
;; Licensed under the same terms as Emacs.

(defvar perspectives-hash (make-hash-table :test 'equal :size 10))

(defvar persp-current-name "main")

(defun persp-switch (name)
  (interactive "sPerspective name: \n")
  (let ((curr-conf (current-window-configuration))
        conf)
    (if (equal name persp-current-name) curr-conf
      (setq conf (gethash name perspectives-hash))
      (if (null conf) curr-conf
        (remhash name perspectives-hash)
        (puthash persp-current-name curr-conf perspectives-hash)
        (setq persp-current-name name)
        (set-window-configuration conf)))))
