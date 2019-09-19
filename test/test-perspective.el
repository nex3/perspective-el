;;; test-perspective.el --- Tests for perspective

;; Licensed under the same terms as Emacs and under the MIT license.

;; URL: http://github.com/nex3/perspective-el
;; Created: 2019-09-18
;; By: Nathaniel Nicandro <nathanielnicandro@gmail.com>

;;; Commentary:

;;; Code:

(require 'perspective)
(require 'cl-lib)
(require 'ert)

(persp-mode 1)

(defmacro persp-test-with-temp-buffers (vars &rest body)
  "Bind temporary buffers to VARS and evaluate BODY."
  (declare (indent 1))
  (let ((binds (cl-loop
                for var in vars
                collect `(,var (generate-new-buffer "persp-test"))))
        (cleanup (cl-loop
                  for var in vars
                  collect `(when (buffer-live-p ,var)
                             (kill-buffer ,var)))))
    `(let (,@binds)
       (unwind-protect
           (progn ,@body)
         ,@cleanup))))

(ert-deftest issue-85 ()
  (persp-test-with-temp-buffers (b1 b2 b3 b4)
    (persp-switch "A")
    ;; Show b1 and b2 in two windows of A
    (switch-to-buffer b1)
    (select-window (split-window))
    (switch-to-buffer b2)
    (should (eq (window-buffer (previous-window)) b1))
    (should (eq (window-buffer (selected-window)) b2))
    (persp-switch "B")
    ;; Show b3 and b4 in two windows of B
    (switch-to-buffer b3)
    (select-window (split-window))
    (switch-to-buffer b4)
    (should (eq (window-buffer (previous-window)) b3))
    (should (eq (window-buffer (selected-window)) b4))
    ;; Switch back to A and do the main test
    (persp-switch "A")
    (should (eq (window-buffer (previous-window)) b1))
    (should (eq (window-buffer (selected-window)) b2))
    (kill-buffer)
    (should (= (count-windows) 2))
    (should (eq (window-buffer (previous-window)) b1))
    (should (eq (window-buffer (selected-window)) b1))))



;;; test-perspective.el ends here
