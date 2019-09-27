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

;; Set frame size so that splitting windows doesn't result in pesky
;;
;;    "Window ... too small for splitting"
;;
;; errors.
(set-frame-height (selected-frame) 80)
(set-frame-width (selected-frame) 160)

(defmacro persp-test-with-persp (&rest body)
  "Allow multiple tests to run with reasonable assumption of
   isolation. This macro assumes persp-mode is turned off, then
   turns on persp-mode, evaluates the body, and finally remove
   all perspectives and open buffers."
  (declare (indent 0))
  `(progn
     (persp-mode 1)
     ,@body
     (persp-mode -1)
     (cl-flet ((new-fns-filter (b) (or (string-equal "*Messages*" (buffer-name b))
                                       (string-equal "*scratch*" (buffer-name b)))))
       (mapc #'kill-buffer (remove-if #'new-fns-filter (buffer-list))))))

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

(ert-deftest issue-85-pulling-buffers-into-other-persps ()
  (persp-test-with-persp
    (persp-test-with-temp-buffers (A1 A2 B1)
      (persp-switch "A")
      (select-window (split-window-right))
      (balance-windows)
      (switch-to-buffer A1)
      (switch-to-buffer A2)
      (persp-switch "B")
      (select-window (split-window-right))
      (switch-to-buffer B1)
      (persp-switch "A")
      (persp-switch "B")
      (kill-buffer)
      (walk-windows
       (lambda (w) (should-not
                    (memq (window-buffer w) (list A1 A2))))))))

(ert-deftest issue-81-renaming-scratch-buffers ()
  (persp-test-with-persp
    (persp-switch "A")
    (should (get-buffer "*scratch* (A)"))
    (persp-rename "B")
    (should (not (get-buffer "*scratch* (A)")))
    (should (get-buffer "*scratch* (B)"))))

;;; test-perspective.el ends here
