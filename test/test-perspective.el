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

(defun persp-test-interesting-buffer? (buf)
  "Return t if BUF is a non-temporary buffer (i.e., lacks
tell-tale leading '*' characters)."
  (and (buffer-live-p buf)
       (not (string-match-p "^[[:space:]]*?\\*" (buffer-name buf)))))

(defun persp-test-buffer-list-all ()
  "Return the list of buffers in Emacs (in all
perspectives), filtering out temporaries."
  (remove-if-not #'persp-test-interesting-buffer? (buffer-list)))

(defun persp-test-buffer-list (persp)
  "Return the list of buffers in the current perspective,
filtering out temporaries."
  (remove-if-not #'persp-test-interesting-buffer? (persp-buffers persp)))

(defmacro persp-test-with-persp (&rest body)
  "Allow multiple tests to run with reasonable assumption of
isolation. This macro assumes persp-mode is turned off, then
turns on persp-mode, evaluates the body, and finally remove all
perspectives and open buffers."
  (declare (indent 0))
  `(progn
     (persp-mode 1)
     ,@body
     ;; get rid of perspective-specific *scratch* buffers first
     (mapc #'kill-buffer (mapcar (lambda (persp)
                                   (format "*scratch* (%s)" persp))
                                 (remove-if (lambda (persp)
                                              (string-equal "main" persp))
                                            (persp-names))))
     (persp-mode -1)
     (mapc #'kill-buffer (persp-test-buffer-list-all))))

(defmacro persp-test-with-temp-buffers (vars &rest body)
  "Bind temporary buffers to VARS, evaluate BODY, then kill
temporary buffers."
  (declare (indent 1))
  (let ((binds (cl-loop for var in vars collect
                        `(,var (generate-new-buffer "persp-test"))))
        (cleanup (cl-loop for var in vars collect
                          `(when (buffer-live-p ,var)
                             (kill-buffer ,var)))))
    `(let (,@binds)
       (unwind-protect
           (progn ,@body)
         ,@cleanup))))

(defmacro persp-test-with-files (temp-files? vars &rest body)
  "Bind temporary buffers with on-disk files to VARS,
evaluate BODY, then kill temporary buffers. If TEMP-FILES is t,
the files are purely temporary and will be deleted at cleanup. If
TEMP-FILES is nil, the file names are just the var symbols, will
be created and non-empty if they do not exist, and will not be
deleted at cleanup."
  (let ((binds (cl-loop for var in vars collect
                        `(,var (let ((new-buf (find-file
                                               (if ,temp-files?
                                                   (make-temp-file (symbol-name ',var))
                                                 (symbol-name ',var)))))
                                 (with-current-buffer new-buf
                                   (when (= 0 (buffer-size))
                                     (insert (buffer-file-name new-buf))
                                     (save-buffer))
                                   new-buf)))))
        (cleanup (cl-loop for var in vars collect
                          `(when (buffer-live-p ,var)
                             (when ,temp-files?
                               (delete-file (buffer-file-name ,var)))
                             (kill-buffer ,var)))))
    `(let (,@binds)
       (unwind-protect
           (progn ,@body)
         ,@cleanup))))

(defun persp-test-clean-files (&rest test-files)
  "Cleans the given TEST-FILES."
  (cl-loop for f in test-files do
           (when (file-exists-p f)
             (delete-file f))))

(ert-deftest helper-macros ()
  (should (= 0 (length (persp-test-buffer-list-all))))
  (let ((saved-path-B1 "")
        (saved-path-B2 ""))
    ;; temp files
    (persp-test-with-files t (B1 B2)
      (should (get-buffer B1))
      (setf saved-path-B1 (buffer-file-name B1))
      (should (file-exists-p saved-path-B1))
      (should (get-buffer B2))
      (setf saved-path-B2 (buffer-file-name B2))
      (should (file-exists-p saved-path-B2)))
    (should-not (file-exists-p saved-path-B1))
    (should-not (file-exists-p saved-path-B2))
    ;; non-temp files, will require cleanup
    (should-not (get-buffer "A1"))
    (should-not (get-buffer "A2"))
    (persp-test-with-files nil (A1 A2)
      (should (get-buffer A1))
      (should (get-buffer "A1"))
      (should (get-buffer A2))
      (should (get-buffer "A2")))
    ;; clean up non-temp files
    (delete-file "A1")
    (delete-file "A2"))
  ;; no buffers should be open after all this
  (should (= 0 (length (persp-test-buffer-list-all)))))

(ert-deftest basic-persp-creation-deletion ()
  (persp-test-with-persp
    (should (equal (list "main") (persp-names)))
    (persp-switch "A")
    (should (equal (list "A" "main") (sort (persp-names) #'string-lessp)))
    (persp-switch "B")
    (should (equal (list "A" "B" "main") (sort (persp-names) #'string-lessp)))
    (persp-kill "A")
    (should (equal (list "B" "main") (sort (persp-names) #'string-lessp)))
    (persp-kill "B")
    (should (equal (list "main") (persp-names)))))

(ert-deftest basic-persp-switching ()
  (persp-test-with-persp
    (persp-test-with-temp-buffers (A1 A2 B1 B2 B3)
      ;; currently in "main" perspective
      (cl-loop for buf in (list A1 A2 B1 B2 B3) do
               (should-not (memq buf (persp-buffers (persp-curr)))))
      (persp-switch "A")
      (switch-to-buffer A1)
      (switch-to-buffer A2)
      (cl-loop for buf in (list A1 A2) do
               (should (memq buf (persp-buffers (persp-curr)))))
      (cl-loop for buf in (list B1 B2 B3) do
               (should-not (memq buf (persp-buffers (persp-curr)))))
      (persp-switch "B")
      (switch-to-buffer B1)
      (switch-to-buffer B2)
      (switch-to-buffer B3)
      (cl-loop for buf in (list A1 A2) do
               (should-not (memq buf (persp-buffers (persp-curr)))))
      (cl-loop for buf in (list B1 B2 B3) do
               (should (memq buf (persp-buffers (persp-curr)))))
      (persp-switch "main")
      (cl-loop for buf in (list A1 A2 B1 B2 B3) do
               (should-not (memq buf (persp-buffers (persp-curr))))))))

(defmacro persp-test-make-sample-environment ()
  "Make a test environment with the following window layout:

        A                  B
+----+-----+----+  +---------------+
|    |     |    |  |    B1 / B3    |
|    |     |    |  |               |
| A1 |  A2 | A3 |  +---------------+
|    |     |    |  |    B2 / B4    |
|    |     |    |  |               |
+----+-----+----+  +---------------+

This produces two perspectives, each with individual window
layouts and files. The 'main' perspective remains empty.

Notice the shameless symbol capture! This macro expects to be
called where A1, A2, A3, B1, B2, B3, and B4 are already bound
using persp-test-with-files."
  `(progn
     (persp-switch "A")
     (switch-to-buffer A1)
     (select-window (split-window-right))
     (switch-to-buffer A2)
     (select-window (split-window-right))
     (switch-to-buffer A3)
     (balance-windows)
     (persp-switch "B")
     (switch-to-buffer B3)
     (switch-to-buffer B1)
     (select-window (split-window-below))
     (switch-to-buffer B4)
     (switch-to-buffer B2)))

(defmacro persp-test-check-sample-environment ()
  "Runs a series of checks on the environment created by
persp-test-make-sample-environment."
  `(progn
     (should (equal (list "A" "B" "main") (sort (persp-names) #'string-lessp)))
     (persp-switch "A")
     ;; check buffers in perspective A
     (should (equal (list "A1" "A2" "A3")
                    (sort (mapcar #'buffer-name (persp-test-buffer-list (persp-curr)))
                          #'string-lessp)))
     ;; check windows in perspective A
     (should (equal (list "A1" "A2" "A3")
                    (sort (mapcar #'buffer-name (mapcar #'window-buffer (window-list)))
                          #'string-lessp)))
     (persp-switch "B")
     ;; check buffers in perspective B
     (should (equal (list "B1" "B2" "B3" "B4")
                    (sort (mapcar #'buffer-name (persp-test-buffer-list (persp-curr)))
                          #'string-lessp)))
     ;; check windows in perspective B
     (should (equal (list "B1" "B2")
                    (sort (mapcar #'buffer-name (mapcar #'window-buffer (window-list)))
                          #'string-lessp)))))

(ert-deftest basic-persp-window-retention ()
  (unwind-protect
      (persp-test-with-persp
        (persp-test-with-files nil (A1 A2 A3 B1 B2 B3 B4)
          (persp-test-make-sample-environment)
          (persp-test-check-sample-environment)))
    (persp-test-clean-files "A1" "A2" "A3" "B1" "B2" "B3" "B4")))

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
