;;; test-perspective.el --- Tests for perspective  -*- lexical-binding: t; -*-

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
  (cl-remove-if-not #'persp-test-interesting-buffer? (buffer-list)))

(defun persp-test-buffer-list (persp)
  "Return the list of buffers in the current perspective,
filtering out temporaries."
  (cl-remove-if-not #'persp-test-interesting-buffer? (persp-buffers persp)))

(defun persp-test-match-scratch-buffers (&rest buffer-or-name)
  "Return a list of buffers that are considered *scratch* buffers.
If not nil, verify that BUFFER-OR-NAME are all the buffers that
are in the list of buffers or return nil otherwise.  Return nil
if there are no *scratch* buffers to be found.

Consider buffers being *scratch* buffers when their name begins
with \"*scratch*\".  Sort the list by names via `string-lessp'."
  (let ((scratch-buffers (mapcar #'get-buffer buffer-or-name))
        (matched-buffers (cl-remove-if-not
                          (lambda (buffer)
                            (string-match-p "^\\*scratch\\*.*$" (buffer-name buffer)))
                          (buffer-list))))
    (when (cl-every (lambda (buffer)
                      (memq buffer matched-buffers))
                    scratch-buffers)
      (sort matched-buffers
            (lambda (a b)
              (string-lessp (buffer-name a) (buffer-name b)))))))

(defun persp-test-kill-extra-buffers (&rest buffer-or-name)
  "Kill BUFFER-OR-NAME and extra scratch buffers found.
Return a list of names of the killed buffers, or nil if there's
no candidate to kill.

Extra scratch buffers have a name that begins with \"*scratch*\",
like \"*scratch* \" and \"*scratch* (NAME)\"."
  (let* (kill-list-names
         (scratch-buffer (get-buffer "*scratch*"))
         (extra-buffers (mapcar #'get-buffer buffer-or-name))
         (matched-buffers (remq scratch-buffer (persp-test-match-scratch-buffers))))
    (dolist (buffer (cl-remove-duplicates (append extra-buffers matched-buffers)))
      (when (buffer-live-p buffer)
        (push (buffer-name buffer) kill-list-names)
        (kill-buffer buffer)))
    (sort kill-list-names #'string-lessp)))

(defmacro persp-test-with-persp (&rest body)
  "Allow multiple tests to run with reasonable assumption of
isolation. This macro assumes persp-mode is turned off, then
turns on persp-mode, evaluates the body, and finally remove all
perspectives and open buffers."
  (declare (indent 0))
  `(progn
     (persp-mode 1)
     (unwind-protect
         (progn ,@body)
       (let ((other-persps (delete persp-initial-frame-name (persp-names))))
         ;; Kill other perspectives than the main perspective to get rid
         ;; of buffers that are not found in the main perspective.
         (mapc #'persp-kill other-persps)
         ;; Then get rid of perspective-specific *scratch* buffers which
         ;; have become part of the main perspective.
         (mapc (lambda (persp)
                 ;; `get-buffer' should suffice here, there's no need to
                 ;; also call `buffer-live-p' when a string is passed as
                 ;; argument to the former, but we do it anyway ;)
                 (let* ((scratch-name (format "*scratch* (%s)" persp))
                        (scratch-buffer (get-buffer scratch-name)))
                   (when (buffer-live-p scratch-buffer)
                     (kill-buffer scratch-buffer))))
               other-persps))
       (persp-mode -1)
       ;; Remove live buffers that are not temporaries.
       (mapc #'kill-buffer (persp-test-buffer-list-all)))))

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

(ert-deftest basic-persp-test-with-persp ()
  "Test `persp-test-with-persp'.

When cleaning up, don't assume there's a \"*scratch* (NAME)\" for
every perspective NAME still existing before the cleanup.  That
may cause `kill-buffer' to fail passing a non-existent buffer.

Buffers like \"*dummy* (NAME)\" should not be killed just because
they follow the pattern \"*scratch* (NAME)\".

Buffers should always be killed when only found in perspectives
that are not the main perspective."
  (unwind-protect
      (persp-test-with-persp
        ;; Summary before exiting `persp-test-with-persp':
        ;;
        ;; main: *dummy*, *dummy* (A), *scratch*, *scratch* (A), *scratch* (C)
        ;;    A:          *dummy* (A)
        ;;    B:          *dummy* (B)
        ;;    C is killed before the cleanup
        ;;    D:                                  *scratch* (D)
        ;;
        ;; Expected buffers after `persp-test-with-persp' cleanup:
        ;;
        ;;       *dummy*, *dummy* (A), *scratch*, *scratch* (C)
        ;;
        ;; *scratch* (A) is killed during cleanup, because perspective
        ;; A exists before the cleanup, instead *scratch* (C) will not
        ;; be killed, because perspective C has been killed before the
        ;; cleanup.  There should be no attempt killing *scratch* (B),
        ;; since it's non-existent and *scratch* (D) is killed killing
        ;; perspective D.  *dummy* (A) persists, it is shared with the
        ;; main perspective, instead *dummy* (B) is killed killing the
        ;; perspective B, it's not shared with the main perspective.
        (let ((dummy-buffer (get-buffer-create "*dummy*"))
              (dummy-buffer-A (get-buffer-create "*dummy* (A)"))
              (dummy-buffer-B (get-buffer-create "*dummy* (B)")))
          (should persp-mode)
          (should (buffer-live-p dummy-buffer))
          (should (buffer-live-p dummy-buffer-A))
          (should (buffer-live-p dummy-buffer-B))
          (should (get-buffer-create "*scratch*"))
          (persp-set-buffer (get-buffer "*scratch*"))
          (persp-switch "A")
          (persp-set-buffer dummy-buffer-A)
          (should (get-buffer "*scratch* (A)"))
          (persp-switch "B")
          (persp-set-buffer dummy-buffer-B)
          (should (get-buffer "*scratch* (B)"))
          (persp-switch "C")
          (persp-set-buffer dummy-buffer)
          (should (get-buffer "*scratch* (C)"))
          (persp-switch "D")
          (should (get-buffer "*scratch* (D)"))
          (persp-switch "main")
          (persp-add-buffer dummy-buffer)
          (persp-add-buffer dummy-buffer-A)
          (should (kill-buffer "*scratch* (B)"))
          (should-not (get-buffer "*scratch* (B)"))
          (persp-set-buffer (get-buffer "*scratch* (A)"))
          (persp-set-buffer (get-buffer "*scratch* (C)"))
          (persp-kill "C")))
    (should-not persp-mode)
    ;; Buffers found only in the main perspective or shared with main.
    (should (get-buffer "*dummy*"))
    (should (get-buffer "*scratch*"))
    (should (get-buffer "*dummy* (A)"))
    ;; Buffers found in the main perspective while perspective NAME is
    ;; killed before the cleanup.
    (should (get-buffer "*scratch* (C)"))
    ;; Buffers found in the main perspective while perspective NAME is
    ;; alive before the cleanup.
    (should-not (get-buffer "*scratch* (A)"))
    ;; Buffers found only in other perspectives than main perspective.
    (should-not (get-buffer "*dummy* (B)"))
    (should-not (get-buffer "*scratch* (D)"))
    ;; Buffers manually killed but perspective NAME is kept alive till
    ;; the cleanup.
    (should-not (get-buffer "*scratch* (B)"))
    ;; Reset state.
    (should (kill-buffer "*dummy*"))
    (should-not (get-buffer "*dummy*"))
    (should (kill-buffer "*dummy* (A)"))
    (should-not (get-buffer "*dummy* (A)"))
    (should (kill-buffer "*scratch* (C)"))
    (should-not (get-buffer "*scratch* (C)"))))

(ert-deftest basic-persp-test-match-scratch-buffers ()
  "Test `persp-test-match-scratch-buffers'.

Expect a list of live buffers that may be considered *scratch*
buffers, aka the buffer's name begins with \"*scratch*\", or nil
if there's none.

When providing buffer or buffer's name arguments, them should
include exactly what the function would find.  Repetition and
order do not matter.  If just one argument isn't a candidate,
return nil."
  (let (matched-buffers dummy-buffer scratch-buffer scratch-buffer-A)
    ;; Cleanup *scratch* buffers.
    (mapc (lambda (buffer)
            (when (string-match-p "^\\*scratch\\*.*$" (buffer-name buffer))
              (kill-buffer buffer)))
          (buffer-list))
    ;; Match live *scratch* buffers.
    (persp-test-with-persp
      (should (setq dummy-buffer (get-buffer-create "*dummy*")))
      (should (setq scratch-buffer (get-buffer-create "*scratch*")))
      ;; get a list of all live *scratch* buffers
      (should (setq matched-buffers (list scratch-buffer)))
      (should (equal matched-buffers (persp-test-match-scratch-buffers)))
      ;; verify that all arguemnts meet the criteria
      (should (equal matched-buffers (persp-test-match-scratch-buffers "*scratch*")))
      (should (equal matched-buffers (persp-test-match-scratch-buffers scratch-buffer)))
      (should (equal matched-buffers (persp-test-match-scratch-buffers "*scratch*" scratch-buffer)))
      (should (equal matched-buffers (persp-test-match-scratch-buffers scratch-buffer "*scratch*")))
      ;; verify that the matching criteria cannot be subverted
      (should-not (persp-test-match-scratch-buffers "*dummy*"))
      (should-not (persp-test-match-scratch-buffers dummy-buffer))
      (should-not (persp-test-match-scratch-buffers "*scratch* "))
      (should-not (persp-test-match-scratch-buffers "*scratch*" "*dummy*"))
      (should-not (persp-test-match-scratch-buffers "*dummy*" "*scratch*"))
      (should-not (persp-test-match-scratch-buffers "*scratch*" "*scratch* "))
      (should-not (persp-test-match-scratch-buffers "*scratch* " "*scratch*"))
      (should-not (persp-test-match-scratch-buffers scratch-buffer dummy-buffer))
      (should-not (persp-test-match-scratch-buffers dummy-buffer scratch-buffer))
      (should-not (persp-test-match-scratch-buffers "*scratch*" "*scratch* " "*scratch*"))
      (should (kill-buffer dummy-buffer))
      (should-not (get-buffer "*dummy*"))
      (should-not (buffer-live-p dummy-buffer))
      (should-not (persp-test-match-scratch-buffers dummy-buffer))
      (should-not (persp-test-match-scratch-buffers scratch-buffer dummy-buffer))
      (should-not (persp-test-match-scratch-buffers dummy-buffer scratch-buffer))
      (should (get-buffer-create "*scratch* "))
      (setq matched-buffers (list scratch-buffer (get-buffer "*scratch* ")))
      (should (equal matched-buffers (persp-test-match-scratch-buffers "*scratch*" "*scratch* ")))
      (should (equal matched-buffers (persp-test-match-scratch-buffers "*scratch* " "*scratch*")))
      (should (equal matched-buffers (persp-test-match-scratch-buffers "*scratch*" "*scratch* " "*scratch*"))))
    ;; Cleanup *scratch* buffers.
    (mapc (lambda (buffer)
            (when (string-match-p "^\\*scratch\\*.*$" (buffer-name buffer))
              (kill-buffer buffer)))
          (buffer-list))
    ;; Match live *scratch* buffers.
    (persp-test-with-persp
      (persp-new "A")
      (should (setq scratch-buffer (get-buffer-create "*scratch*")))
      (should (setq scratch-buffer-A (get-buffer "*scratch* (A)")))
      ;; get a list of all live *scratch* buffers
      (should (setq matched-buffers (list scratch-buffer scratch-buffer-A)))
      (should (equal matched-buffers (persp-test-match-scratch-buffers)))
      ;; verify that all arguemnts meet the criteria
      (should (equal matched-buffers (persp-test-match-scratch-buffers "*scratch*" "*scratch* (A)")))
      (should (equal matched-buffers (persp-test-match-scratch-buffers "*scratch* (A)" "*scratch*")))
      (should (equal matched-buffers (persp-test-match-scratch-buffers scratch-buffer scratch-buffer-A)))
      (should (equal matched-buffers (persp-test-match-scratch-buffers scratch-buffer-A "*scratch* (A)" scratch-buffer "*scratch*")))
      ;; verify that the matching criteria cannot be subverted
      (should-not (persp-test-match-scratch-buffers "*scratch*" "*scratch* " "*scratch* (A)"))
      (should (get-buffer-create "*scratch* "))
      (setq matched-buffers (list scratch-buffer (get-buffer "*scratch* ") scratch-buffer-A))
      (should (equal matched-buffers (persp-test-match-scratch-buffers "*scratch*" "*scratch* " "*scratch* (A)")))
      (should (equal matched-buffers (persp-test-match-scratch-buffers "*scratch* (A)" "*scratch*" "*scratch* "))))
    ;; Cleanup *scratch* buffers.
    (mapc (lambda (buffer)
            (when (string-match-p "^\\*scratch\\*.*$" (buffer-name buffer))
              (kill-buffer buffer)))
          (buffer-list))
    (should (get-buffer-create "*scratch*"))))

(ert-deftest basic-persp-header-line-format-default-value ()
  "Disabling `persp-mode' should properly restore the default
value of `header-line-format'.

Updating `header-line-format' default value using a buffer
local value of it is a mistake."
  (let ((persp-show-modestring 'header)
	(default-header-line-format (default-value 'header-line-format)))
    ;; Since `persp-test-with-persp' may change in the future, do not
    ;; use it.  We need to avoid switching to another buffer than the
    ;; *dummy* buffer just before `persp-mode' is disabled.  For this
    ;; test to work, a buffer with a locally modified header needs to
    ;; be the current buffer when `persp-mode' is disabled.
    (persp-mode 1)
    (should (switch-to-buffer "*dummy*"))
    (should (equal (buffer-name) "*dummy*"))
    (setq header-line-format "custom header")
    ;; This is just being paranoid about the header's default value.
    (should-not (equal header-line-format default-header-line-format))
    (should-not (equal header-line-format (default-value 'header-line-format)))
    (persp-mode -1)
    (should (equal (buffer-name) "*dummy*"))
    (should (equal header-line-format "custom header"))
    (let ((updated-header-line-format (default-value 'header-line-format)))
      (should (equal updated-header-line-format default-header-line-format)))
    ;; Cleanup.
    (kill-buffer "*dummy*")))

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

(defmacro with-named-persp (name &rest body)
  "Create a perspective, run the body, and then kill the perspectiev"
  (declare (indent 1))
  `(progn
     (persp-switch ,name)
     ,@body
     (persp-kill ,name)))

;; REFACTOR: push this up?
(defun persp-last-name ()
  (and (persp-last) (persp-name (persp-last))))

;; REFACTOR: push this up?
(defun persp-curr-name ()
  (and (persp-curr) (persp-name (persp-curr))))

(defmacro should-persp-equal (a b c d)
  `(progn
     (should (equal ,a (persp-names)))
     (should (equal ,b (persp-curr-name)))
     (should (equal ,c (persp-last-name)))
     (should (equal ,d (persp-find-some)))))

(ert-deftest issue-90-persp-last--vs--persp-find-some ()
  (persp-test-with-persp
   (let ((persp-sort 'created))         ; this should be respected when killing
     (should-persp-equal       '("main")             "main" nil    "main")
     (with-named-persp "A"
       (should-persp-equal     '("A" "main")         "A"    "main" "main")
       (with-named-persp "B"
         (should-persp-equal   '("B" "A" "main")     "B"    "A"    "A")
         (with-named-persp "C"
           (should-persp-equal '("C" "B" "A" "main") "C"    "B"    "B")) ; pop C
         (should-persp-equal   '("B" "A" "main")     "B"    nil    "B")) ; pop B
       (should-persp-equal     '("A" "main")         "A"    nil    "A")) ; pop A
     (should-persp-equal       '("main")             "main" nil    "main"))))

(ert-deftest state-save-and-load ()
  (unwind-protect
      (persp-test-with-persp
        (persp-test-with-files nil (A1 A2 A3 B1 B2 B3 B4)
          (persp-test-make-sample-environment)
          (should (= 7 (length (persp-test-buffer-list-all)))) ; sanity check
          (persp-state-save "state-1.el"))
        ;; reset perspectives
        (persp-mode -1)
        (delete-other-windows)
        (should (= 0 (length (persp-test-buffer-list-all)))) ; no open files
        (persp-mode 1)
        (should (equal (list "main") (sort (persp-names) #'string-lessp)))
        ;; load it back up
        (persp-state-load "state-1.el")
        (should (= 7 (length (persp-test-buffer-list-all)))) ; sanity check again
        (persp-test-check-sample-environment))
    (persp-test-clean-files "A1" "A2" "A3" "B1" "B2" "B3" "B4" "state-1.el")))

;;; test-perspective.el ends here
