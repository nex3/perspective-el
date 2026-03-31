;;; perf-perspective.el --- Performance harness for Perspective  -*- lexical-binding: t; -*-

;; Licensed under the same terms as Emacs and under the MIT license.

;; URL: http://github.com/nex3/perspective-el
;; Created: 2026-03-30

;;; Commentary:

;; This file provides an opt-in batch harness for comparing Perspective buffer
;; killing behavior across revisions. It is intentionally not part of the
;; default ERT suite.
;;
;; Run from the repository root with:
;;
;;   make perf
;;
;; Or directly:
;;
;;   emacs -Q --batch -L . -l test/perf-perspective.el --eval "(persp-perf-run)"
;;
;; Customize the run with:
;;
;;   (let ((persp-perf-num-perspectives 16)
;;         (persp-perf-buffers-per-perspective 24)
;;         (persp-perf-temp-buffers-per-perspective 8)
;;         (persp-perf-iterations 15))
;;     (persp-perf-run))

;;; Code:

(require 'benchmark)
(require 'cl-lib)
(require 'perspective)

(defvar persp-perf-num-perspectives 12
  "How many non-main perspectives to create for the performance harness.")

(defvar persp-perf-buffers-per-perspective 20
  "How many ordinary buffers to create in each perspective.")

(defvar persp-perf-temp-buffers-per-perspective 6
  "How many temporary buffers to create in each perspective.")

(defvar persp-perf-iterations 10
  "How many times to run each benchmark scenario.")

(defun persp-perf--cleanup-buffers ()
  "Kill all live buffers except the startup scratch buffer."
  (let ((scratch (get-buffer "*scratch*")))
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (not (eq buffer scratch)))
        (kill-buffer buffer)))))

(defmacro persp-perf--with-environment (&rest body)
  "Run BODY in a fresh Perspective environment and clean up afterward."
  (declare (indent 0))
  `(let ((persp-suppress-no-prefix-key-warning t)
         (persp-feature-flag-prevent-killing-last-buffer-in-perspective t))
     (persp-perf--cleanup-buffers)
     (persp-mode 1)
     (unwind-protect
         (progn ,@body)
       (persp-mode -1)
       (persp-perf--cleanup-buffers))))

(defun persp-perf--populate-perspectives ()
  "Populate multiple perspectives with ordinary and temporary buffers."
  (cl-loop for idx from 1 to persp-perf-num-perspectives do
           (let ((persp-name (format "P%d" idx)))
             (persp-switch persp-name)
             (cl-loop for buf-idx from 1 to persp-perf-buffers-per-perspective do
                      (switch-to-buffer (format "*%s-buf-%02d*" persp-name buf-idx)))
             (cl-loop for tmp-idx from 1 to persp-perf-temp-buffers-per-perspective do
                      (switch-to-buffer (format " %s-temp-%02d" persp-name tmp-idx)))))
  (persp-switch "main"))

(defun persp-perf--scenario-unrelated-kill ()
  "Kill a buffer from the current perspective while other perspectives are unrelated."
  (persp-perf--with-environment
   (persp-perf--populate-perspectives)
   (let ((target (switch-to-buffer "*perf-target*")))
     (switch-to-buffer "*perf-helper*")
     (switch-to-buffer target)
     (kill-buffer target))))

(defun persp-perf--scenario-shared-keeper ()
  "Kill a shared buffer that must remain alive in another perspective."
  (persp-perf--with-environment
   (let ((ido-ignore-buffers '("^\\*scratch\\*")))
     (persp-perf--populate-perspectives)
     (let ((target (switch-to-buffer "*perf-target*")))
       (persp-switch "P1")
       (switch-to-buffer target)
       (persp-switch "main")
       (switch-to-buffer "*perf-helper*")
       (switch-to-buffer target)
       (kill-buffer target)))))

(defun persp-perf--benchmark (name function)
  "Run benchmark NAME by calling FUNCTION `persp-perf-iterations' times."
  (let ((result (benchmark-run persp-perf-iterations (funcall function))))
    (message "%s" name)
    (message "  iterations: %d" persp-perf-iterations)
    (message "  elapsed:    %.6fs" (nth 0 result))
    (message "  gc-count:   %d" (nth 1 result))
    (message "  gc-elapsed: %.6fs" (nth 2 result))
    (message "")))

(defun persp-perf-run ()
  "Run the Perspective performance harness in batch mode."
  (interactive)
  (message "Perspective performance harness")
  (message "  perspectives: %d" persp-perf-num-perspectives)
  (message "  regular buffers / perspective: %d" persp-perf-buffers-per-perspective)
  (message "  temp buffers / perspective:    %d" persp-perf-temp-buffers-per-perspective)
  (message "")
  (persp-perf--benchmark
   "Scenario 1: kill buffer with unrelated other perspectives"
   #'persp-perf--scenario-unrelated-kill)
  (persp-perf--benchmark
   "Scenario 2: kill shared buffer that another perspective must keep"
   #'persp-perf--scenario-shared-keeper))

(provide 'perf-perspective)

;;; perf-perspective.el ends here
