;;; anaphora.el --- anaphoric macros providing implicit temp variables
;;
;; This code is in the public domain.
;;
;; Author: Roland Walker <walker@pobox.com>
;; Homepage: http://github.com/rolandwalker/anaphora
;; URL: http://raw.github.com/rolandwalker/anaphora/master/anaphora.el
;; Version: 0.1.0
;; Last-Updated: 30 Oct 2012
;; EmacsWiki: Anaphora
;; Keywords: extensions
;;
;;; Commentary:
;;
;; Quickstart
;;
;;     (require 'anaphora)
;;
;;     (awhen (big-long-calculation)
;;       (foo it)      ; `it' is provided as
;;       (bar it))     ; a temporary variable
;;
;;     ;; anonymous function to compute factorial using `self'
;;     (alambda (x) (if (= x 0) 1 (* x (self (1- x)))))
;;
;; Explanation
;;
;; Anaphoric expressions implicitly create one or more temporary
;; variables which can be referred to during the expression.  This
;; technique can improve clarity in certain cases.  It also enables
;; recursion for anonymous functions.
;;
;; To use anaphora, place the anaphora.el library somewhere
;; Emacs can find it, and add the following to your ~/.emacs file:
;;
;;     (require 'anaphora)
;;
;; The following macros are made available
;;
;;     `aand'
;;     `ablock'
;;     `acase'
;;     `acond'
;;     `aecase'
;;     `aetypecase'
;;     `aif'
;;     `alambda'
;;     `alet'
;;     `aprog1'
;;     `aprog2'
;;     `atypecase'
;;     `awhen'
;;     `awhile'
;;     `a+'
;;     `a-'
;;     `a*'
;;     `a/'
;;
;; The following macros are experimental, especially the last one
;;
;;     `anaphoric-set'
;;     `anaphoric-setq'
;;     `anaphoric-setf-experimental'
;;
;; See Also
;;
;;     M-x customize-group RET anaphora RET
;;     http://en.wikipedia.org/wiki/On_Lisp
;;     http://en.wikipedia.org/wiki/Anaphoric_macro
;;
;; Notes
;;
;; Partially based on examples from the book "On Lisp", by Paul
;; Graham.
;;
;; When this library is loaded, the provided anaphoric forms are
;; registered as keywords in font-lock.  This may be disabled via
;; customize.
;;
;; Compatibility and Requirements
;;
;;     GNU Emacs version 24.3-devel     : yes, except macros marked experimental
;;     GNU Emacs version 24.1 & 24.2    : yes
;;     GNU Emacs version 23.3           : yes
;;     GNU Emacs version 22.3           : yes
;;     GNU Emacs version 21.x and lower : unknown
;;
;; Bugs
;;
;; TODO
;;
;;     better face for it and self
;;
;;; License
;;
;; All code contributed by the author to this library is placed in the
;; public domain.  It is the author's belief that the portions adapted
;; from examples in "On Lisp" are in the public domain.  At least 10
;; lines of code have been adapted from the Emacs 'cl package (in the
;; functions `anaphoric-setq' and `anaphoric-setf-experimental').  It
;; may be that the function `anaphoric-setf-experimental' is
;; sufficiently derived from Emacs as to be copyrighted under the GPL,
;; Version 3.
;;
;; Regardless of the copyright status of individual functions, all
;; code herein is free software, and is provided without any express
;; or implied warranties.
;;
;;; Code:
;;

;;; requirements

;; for declare, labels, do, block, case, ecase, typecase, etypecase
(require 'cl)

;;; customizable variables

;;;###autoload
(defgroup anaphora nil
  "Anaphoric macros providing implicit temp variables"
  :version "0.1.0"
  :link '(emacs-commentary-link :tag "Commentary" "anaphora")
  :link '(url-link :tag "Github" "http://github.com/rolandwalker/anaphora")
  :link '(url-link :tag "EmacsWiki" "http://emacswiki.org/emacs/Anaphora")
  :prefix "anaphora-"
  :group 'extensions)

(defcustom anaphora-add-font-lock-keywords t
  "Add anaphora macros to font-lock keywords when editing Emacs Lisp."
  :type 'boolean
  :group 'anaphora)

;;;###autoload
(defcustom anaphora-use-long-names-only nil
  "Use only long names such as `anaphoric-if' instead of traditional `aif'."
  :type 'boolean
  :group 'anaphora)

;;; font-lock

(when anaphora-add-font-lock-keywords
  (eval-after-load "lisp-mode"
    '(progn
       (let ((new-keywords '(
                             "anaphoric-if"
                             "anaphoric-prog1"
                             "anaphoric-prog2"
                             "anaphoric-when"
                             "anaphoric-while"
                             "anaphoric-and"
                             "anaphoric-cond"
                             "anaphoric-lambda"
                             "anaphoric-block"
                             "anaphoric-case"
                             "anaphoric-ecase"
                             "anaphoric-typecase"
                             "anaphoric-etypecase"
                             "anaphoric-let"
                             "aif"
                             "aprog1"
                             "aprog2"
                             "awhen"
                             "awhile"
                             "aand"
                             "acond"
                             "alambda"
                             "ablock"
                             "acase"
                             "aecase"
                             "atypecase"
                             "aetypecase"
                             "alet"
                             ))
             (special-variables '(
                                  "it"
                                  "self"
                                  )))
         (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "\\<" (regexp-opt special-variables 'paren) "\\>")
                                                     1 font-lock-variable-name-face)) 'append)
         (font-lock-add-keywords 'emacs-lisp-mode `((,(concat "(\\s-*" (regexp-opt new-keywords 'paren) "\\>")
                                                     1 font-lock-keyword-face)) 'append)))))

;;; aliases

;;;###autoload
(progn
  (defun anaphora--install-traditional-aliases (&optional arg)
    "Install traditional short aliases for anaphoric macros.

With negative numeric ARG, remove traditional aliases."
    (let ((syms '(
                  (if         .  t)
                  (prog1      .  t)
                  (prog2      .  t)
                  (when       .  when)
                  (while      .  t)
                  (and        .  t)
                  (cond       .  cond)
                  (lambda     .  lambda)
                  (block      .  block)
                  (case       .  case)
                  (ecase      .  ecase)
                  (typecase   .  typecase)
                  (etypecase  .  etypecase)
                  (let        .  let)
                  (+          .  t)
                  (-          .  t)
                  (*          .  t)
                  (/          .  t)
                  )))
      (cond
        ((and (numberp arg)
              (< arg 0))
         (dolist (cell syms)
           (when (ignore-errors
                   (eq (symbol-function (intern-soft (format "a%s" (car cell))))
                                        (intern-soft (format "anaphoric-%s" (car cell)))))
             (fmakunbound (intern (format "a%s" (car cell)))))))
        (t
         (dolist (cell syms)
           (let* ((builtin (car cell))
                  (traditional (intern (format "a%s" builtin)))
                  (long (intern (format "anaphoric-%s" builtin))))
             (defalias traditional long)
             (put traditional 'lisp-indent-function
                  (get builtin 'lisp-indent-function))
             (put traditional 'edebug-form-spec (cdr cell)))))))))

;;;###autoload
(unless anaphora-use-long-names-only
  (anaphora--install-traditional-aliases))

;;; macros

;;;###autoload
(defmacro anaphoric-if (cond then &rest else)
  "Like `if', but the result of evaluating COND is bound to `it'.

The variable `it' is available within THEN and ELSE.

COND, THEN, and ELSE are otherwise as documented for `if'."
  (declare (debug t)
           (indent 2))
  `(let ((it ,cond))
     (if it ,then ,@else)))

;;;###autoload
(defmacro anaphoric-prog1 (first &rest body)
  "Like `prog1', but the result of evaluating FIRST is bound to `it'.

The variable `it' is available within BODY.

FIRST and BODY are otherwise as documented for `prog1'."
  (declare (debug t)
           (indent 1))
  `(let ((it ,first))
     (progn ,@body)
     it))

;;;###autoload
(defmacro anaphoric-prog2 (form1 form2 &rest body)
  "Like `prog2', but the result of evaluating FORM2 is bound to `it'.

The variable `it' is available within BODY.

FORM1, FORM2, and BODY are otherwise as documented for `prog2'."
  (declare (debug t)
           (indent 2))
  `(progn
     ,form1
     (let ((it ,form2))
       (progn ,@body)
       it)))

;;;###autoload
(defmacro anaphoric-when (cond &rest body)
  "Like `when', but the result of evaluating COND is bound to `it'.

The variable `it' is available within BODY.

COND and BODY are otherwise as documented for `when'."
  (declare (debug when)
           (indent 1))
  `(anaphoric-if ,cond
       (progn ,@body)))

;;;###autoload
(defmacro anaphoric-while (test &rest body)
  "Like `while', but the result of evaluating TEST is bound to `it'.

The variable `it' is available within BODY.

TEST and BODY are otherwise as documented for `while'."
  (declare (debug t)
           (indent 1))
  `(do ((it ,test ,test))
       ((not it))
     ,@body))

;;;###autoload
(defmacro anaphoric-and (&rest conditions)
  "Like `and', but the result of the previous condition is bound to `it'.

The variable `it' is available within all CONDITIONS after the
initial one.

CONDITIONS are otherwise as documented for `and'.

Note that some implementations of this macro bind only the first
condition to `it', rather than each successive condition."
  (declare (debug t))
  (cond
    ((null conditions)
     t)
    ((null (cdr conditions))
     (car conditions))
    (t
     `(anaphoric-if ,(car conditions) (anaphoric-and ,@(cdr conditions))))))

;;;###autoload
(defmacro anaphoric-cond (&rest clauses)
  "Like `cond', but the result of each condition is bound to `it'.

The variable `it' is available within the remainder of each of CLAUSES.

CLAUSES are otherwise as documented for `cond'."
  (declare (debug cond))
  (if (null clauses)
      nil
    (let ((cl1 (car clauses))
          (sym (gensym)))
      `(let ((,sym ,(car cl1)))
         (if ,sym
             (if (null ',(cdr cl1))
                 ,sym
               (let ((it ,sym)) ,@(cdr cl1)))
           (anaphoric-cond ,@(cdr clauses)))))))

;;;###autoload
(defmacro anaphoric-lambda (args &rest body)
  "Like `lambda', but the function may refer to itself as `self'.

ARGS and BODY are otherwise as documented for `lambda'."
  (declare (debug lambda)
           (indent defun))
  `(labels ((self ,args ,@body))
     #'self))

;;;###autoload
(defmacro anaphoric-block (name &rest body)
  "Like `block', but the result of the previous expression is bound to `it'.

The variable `it' is available within all expressions of BODY
except the initial one.

NAME and BODY are otherwise as documented for `block'."
  (declare (debug block)
           (indent 1))
  `(block ,name
     ,(funcall (anaphoric-lambda (body)
                 (case (length body)
                   (0 nil)
                   (1 (car body))
                   (t `(let ((it ,(car body)))
                         ,(self (cdr body))))))
               body)))

;;;###autoload
(defmacro anaphoric-case (expr &rest clauses)
  "Like `case', but the result of evaluating EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `case'."
  (declare (debug case)
           (indent 1))
  `(let ((it ,expr))
     (case it ,@clauses)))

;;;###autoload
(defmacro anaphoric-ecase (expr &rest clauses)
  "Like `ecase', but the result of evaluating EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `ecase'."
  (declare (debug ecase)
           (indent 1))
  `(let ((it ,expr))
     (ecase it ,@clauses)))

;;;###autoload
(defmacro anaphoric-typecase (expr &rest clauses)
  "Like `typecase', but the result of evaluating EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `typecase'."
  (declare (debug typecase)
           (indent 1))
  `(let ((it ,expr))
     (typecase it ,@clauses)))

;;;###autoload
(defmacro anaphoric-etypecase (expr &rest clauses)
  "Like `etypecase', but result of evaluating EXPR is bound to `it'.

The variable `it' is available within CLAUSES.

EXPR and CLAUSES are otherwise as documented for `etypecase'."
  (declare (debug etypecase)
           (indent 1))
  `(let ((it ,expr))
     (etypecase it ,@clauses)))

;;;###autoload
(defmacro anaphoric-let (varlist &rest body)
  "Like `let', but the content of VARLIST is bound to `it'.

VARLIST as it appears in `it' is not evaluated.  The variable `it'
is available within BODY.

VARLIST and BODY are otherwise as documented for `let'."
  (declare (debug let)
           (indent 1))
  `(let ((it ',varlist)
         ,@varlist)
     (progn ,@body)))

;;;###autoload
(defmacro anaphoric-+ (&rest numbers-or-markers)
  "Like `+', but the result of evaluating the previous expression is bound to `it'.

The variable `it' is available within all expressions after the
initial one.

NUMBERS-OR-MARKERS are otherwise as documented for `+'."
  (declare (debug t))
  (cond
    ((null numbers-or-markers)
     0)
    (t
     `(let ((it ,(car numbers-or-markers)))
        (+ it (anaphoric-+ ,@(cdr numbers-or-markers)))))))

;;;###autoload
(defmacro anaphoric-- (&optional number-or-marker &rest numbers-or-markers)
  "Like `-', but the result of evaluating the previous expression is bound to `it'.

The variable `it' is available within all expressions after the
initial one.

NUMBER-OR-MARKER and NUMBERS-OR-MARKERS are otherwise as
documented for `-'."
  (declare (debug t))
  (cond
    ((null number-or-marker)
     0)
    ((null numbers-or-markers)
     `(- ,number-or-marker))
    (t
     `(let ((it ,(car numbers-or-markers)))
        (- ,number-or-marker (+ it (anaphoric-+ ,@(cdr numbers-or-markers))))))))

;;;###autoload
(defmacro anaphoric-* (&rest numbers-or-markers)
  "Like `*', but the result of evaluating the previous expression is bound to `it'.

The variable `it' is available within all expressions after the
initial one.

NUMBERS-OR-MARKERS are otherwise as documented for `*'."
  (declare (debug t))
  (cond
    ((null numbers-or-markers)
     1)
    (t
     `(let ((it ,(car numbers-or-markers)))
        (* it (anaphoric-* ,@(cdr numbers-or-markers)))))))

;;;###autoload
(defmacro anaphoric-/ (dividend divisor &rest divisors)
  "Like `/', but the result of evaluating the previous divisor is bound to `it'.

The variable `it' is available within all expressions after the
first divisor.

DIVIDEND, DIVISOR, and DIVISORS are otherwise as documented for `/'."
  (declare (debug t))
  (cond
    ((null divisors)
     `(/ ,dividend ,divisor))
    (t
     `(let ((it ,divisor))
        (/ ,dividend (* it (anaphoric-* ,@divisors)))))))

;;;###autoload
(defmacro anaphoric-set (symbol value)
  "Like `set', except that the value of SYMBOL is bound to `it'.

The variable `it' is available within VALUE.

SYMBOL and VALUE are otherwise as documented for `set'.

Note that if this macro followed traditional naming for
anaphoric expressions, it would conflict with the existing
\(quite different\) function `aset'."
  `(let ((it ,symbol))
     (set it ,value)))

;;;###autoload
(defmacro anaphoric-setq (&rest args)
  "Like `setq', except that the value of SYM is bound to `it'.

The variable `it' is available within each VAL.

ARGS in the form [SYM VAL] ... are otherwise as documented for `setq'.

No alias `asetq' is provided, because it would easily mistaken
for the pre-existing `aset', and because `anaphoric-setq' is not
likely to find frequent use."
  (cond
    ((null args)
     nil)
    ((> (length args) 2)
     (let ((pairs nil))
       (while args
         (push (list 'anaphoric-setq (pop args) (pop args)) pairs))
       (cons 'progn (nreverse pairs))))
    (t
     `(let ((it (quote ,(car args))))
        (set it ,(cadr args))))))

;; `anaphoric-setf-experimental' is marked "experimental" because
;;
;;    1 There is a double evaluation and the workaround is only
;;      lightly tested.
;;
;;    2 There is an outstanding test failure: anaphoric-setf-10.
;;
;;    3 Still trying to think of a real use-case where callf is
;;      not sufficient.  There is one contrived example in
;;      the tests: anaphoric-setf-16.
;;
;;    4 The innards of setf are being changed completely in
;;      Emacs 24.3.
;;
;;  Therefore this macro is currently only a plaything and may removed
;;  in a later revision for any of the above reasons.

;;;###autoload
(defmacro anaphoric-setf-experimental (&rest args)
  "Like `setf', except that the value of PLACE is bound to `it'.

The variable `it' is available within VAL.

ARGS in the form [PLACE VAL] ... are otherwise as documented for `setf'.

No alias `asetf' is provided, because it would be easily mistaken
for the pre-existing `aset', and because `anaphoric-setf' is not
likely to find frequent use."
  (cond
    ((not (fboundp 'cl-setf-do-modify))
     (error "unimplemented"))
    ((null args)
     nil)
    ((> (length args) 2)
     (let ((pairs nil))
       (while args
         (push (list 'anaphoric-setf-experimental (pop args) (pop args)) pairs))
       (cons 'progn (nreverse pairs))))
    ((symbolp (car args))
     (and args (cons 'anaphoric-setq args)))
    (t
     ;; ,partially-evaluated-place will be evaluated twice, once when
     ;; setting `it', and once during the eval of `store'.  The
     ;; working theory is that for all cases where setf is allowed,
     ;; the double-eval is safe, after the partial evaluation.  If
     ;; that is not true, digging into `cl-setf-do-modify' and
     ;; `cl-setf-do-store' would be required to make this macro work.
     ;; see test failure anaphoric-setf-10
     (let ((partially-evaluated-place (gensym)))
       `(let* ((,partially-evaluated-place (mapcar #'(lambda (x)
                                                       (if (and (listp x)
                                                                (not (eq 'quote (car x))))
                                                           (if (listp (setq x (eval x)))
                                                               (list 'quote x)
                                                             ;; else
                                                             x)
                                                         ;; else
                                                         x))
                                                   ',(car args)))
               (it (eval ,partially-evaluated-place)))
          (let* ((method (cl-setf-do-modify ,partially-evaluated-place ',(nth 1 args)))
                 (store (cl-setf-do-store (nth 1 method) ',(nth 1 args))))
            (if (car method)
                (eval (list 'let* (car method) store))
              ;; else
              (eval store))))))))

(provide 'anaphora)

;;
;; Emacs
;;
;; Local Variables:
;; indent-tabs-mode: nil
;; mangle-whitespace: t
;; require-final-newline: t
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions redefine)
;; End:
;;
;; LocalWords: Anaphora EXPR awhen COND ARGS alambda ecase typecase
;; LocalWords: etypecase aprog aand acond ablock acase aecase alet
;; LocalWords: atypecase aetypecase VARLIST
;;

;;; anaphora.el ends here
