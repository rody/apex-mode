;;; apex-mode.el --- Apex support emacs -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Rodolphe Blancho
;;
;; Author: Rodolphe Blancho <http://github.com/rody>
;; Maintainer: Rodolphe Blancho <rodolphe.blancho@gmail.com>
;; Created: November 06, 2020
;; Modified: November 06, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/rody/apex-mode
;; Package-Requires: ((emacs 27.1))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Apex support for Emacs.
;;
;;; Code:
(require 'cc-mode)

(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'apex-mode 'java-mode))

(add-to-list 'auto-mode-alist '("\\.cls\\'" . apex-mode))
(add-to-list 'auto-mode-alist '("\\.trigger\\'" . apex-mode))

;; fix single quote issue introduced with emacs 26:
(c-lang-defconst c-before-font-lock-functions
  apex '(c-depropertize-new-text
         c-restore-<>-properties
         c-change-expand-fl-region))

(c-lang-defconst c-modifier-kwds
  apex '("abstract" "virtual" "final" "static" "override"
         "with sharing" "without sharing" "inherited sharing"
         "testmethod" "transient"))

(c-lang-defconst c-type-prefix-kwds
  apex '("class" "interface"))

(c-lang-defconst c-class-decl-kwds
  apex '("class" "interface"))

(c-lang-defconst c-protection-kwds
  apex '("global" "public" "protected" "private"))

(c-lang-defconst c-constant-kwds
  apex '("null" "true" "false"))

(c-lang-defconst c-block-comment-prefix       apex "/*")
(c-lang-defconst c-block-comment-starter      apex "/*")
(c-lang-defconst c-block-comment-ender        apex "*/")
(c-lang-defconst c-block-comment-start-regexp apex "/[*+]")
(c-lang-defconst c-comment-start-regexp       apex "/[*+/]")

(c-lang-defconst c-operators
  apex `(
         (prefix "super")

         (left-assoc ".")

         (postfix "++" "--" "[" "]" "(" ")")

         (prefix "++" "--" "+" "-" "!"
                 "(" ")") ; Cast
         ;; Multiplicative.
         (left-assoc "*" "/" "%")

         ;; Additive.
         (left-assoc "+" "-")

         ;; Equality.
         (left-assoc "==" "!=" "!==" "===" "!==")))

;; (c-lang-defconst c-other-kwds
;;   apex '("and" "as" "asc" "bulk" "by" "desc" "from" "instanceof"
;;          "like" "limit" "not" "nulls" "on" "or" "select" "where"))

(c-lang-defconst c-other-kwds
  apex '("select" "typeof" "end" "from" "using scope" "where" "with" "data category"
         "group by" "having" "order by" "asc" "desc" "nulls first" "null last"
         "limit" "offset" "for view" "for reference" "like" "in" "not" "includes" "excludes"
         "and" "or" "using scope" "update tracking" "update viewstat"))

;; helper functions for constructing case-insensitive regex
(defun anycase-letter (letter inside)
  (if (string-match "[[:alpha:]]" letter)
      (if inside
          (concat (downcase letter) (upcase letter))
        (concat "[" (downcase letter) (upcase letter) "]"))
    letter))


(defun anycase-regexp (source)
  (let ((result)
        (brace-lvl 0))
    (dotimes (i (length source))
      (let* ((current (substring source i (+ i 1)))
             (previous (when (> i 0) (substring source (- i 1) i)))
             (notspecial (not (string= "\\" previous))))
        (if notspecial
            (progn
              (when (string= "[" current) (setq brace-lvl (+ brace-lvl 1)))
              (when (string= "]" current) (setq brace-lvl (- brace-lvl 1)))
              (setq result (concat result (anycase-letter current (> brace-lvl 0))))) ;; FIXME: should probably not do this for [[?]]
          (setq result (concat result current)))))
    result))


;; overwrite the regex function and modify for case insensitive matching
(defun c-make-keywords-re (adorn list &optional mode)

  ;; delete duplicates
  (let (unique)
    (dolist (elt list)
      (unless (member elt unique)
        (push elt unique)))
    (setq list (delete nil unique)))


  (if list
      (let (re)

        ;; This is kludgy but it works: Search for a string that
        ;; doesn't occur in any word in LIST.  Append it to all
        ;; the alternatives where we want to add \>.  Run through
        ;; `regexp-opt' and then replace it with \>.
        (if (eq adorn 'appendable)
            (let ((unique "") pos)
              (while (let (found)
                       (setq unique (concat unique "@")
                             pos list)
                       (while (and pos
                                   (if (string-match unique (car pos))
                                       (progn (setq found t)
                                              nil)
                                     t))
                         (setq pos (cdr pos)))
                       found))
              (setq pos list)
              (while pos
                (if (string-match "\\w\\'" (car pos))
                    (setcar pos (concat (car pos) unique)))
                (setq pos (cdr pos)))
              (setq re (regexp-opt list))
              (setq pos 0)
              (while (string-match unique re pos)
                (setq pos (+ (match-beginning 0) 2)
                      re (replace-match "\\>" t t re))))
          (if (c-major-mode-is 'apex-mode)
              (setq re (anycase-regexp (regexp-opt list))) ;; make case insensitive!!
            (setq re (regexp-opt list))))

        ;; Emacs 20 and XEmacs (all versions so far) has a buggy
        ;; regexp-opt that doesn't always cope with strings containing
        ;; newlines.  This kludge doesn't handle shy parens correctly
        ;; so we can't advice regexp-opt directly with it.
        (let (fail-list)
          (while list
            (and (string-match "\n" (car list)) ; To speed it up a little.
                 (not (string-match (concat "\\`\\(" re "\\)\\'")
                                    (car list)))
                 (setq fail-list (cons (car list) fail-list)))
            (setq list (cdr list)))
          (when fail-list
            (setq re (concat re
                             "\\|"
                             (mapconcat
                              (if (eq adorn 'appendable)
                                  (lambda (str)
                                    (if (string-match "\\w\\'" str)
                                        (concat (regexp-quote str)
                                                "\\>")
                                      (regexp-quote str)))
                                'regexp-quote)
                              (sort fail-list
                                    (lambda (a b)
                                      (> (length a) (length b))))
                              "\\|")))))

        ;; Add our own grouping parenthesis around re instead of
        ;; passing adorn to `regexp-opt', since in XEmacs it makes the
        ;; top level grouping "shy".
        (cond ((eq adorn 'appendable)
               (concat "\\(" re "\\)"))
              (adorn
               (concat "\\(" re "\\)"
                       "\\("
                       (c-get-lang-constant 'c-nonsymbol-key nil mode)
                       "\\|$\\)"))
              (t
               re)))

    ;; Produce a regexp that matches nothing.
    (if adorn
        "\\(\\<\\>\\)"
      "\\<\\>")))

(put 'c-make-keywords-re 'lisp-indent-function 1)


(defconst apex-font-lock-keywords-1 (c-lang-const c-matchers-1 apex)
  "Minimal font locking for Apex mode.")

(defconst apex-font-lock-keywords-2 (c-lang-const c-matchers-2 apex)
  "Fast normal font locking for Apex mode.")

(defconst apex-font-lock-keywords-3 (c-lang-const c-matchers-3 apex)
  "Accurate normal font locking for Apex mode.")

(defvar apex-font-lock-keywords apex-font-lock-keywords-3
  "Default expressions to highlight in Apex mode.")

(defvar apex-mode-syntax-table nil
  "Syntax table used in apex-mode buffers.")

(or apex-mode-syntax-table
    (setq apex-mode-syntax-table
	  (funcall (c-lang-const c-make-mode-syntax-table apex))))

(add-to-list 'c-default-style '(apex-mode . "java"))

(defun apex-mode ()
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table apex-mode-syntax-table)
  (setq major-mode 'apex-mode
        mode-name "Apex"
        ;; local-abbrev-table apex-mode-abbrev-table
        abbrev-mode nil)
  ;;(use-local-map apex-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars apex-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'apex-mode)
  (add-to-list 'c-doc-comment-style '(apex-mode . javadoc))
  ;;(easy-menu-add apex-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'apex-mode-hook)
  (c-set-offset 'arglist-close 0)
  (c-set-offset 'arglist-intro '+)
  (c-update-modeline))

(provide 'apex-mode)
;;; apex-mode.el ends here
