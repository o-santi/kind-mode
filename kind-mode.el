;;; kind-mode.el --- simple major mode for editing kind. -*- coding: utf-8; lexical-binding: t; -*-

;; Author: santi ( leonardors@dcc.ufrj.br )
;; Version: 0.0.1
;; Created: 17 Jun 2021
;; Keywords: languages kind emacs kind-lang

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; simple kind mode syntax highliter

;;; Code:

(require 'ansi-color)
;; (require 'smie)

;; i dont really like this hook but without it
;; the compilation buffer cant really render
;; ascii color codes consistently (or at least i havent found a way).
;; will keep this for now
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)

(defconst kind-highlights
  (let* ( (keywords '("let" "def" "get" "return" "type" "do" "case" "as"
		      "for" "if" "else" "rewrite" "in" "with"))
	  (constants '("true" "false" "refl" "mirror" "nil" "cons" "succ"))
	  (builtins '("->" "::" "+" "!" "-" "*" "&" "/" "^" "<" ">" "|")))
    `((,(regexp-opt keywords 'symbols). 'font-lock-keyword-face)
      (,(regexp-opt constants 'symbols) . 'font-lock-constant-face)
      (,(regexp-opt builtins 't) . 'font-lock-builtin-face)
      ("[A-Z][a-z0-9A-Z_]*" . 'font-lock-type-face))))

(defvar kind-mode-syntax-table nil "Syntax table for kind lang.")

(setq kind-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; coments in kind are C-like
    (modify-syntax-entry ?\/ ". 12b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?{ "(" table)
    (modify-syntax-entry ?} ")" table)
    table))


(defvar kind-mode-map nil "Keymap for kind mode.")

(defun kind-typecheck-buffer ()
  "Typecheck the current file."
  (interactive)
  (compile (concat "kind " (file-name-nondirectory buffer-file-name))))

(defun kind-run-term (term_name)
  "Run the term TERM_NAME."
  (interactive "sterm: ")
  (compile (concat "kind " term_name " --run")))

;; (defun kind-indent-current-line (kind token)
;;   "Kind indenter."
;;   (interactive)
;;   (beggining-of-line)
;;   (if (bobp)
;;       (indent-line-to 0)
;;     (let ((not-indented t) curr-indent)
;;       (if (not (looking-at "^.*?:.*?$"))
;; 	  (progn
;; 	    (save-excursion
;; 	      (forward-line -1)
;; 	      (setq cur-indent (+ (current-indentation) default-tab-width))

(defmacro add-command (keybind function)
  `(define-key kind-mode-map (kbd ,keybind) ,function))

(progn
  (setq kind-mode-map (make-sparse-keymap))
  (add-command "C-c C-c" 'kind-typecheck-buffer)
  (add-command "C-c C-r" 'kind-run-term))

(define-derived-mode kind-mode fundamental-mode "Kind"
  "Major mode for editing Kind lang code."
  (setq font-lock-defaults '(kind-highlights))
  (setq comment-start "//")
  ;;(setq indent-line-function #'kind-indent-function)
  ;; TODO: indentation
  (use-local-map kind-mode-map)
  (set-syntax-table kind-mode-syntax-table))

(provide 'kind-mode)
;;; kind-mode.el ends here
