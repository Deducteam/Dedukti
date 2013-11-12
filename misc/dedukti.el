;; Basics syntax highlighting for Dedukti files in GNU Emacs
;; Author: Raphaël Cauderlier 

(require 'generic-x)

; An identifier is composed of alphanumerical symbols and underscores
; but cannot start with a digit
(setq dedukti-id "[_a-zA-Z][_a-zA-Z0-9]*")

(setq dedukti-symbolic-keywords
      '(":="        ; Definition
        ":"         ; Declaration, annotated lambdas and pis
        "-->"       ; Rewrite-rule
        "->"        ; Pi (dependant type constructor)
        "=>"        ; Lambda (function constructor)
        "\\[" "\\]" ; Rewrite-rule environment
        "(" ")"     ; Expression grouping
        "{" "}"     ; Dot patterns
        ","         ; Environment separator
        "."         ; Global context separator
        ))

(defun dedukti-regexp-dijunct (l)
  "Return a regexp representing the disjunction of the elements of l
in the same order."
  (if l
      (if (cdr l)
          (concat (car l) "\\|" (dedukti-regexp-dijunct (cdr l)))
        (car l))
    nil))

(define-generic-mode
  dedukti-mode
  '(("(;".";)"))                                              ;; comments
  '("Type")                                                   ;; keywords
  `(
    (,(format "^ *#\\(IMPORT\\|NAME\\) %s" dedukti-id) .
     'font-lock-preprocessor-face)                            ;; pragmas
    (,(format "^ *%s *:=?" dedukti-id) .
     'font-lock-function-name-face)                           ;; declarations and definitions
    (,(format "%s *:[^=]" dedukti-id) .
     'font-lock-function-name-face)                           ;; variable name in lambdas and pis
    (,(format "%s\\.%s" dedukti-id dedukti-id) .
     'font-lock-constant-face)                                ;; qualified identifiers
    (,dedukti-id .
                 'font-lock-variable-name-face)               ;; identifiers
    (,(dedukti-regexp-dijunct dedukti-symbolic-keywords) . 'font-lock-keyword-face)
    ) 
  '(".dk\\'")                                              ;; use this mode for .dk files
  nil
  "Major mode for editing Dedukti source code files.")

(defun dedukti-compilation-error-find-file ()
  "Look backward in the compilation buffer looking for the last Dedukti file."
  (save-excursion
    (re-search-backward "[a-zA-Z_/]+.dk")
    (list (match-string 0))))

(require 'compile)
(add-to-list 'compilation-error-regexp-alist
    '("^File \\(.+\\), line \\([0-9]+\\), characters \\([0-9]+\\)-\\([0-9]+\\):"
      1 2 (3 . 4)))

(add-to-list 'compilation-error-regexp-alist
    '("^\\[l:\\([0-9]+\\);c:\\([0-9]+\\)\\].*\\[KO\\]"
      nil 1 2))

(add-to-list 'compilation-error-regexp-alist
    '("^\\(WARNING|ERROR\\) line:\\([0-9]+\\) column:\\([0-9]+\\)"
      nil 2 3))

(provide 'dedukti)
