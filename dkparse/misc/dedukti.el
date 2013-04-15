;; Basics syntax highlighting for Dedukti files in GNU Emacs
;; Author: Raphaël Cauderlier 

(require 'generic-x)
(define-generic-mode
  'dedukti-mode
  '(("(;".";)"))                                           ;; comments
  '("-->" "[" "]" "." "->" "=>" ":" "(" ")" ":=")          ;; keywords
  '(("[_a-zA-Z][_a-zA-Z0-9]*" . 'font-lock-variable-face)) ;; identifiers
  '(".dk\\'")                                              ;; use this mode for .dk files
  nil
  "Major mode for editing Dedukti source code files.")

(provide 'dedukti)
