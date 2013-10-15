;; Basics syntax highlighting for Dedukti files in GNU Emacs
;; Author: Raphaël Cauderlier 

(require 'generic-x)

(setq dedukti-id "[_a-zA-Z][_a-zA-Z0-9]*")

(define-generic-mode
  'dedukti-mode
  '(("(;".";)"))                                              ;; comments
  '("Type")                                                   ;; keywords
  `(
    (,(format "#\\(IMPORT\\|NAME\\) %s" dedukti-id) .
     'font-lock-preprocessor-face)                            ;; pragmas
    (,(format "%s *:[^=]" dedukti-id) .
     'font-lock-function-name-face)                           ;; declarations
    (,(format "%s\\.%s" dedukti-id dedukti-id) .
     'font-lock-constant-face)                                ;; qualified identifiers
    (,dedukti-id .
                 'font-lock-variable-name-face)               ;; identifiers
    (":=\\|-->\\|->\\|=>\\|\\[\\|\\]\\|(\\|)\\|{\\|}\\|,\\|\\." . 'font-lock-keyword-face)
    ) 
  '(".dk\\'")                                              ;; use this mode for .dk files
  nil
  "Major mode for editing Dedukti source code files.")

(provide 'dedukti)
