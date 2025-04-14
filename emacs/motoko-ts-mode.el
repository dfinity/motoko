;; -*- lexical-binding: t; -*-

(defun motoko-ts-font-lock-rules ()
  (treesit-font-lock-rules
   :language 'motoko
   :override t
   :feature 'delimiter
   '((["(" ")" "[" "]" "{" "}" "<" ">"] @font-lock-bracket-face)
     ([";", ","] @font-lock-delimiter-face))

   :language 'motoko
   :override t
   :feature 'comment
   '([(line_comment) (block_comment) (doc_comment)] @font-lock-comment-face)

   :language 'motoko
   :override t
   :feature 'keyword
   '(["actor" "and" "await" "break" "case" "catch"
      "class" "continue" "composite" "debug" "do"
      "else" "finally" "for" "func" "if" "in" "import"
      "module" "not" "object" "or" "label" "let" "loop"
      "private" "public" "return" "shared"
      "try" "throw" "query" "switch" "type"
      "var" "while" "with" "stable" "flexible" "system"
      "assert" "ignore" "async" "persistent" "transient"
      ]
     @font-lock-keyword-face)

   :language 'motoko
   :override t
   :feature 'calls
   '((call_exp invoked_value: (variable) @font-lock-function-call-face)
     (call_exp invoked_value: (exp_post (member key: (name) @font-lock-function-call-face))))

   :language 'motoko
   :override t
   :feature 'keyword
   '((dec_type type_name: (name) @font-lock-type-face))

   :language 'motoko
   :override t
   :feature 'keyword
   '((typ_id type_name: (name) @font-lock-type-face)
     (typ_bind type_parameter_name: (name) @font-lock-type-face))

   :language 'motoko
   :override t
   :feature 'constant
   '((text) @font-lock-string-face)

   :language 'motoko
   :override t
   :feature 'constant
   '([(nat) (float)] @font-lock-number-face)

   :language 'motoko
   :override t
   :feature 'constant
   '("null" @font-lock-constant-face)

   :language 'motoko
   :override t
   :feature 'declaration
   '((dec_func function_name: (name) @font-lock-function-name-face))
   ))

;; (defvar motoko-ts-indent-rules
;;   `((motoko
;;      ((parent-is "block_expr") column-0 0))))

(defun motoko-ts-setup ()
  "Setup treesit for motoko-ts-mode."

  (setq-local treesit-font-lock-feature-list
              '((comment)
                (constant keyword calls)
                (declaration)))

  (setq-local treesit-font-lock-settings (motoko-ts-font-lock-rules))

  ;; This handles indentation -- again, more on that below.
  ;; (setq-local treesit-simple-indent-rules motoko-ts-indent-rules)

  (treesit-major-mode-setup))

(define-derived-mode motoko-ts-mode prog-mode "Motoko"
  "Major mode for editing Motoko source files"
  (when (treesit-ready-p 'motoko)
    (treesit-parser-create 'motoko)
    (motoko-ts-setup)))

(add-to-list 'auto-mode-alist '("\\.mo\\'" . motoko-ts-mode))
(add-to-list 'treesit-language-source-alist '(motoko "https://github.com/christoph-dfinity/tree-sitter-motoko"))

(provide 'motoko-ts-mode)
