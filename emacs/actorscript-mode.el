;; ActorScript major mode for Emacs
;; initially based on Swift Mode.

(setq actorscript-font-lock-keywords
      (let* (
             ;; define several category of keywords
             ;; these are each taken from either ActorScript's `lexer.mll' or `prelude.ml' files.
             (x-types
              '("Any"
                "None"
                "Shared"
                "Null"
                "Bool"
                "Nat"
                "Int"
                "Word8"
                "Word16"
                "Word32"
                "Word64"
                "Float"
                "Char"
                "Text"))
              (x-constants
               '("null"
                 "true"
                 "false"
                 ))
              (x-keywords
               '("actor"
                 "and"
                 "async"
                 "assert"
                 "await"
                 "break"
                 "case"
                 "class"
                 "continue"
                 "label"
                 "else"
                 "for"
                 "func"
                 "if"
                 "in"
                 "module"
                 "new"
                 "not"
                 "object"
                 "or"
                 "let"
                 "loop"
                 "private"
                 "return"
                 "shared"
                 "switch"
                 "type"
                 "var"
                 "while"
                 "prim"
                 ))
              ;; Braces introduce blocks; it's nice to make them stand
              ;; out more than ordinary symbols
              (x-braces
               '( "{"
                  "}"))
              (x-symbols
               '( "("
                  ")"
                  "["
                  "]"
                  ;"{"
                  ;"}"
                  ";"
                  ","
                  ":"
                  "<:"
                  ;"\\."
                  ;"\\?"
                  "="
                  "<"
                  ">"
                  ;"\\+"
                  "-"
                  ;"\\*"
                  "/"
                  "%"
                  "**"
                  "&"
                  "|"
                  ;"\\^"
                  "<<"
                  ">>"
                  "<<>"
                  "<>>"
                  "#"
                  "=="
                  "!="
                  ">="
                  "<="
                  ":="
                  "+="
                  "-="
                  "*="
                  "/="
                  "%="
                  "**="
                  "&="
                  "|="
                  "^="
                  "<<="
                  ">>="
                  "<<>="
                  "<>>="
                  "#="
                  ))
              ;; xxx These still don't work:
              (x-symbols-more
               '( "\\."
                  "\\?"
                  "\\+"
                  "\\-"
                  "\\*"
                  "\\^"
                  ))
        ;; generate regex string for each category of keywords
        (x-types-regexp (regexp-opt x-types 'words))
        (x-constant-regexp (regexp-opt x-constants 'words))
        (x-keywords-regexp (regexp-opt x-keywords 'words))
        (x-braces-regexp (regexp-opt x-braces))
        (x-symbols-regexp (regexp-opt x-symbols))
        (x-symbols-more-regexp (regexp-opt x-symbols-more))
        )
        ;;
        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-constant-regexp . font-lock-constant-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          (,x-braces-regexp . font-lock-keyword-face)
          (,x-symbols-regexp . font-lock-builtin-face)
          (,x-symbols-more-regexp . font-lock-builtin-face)
          )))

(define-derived-mode actorscript-mode
  swift-mode "ActorScript"
  "Major mode for ActorScript, aka 'CanisterScript'."
  (setq font-lock-defaults '((actorscript-font-lock-keywords)))
  )

(add-to-list 'auto-mode-alist '("\\.as\\'" . actorscript-mode))

;; add the mode to the `features' list
(provide 'actorscript-mode)
