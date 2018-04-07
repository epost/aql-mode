;; Usage:
;;   `(add-to-list 'auto-mode-alist '("\\.aql\\'" . aql-mode))`
;;
;; References:
;;  - http://www.wilfred.me.uk/blog/2015/03/19/adding-a-new-language-to-emacs/
;;  - https://www.emacswiki.org/emacs/EmacsSyntaxTable
;;  - https://github.com/CategoricalData/fql/blob/2df5b37f765e850ccb9a4f9e0c361179d6f484fd/src/catdata/aql/exp/AqlParser.java

(defconst aql-mode-syntax-table
  (let ((table (make-syntax-table java-mode-syntax-table)))
    ;; # is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(setq aql-highlights
      '(
        ("identity\\|literal\\|sql\\|schemaOf\\|typesideOf\\|toQuery\\|fromQuery\\|simple\\|chase\\|anonymize\\|frozen\\|unit\\|counit_query\\|counit\\|empty\\|union\\|coproduct\\|coproduct_sigma\\|distinct\\|eval\\|coeval\\|coequalize\\|src\\|dst\\|check\\|assert_consistent" . font-lock-constant-face)
        ("pragma\\|command\\|typeside\\|schema\\|instance\\|mapping\\|transform\\|constraints\\|query\\|graph\\|exec_jdbc\\|exec_js\\|exec_cmdline\\|import_csv\\|import_jdbc\\|export_jdbc_instance" . font-lock-keyword-face)
        ("html\\|md" . font-lock-keyword-face)
        ("from\\|where\\|exists" . font-lock-type-face)
        ("nodes\\|edges\\|entity\\|entities\\|foreign_keys\\|attributes\\|generators\\|equations\\|multi_equations\\|path_equations\\|observation_equations\\|options\\|functions\\|constants\\|types\\|imports\\|java_types\\|java_constants\\|java_functions" . font-lock-function-name-face)
        ("forall" . font-lock-variable-name-face)
        ("delta\\|lambda\\|pi\\|sigma" . font-lock-builtin-face)
        (":\\|->\\|;" . font-lock-builtin-face)
        ))

(define-derived-mode aql-mode fundamental-mode "aql"
  "major mode for editing AQL (Algebraic Query Language) specifications."
  :syntax-table aql-mode-syntax-table
  (setq font-lock-defaults '(aql-highlights)))

(provide 'aql)
