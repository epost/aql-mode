;; Usage:
;;   `(add-to-list 'auto-mode-alist '("\\.aql\\'" . aql-mode))`
;;
;; References:
;;  - http://www.wilfred.me.uk/blog/2015/03/19/adding-a-new-language-to-emacs/
;;  - https://www.emacswiki.org/emacs/EmacsSyntaxTable

(defconst aql-mode-syntax-table
  (let ((table (make-syntax-table java-mode-syntax-table)))
    ;; # is punctuation, but // is a comment starter
    (modify-syntax-entry ?/ ". 12" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(setq aql-highlights
      '(
        ("pragma\\|typeside\\|schema\\|instance\\|literal\\|exec_jdbc\\|import_jdbc\\|export_jdbc_instance\\|entities\\|foreign_keys\\|attributes\\|java_types\\|java_constants" . font-lock-keyword-face)
        (":\\|->" . font-lock-builtin-face)
        ))

(define-derived-mode aql-mode fundamental-mode "aql"
  "major mode for editing AQL (Algebraic Query Language) specifications."
  :syntax-table aql-mode-syntax-table
  (setq font-lock-defaults '(aql-highlights)))

(provide 'aql)
