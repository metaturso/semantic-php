;;; setup.el --- An iterative style parser for PHP 5.5 buffers

;;; Commentary:
;;
;; This file is used to configure and install a tagging PHP parser
;; that extracts crucial information about the symbols defined and
;; used in a php buffer.
;;
;; To allow you to M-x load-file setup.el and test this grammar
;; with a php buffer, I have added this load-path configuration:
;;
(add-to-list 'load-path "./")

;;; Code:

(require 'semantic/wisent)
(require 'grammar)

;; TODO: Handle file name resolution outside of ede-php-autoload projects.
(define-mode-local-override semantic-tag-include-filename php-mode (tag)
  "Maps a PHP qualified class name to a file.

  This allows Semantic to known about symbols used in this buffer
  and defined in a different file."

  (let ((class-name (semantic-tag-name tag)))
    (if (and (featurep 'ede-php-autoload) (ede-current-project))
        (let ((file-name (ede-php-autoload-find-class-def-file (ede-current-project) class-name)))
          (if file-name
              file-name
            class-name))
      class-name)))

(define-mode-local-override semantic-analyze-split-name php-mode (name)
  "Split up tag NAME into multiple parts by T_NS_SEPARATOR."
  (let ((ans (split-string name "\\\\")))
    (if (= (length ans) 1)
	name
      (delete "" ans))))

(define-mode-local-override semantic-analyze-unsplit-name php-mode (namelist)
  "Reassembles the components of NAMELIST into a qualified name."
  (mapconcat 'identity namelist "\\"))

(defun grammar-setup ()
  "Setup a new grammar to process PHP buffers using Semantic."

  (grammar--install-parser)

  (setq
   ;; Lexical analysis
   semantic-lex-analyzer 'grammar-lexer

   ;; Semantic requires this expression for line-comments, if lexing
   ;; without major mode.
   semantic-lex-comment-regex "\\s<\\|\\(/\\*\\|//\\)"

   ;; TODO: Don't ignore comment to allow docblock to scan types.
   parse-sexp-ignore-comments t

   ;; Separators to use when finding context prefix
   semantic-type-relation-separator-character '("::" "->")
   semantic-command-separation-character ";"


   semantic-symbol->name-assoc-list-for-type-parts
   '((type     . "Classes")
     (variable . "Attributes")
     (function . "Methods"))
   semantic-symbol->name-assoc-list

   (append semantic-symbol->name-assoc-list-for-type-parts
           '((package  . "In this pacakge")  ;; TODO add symbols in the same namespace
             (include  . "Imported packages")))

   ;; navigation inside 'type children
   senator-step-at-tag-classes '(function variable type)
   semanticdb-find-default-throttle)
  (semantic-mode 1))

;; Hooks the semantic-php grammar to php-mode.
;;
;; NOTE If you recompiled the grammar since you last visited a
;; php-buffer, revisit it with C-x C-v.
;;
;; NOTE If you are using ECB to view the tags, you probably will
;; have to refresh the Methods buffer with C-. r
(add-hook 'php-mode-hook 'grammar-setup)

(provide 'setup)

;;; setup.el ends here
