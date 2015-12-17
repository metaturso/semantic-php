;;; setup.el --- An iterative style parser for PHP 5.5 buffers

;;; Commentary:
;;
;; This file is used to configure and install a tagging PHP parser
;; that extracts crucial information about the symbols defined and
;; used in a php buffer.
;;

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

(define-mode-local-override semanticdb-find-table-for-include php-mode (includetag &optional table)
  "For a single INCLUDETAG found in TABLE, find a `semanticdb-table' object
INCLUDETAG is a semantic TAG of class 'include.
TABLE is a semanticdb table that identifies where INCLUDETAG came from.
TABLE is optional if INCLUDETAG has an overlay of :filename attribute."
  ;; Generic way of looking up a class.
  ;;
  ;; Are we in the same namespace or package?
  ;; If we are in a namespace, does the namespace part of the include
  ;; match the current namespace? If yes we might be able to find our
  ;; way around the file system without knowing anything else, elthough
  ;; we should rely on ede.
  (if (and (featurep 'ede-php-autoload) (ede-current-project))
      (semantic-file-tag-table (ede-php-autoload-find-class-def-file (ede-current-project) (car includetag)))))

(define-mode-local-override semantic-ctxt-scoped-types php-mode (&optional point)
  "Return a list of type names currently in scope at POINT.
The return value can be a mixed list of either strings (names of
types that are in scope) or actual tags (type declared locally
that may or may not have a name.)"
  ;; 1. all types imported with using
  ;; 2. all types in the same namespace
  ;; 3. all types declared in the same buffer (and namespace scope as 2)
  (save-excursion
    (when point (goto-char point))
    (let* ((top-level (semantic-find-tag-by-overlay))
           (parent-tag (car (cdr (nreverse top-level))))
           ;; NOTE this line assumes top-level is always a namespace.
           ;;
           ;; I have considered always providing a top-level
           ;; namespace, but decided not to walk that route just yet.
           ;;
           ;; The next change I'll be making here is probably
           ;; improving the top-level initialisation code above.
           (imported-types (semantic-find-tags-by-class 'using (semantic-flatten-tags-table top-level)))
           scoped-types)

      ;; 1. all types imported with using
      (setq scoped-types (mapcar 'semantic-tag-type imported-types))

      ;; 2. all types in the same namespace.
      ;; TODO I think this will need a lookup.

      ;; 3. TODO
      scoped-types)))

;; TODO I need to investigate why %scopestart fails to provide
;; bovine-inner-scope so that semantic-get-all-local-variables-default
;; can work with a simple get-local-variables.
(define-mode-local-override semantic-get-local-variables php-mode (&optional point)
  "Get the local variables based on POINT's context.
Local variables are returned in Semantic tag format."
  ;; Disable parsing messages
  (let ((semantic--progress-reporter nil))
    (save-excursion
      (if point (goto-char point))
      (let ((semantic-unmatched-syntax-hook nil)
            (start (point))
            (firstusefulstart nil)
            vars
            parent-tag)
        (while (not (semantic-up-context (point) 'function))
          (when (not vars)
            (setq firstusefulstart (point)))
          (save-excursion
            (forward-char 1)

            (setq parent-tag (semantic-tag-calculate-parent (semantic-current-tag)))

            ;; Check if we're in a method.
            (when (and parent-tag
                       (semantic-tag-of-class-p parent-tag 'type)
                       (member (semantic-tag-type parent-tag) '("class" "interface" "trait")))

              (push (semantic-tag-new-variable "$this" parent-tag) vars)
              (push (semantic-tag-new-variable "static" parent-tag) vars)
              (push (semantic-tag-new-variable "self" parent-tag) vars)

              (when (semantic-tag-type-superclasses parent-tag)
                (push (semantic-tag-new-variable "parent" (car (semantic-tag-type-superclasses parent-tag))) vars)))

            ;; TODO these tags probably all need :filename
            (setq vars (append
                        (semantic-parse-region (point)
                                               (save-excursion (semantic-end-of-context) (point))
                                               'local_variables
                                               nil
                                               t)
                        vars))))

        ;; (message "Parsed variables [first useful start %d]" firstusefulstart)
        ;; (pp vars)
        ;; Hash our value into the first context that produced useful results.
        (when (and vars firstusefulstart)
          (let ((end (save-excursion
                       (goto-char firstusefulstart)
                       (save-excursion
                         (unless (semantic-end-of-context)
                           (point))))))
            ;; (message "Caching values %d->%d." firstusefulstart end)
            (semantic-cache-data-to-buffer
             (current-buffer) firstusefulstart
             (or end
                 ;; If the end-of-context fails,
                 ;; just use our cursor starting
                 ;; position.
                 start)
             vars 'get-local-variables 'exit-cache-zone))
          )
        ;; Return our list.
        vars))))

(define-mode-local-override semantic-find-tags-included php-mode (&optional table)
  "Find all tags in TABLE that are namespaces or of the 'include and class.
TABLE is a tag table.  See `semantic-something-to-tag-table'."
  ;; TODO limit to current 'scope'
  (unless table (setq table (current-buffer)))
  (semantic-find-tags-by-class 'include (semantic-flatten-tags-table table)))

(defun semantic-php-name-nonnamespace (name)
  "Returns the non-namespace part of NAME."
  (car (last (split-string name "\\\\"))))

(defun semantic-php-name-fully-qualified-p (name)
  ""
  (equal "\\" (substring name 0 1)))

(defun semantic-php-tag-expand (tag)
  ""
  ;; Until another solution is found, remove leading dollar sign from
  ;; instance attributes. This will allow autocompletion in from of a
  ;; $instance-> prefix.
  (when (and (semantic-tag-of-class-p tag 'variable)
             (not (member "static" (semantic-tag-modifiers tag)))
             (member (semantic-tag-protection tag) '(public private protected)))
    (semantic-tag-set-name tag (substring (semantic-tag-name tag) 1)))

  ;; Split the compound using and include statements.
  (when (semantic-tag-of-class-p tag 'using)
    ;; TODO use split name function to return the cell
    ;; (namespace . symbol-name).
    (let* ((using-tag tag)
           (type-tag (semantic-tag-get-attribute using-tag :type))
           (qualified-name (semantic-tag-name type-tag))
           (symbol-alias (semantic-php-name-nonnamespace (semantic-tag-name using-tag)))
           (include-tag (semantic-tag-new-include qualified-name nil)))

      ;; Change the name of the using tag to be the local name
      ;; of the imported class, or its alias.
      ;;
      ;; NOTE We could split qualified names (namespace . symbol) in
      ;; the grammar or using the semantic-analyze-split-name
      ;; function.
      (semantic-tag-set-name using-tag symbol-alias)

      (semantic-tag-set-bounds include-tag (semantic-tag-start using-tag) (semantic-tag-end using-tag))
      (list using-tag include-tag))))

;;;###autoload
(defun grammar-setup ()
  "Setup a new grammar to process PHP buffers using Semantic."

  (grammar--install-parser)

  (setq semantic-tag-expand-function 'semantic-php-tag-expand)
  (setq semantic-lex-analyzer #'grammar-lexer)

  ;; TODO: Don't ignore comment to allow docblock to scan types?
  ;; Separators to use when finding context prefix
  (setq parse-sexp-ignore-comments t
        semantic-type-relation-separator-character '("::" "->" "\\")
        semantic-command-separation-character ";")

  (setq semanticdb-find-default-throttle
        '(file unloaded system omniscience))

  (setq semantic-symbol->name-assoc-list-for-type-parts
        '((type     . "Classes")
          (variable . "Attributes")
          (function . "Methods")
          (using    . "Using")
          (include  . "Imports")))

   (setq semantic-symbol->name-assoc-list
         '((package  . "Provides")
           (using    . "Using")
           (include  . "Imports")))

   ;; navigation inside 'type children
   (setq senator-step-at-tag-classes '(function variable type)
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

(provide 'grammar-setup)

;;; setup.el ends here
