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

(define-child-mode php-mode nil)

;; (defun semantic-php-init-parser-context ()
;;   "Initialize context of the LR parser engine.
;; Used as a local `wisent-pre-parse-hook' to cleanup the stack of imported symbols."
;;   (setq semantic-php-cache--namespaces nil))

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

(define-mode-local-override semantic-ctxt-scoped-types php-mode (&optional point)
  ""
  ;; TODO filter types to those imported in the current namespace only
  ;; or keep this information buffer-wise?
  (let* ((table (current-buffer))
         (tags (semantic-find-tags-by-class 'using (semantic-flatten-tags-table table)))
         scoped-types)
    (dolist (tag tags scoped-types)
      (push (semantic-tag-name tag) scoped-types))))

(define-mode-local-override semantic-ctxt-imported-packages php-mode (&optional point)
  ""
  (message "Imported pckage analysis")
  (save-excursion
    (when point (goto-char point))
    (let ((parent-tag (car (cdr (nreverse (semantic-find-tag-by-overlay)))))
          (aliased-names (semantic-find-tags-by-class 'using (semantic-flatten-tags-table (current-buffer)))))
      (pp aliased-names)
      (remq nil (list "\\"
                      ;; all using statements are implied packages
                      ;; TODO limit this to S_NS_SCOPE at point.

                      ;; the current namespace is an implied package
                      (when (string-equal "namespace" (semantic-tag-type parent-tag))
                        (semantic-tag-name parent-tag)))))))

(define-mode-local-override semantic-ctxt-scoped-types php-mode (&optional point)
  ""
  nil)

(define-mode-local-override semantic-get-local-variables php-mode (&optional point)
  ""
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

            ;; Check if we're in a method.
            (setq parent-tag (semantic-tag-calculate-parent (semantic-current-tag)))
            (when (and parent-tag
                       (semantic-tag-of-class-p parent-tag 'type)
                       (member (semantic-tag-type parent-tag) '("class" "interface")))
              (push (semantic-tag-new-variable "$this" parent-tag) vars)
              (push (semantic-tag-new-variable "static" parent-tag) vars)
              (push (semantic-tag-new-variable "self" parent-tag) vars)
              (push (semantic-tag-new-variable "parent" (car (semantic-tag-type-superclasses parent-tag))) vars)
              (message "In method"))

            ;; TODO handle cached variables.
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
            (message "Caching values %d->%d." firstusefulstart end)
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
  (unless table (setq table (current-buffer)))
  (semantic-find-tags-by-class 'include (semantic-flatten-tags-table table)))

(defun semantic-php-name-nonnamespace (name)
  "Returns the non-namespace part of NAME."
  (car (last (split-string name "\\\\"))))

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
