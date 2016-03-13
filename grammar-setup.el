;;; setup.el --- An iterative style parser for PHP 5.5 buffers

;;; Commentary:
;;
;; This file is used to configure and install a tagging PHP parser
;; that extracts crucial information about the symbols defined and
;; used in a php buffer.
;;
;;
;; TODO
;;
;; - BUG type hints with "mixed" type cause a new type to be recorded as a
;; member. We need to ignore these.
;; - ??? I'm yet unsure about the usability of the name splitting function (ns . type)
;; - inline completion still won't work unless a .hh or .cpp file is opened, this
;; indicates that I need to override some more functions. This is most likely caused by the
;; use of a metatype tag; did this happen with alias tags too?
;; - dequalify scoped types
;;

;;; Code:

(require 'semantic/wisent)
(require 'grammar)

;; to help with debugging.
(require 'semantic/analyze/debug)
(require 'semantic/db-debug)
(require 'data-debug)

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
  "Split up tag NAME into multiple components."
  (let (nameparts (semantic-php-name-parts name))
    (message "semantic-analyze-split-name: [%s => %s]" nameparts)
    (if (null (car nameparts))
        (list "\\" (cdr nameparts))
      (list (car nameparts) (cdr nameparts)))))

(define-mode-local-override semantic-analyze-unsplit-name php-mode (nameparts)
  "Joins the two parts of split NAME into a qualified name."
  (if (string-equal (car nameparts) "\\")
      (concat "\\" (cdr nameparts))
    (concat (car nameparts) "\\" (cdr nameparts))))

;; (define-mode-local-override semanticdb-find-table-for-include php-mode (includetag &optional table)
;;   "For a single INCLUDETAG found in TABLE, find a `semanticdb-table' object
;; INCLUDETAG is a semantic TAG of class 'include.
;; TABLE is a semanticdb table that identifies where INCLUDETAG came from.
;; TABLE is optional if INCLUDETAG has an overlay of :filename attribute."
;;   ;; Generic way of looking up a class.
;;   ;;
;;   ;; Are we in the same namespace or package?
;;   ;; If we are in a namespace, does the namespace part of the include
;;   ;; match the current namespace? If yes we might be able to find our
;;   ;; way around the file system without knowing anything else, elthough
;;   ;; we should rely on ede.
;;   (message "semanticdb-find-table-for-include: searching tag table")
;;   (if (and (featurep 'ede-php-autoload) (ede-current-project))
;;       (semantic-file-tag-table (ede-php-autoload-find-class-def-file (ede-current-project) (car includetag)))))

(define-mode-local-override semantic-ctxt-scoped-types php-mode (&optional point)
  "Return a list of type names currently in scope at POINT.
The return value can be a mixed list of either strings (names of
types that are in scope) or actual tags (type declared locally
that may or may not have a name.)"
  ;; NOTE seems that the original version of this function should
  ;; handle nested definitions. See semantic-ctxt-scoped-types c++-mode in c.el
  (when point (goto-char point))

  (message "semantic-ctxt-scoped-types: local type analysis")
  ;; (when point (goto-char point))
  (let* ((table (semantic-find-tag-by-overlay))
         (parent-tag (car (cdr (nreverse table))))
         ;; NOTE this line assumes table is always a namespace.
         ;;
         ;; TODO I have considered always providing a top-level
         ;; namespace, but decided not to walk that route just yet.
         ;;
         ;; The next change I'll be making here is probably
         ;; improving the top-level initialisation code above.
         (importrules (semantic-find-tags-by-class 'using (semantic-flatten-tags-table table)))

         scoped-types)

    ;; 1. all types imported with using
    ;; (setq scoped-types (mapcar 'semantic-tag-type imported-types))
    ;; (message "semantic-ctxt-scoped-types: using tags")
    ;; (pp importrules)
    (setq scoped-types importrules)

    ;; (message "semantic-ctxt-scoped-types: imported types (remember to dequalify)")
    ;; (pp scoped-types)

    ;; 2. all types implicitly imported
    ;; TODO I think this will need a lookup for types in the
    ;; same namespace.
    ;; TODO Check this is the right place for this operation.

    ;; 3. TODO
    scoped-types))

;; EXPERIMENT: Attempt to find a place to qualify names in code blocks
;; with respect to the import rules using alias tags.
(define-mode-local-override semantic-analyze-dereference-metatype php-mode
  (type scope &optional type-declaration)
  "Return a concrete type tag based on input TYPE tag.

A concrete type is an actual declaration of a memory description,
such as a class, interface, constant, or function.  A meta type
in PHP are partially qualified name, and names imported with a
use declaration.

If TYPE is concrete, it is returned.  If it is a meta type, it
will return the concrete type defined by TYPE.

Just a name, or short tag will be ok.
SCOPE is the scope object with additional items in which to search for names."
  (message "semantic-analyze-dereference-metatype: dereferencing")
  (cond
   ;; The parser adds the `namespace' attribute to tags under
   ;; a namespace, so we can use this information to fully
   ;; qualify them without lookups.
   ((and (semantic-tag-p type)
         (semantic-tag-p type-declaration)
         (semantic-tag-get-attribute type :namespace))
    (semantic-php-dereference-type-under-namespace type scope type-declaration))

   ;; Dereference a type used in an assignment, for example:
   ;; $d = new \StdClass();
   ;; $d->.
   ((and (semantic-tag-p type-declaration)
         (or (null type) (semantic-tag-prototype-p type)))
    (semantic-php-dereference-type-declaration type scope type-declaration))

   ;; Couldn't handle the dereference.
   (t (message "semantic-analyze-dereference-metatype: dunno.")
      (list type type-declaration))))

(defun semantic-php-dereference-type-under-namespace (type scope &optional type-declaration)
  "Dereference a partially qualified name under a namespace."
  (let ((scopetypes (oref scope scopetypes))
	typeparts typename currentns)

      (setq currentns (semantic-tag-get-attribute type :namespace))
      (setq typename (semantic-php--resolve-symbol-for-tag (semantic-tag-name type) currentns))

      ;; (message "TODO Fix name parts") ("\\" . nil)/nil
      ;; (setq typeparts (semantic-analyze-split-name typename))
      (message "semantic-php-dereference-type-under-namespace: Dereferenced [%s => %s]."
               (car type)
               typename)

      (list type type-declaration)))

;; As a sidenote, it might still be worth investigating the
;; semantic-c-dereference-member-of which interestingly enough uses
;; "->" named tags for something. Not sure what for, but worth having
;; a look.
(defun semantic-php-dereference-type-declaration (type scope &optional type-declaration)
  ""
  (let ((scopetypes (oref scope scopetypes))
	typeparts typename result importrule currentns)

      (setq typename (semantic-tag-name type-declaration))
      (setq typeparts (semantic-analyze-split-name typename))
      (setq currentns (car (semantic-find-tag-by-overlay)))

      (if (semantic-php-name-fully-qualified-p typename)
          (progn (message "semantic-php-dereference-type: Fully qualified name [%s] (type: %s)" typename type)
                 (setq result (list type type-declaration)))

        (message "semantic-php-dereference-type: import lookup loop, started.")
        (dolist (scopetype scopetypes result)
          (setq importrule (semantic-tag-type scopetype))
          ;; Lookup the typename in the list of types registered in
          ;; the local scope.
          (when (string-equal (semantic-php-name-nonnamespace (semantic-tag-name scopetype)) typename)
            (message "semantic-php-dereference-type: Found import rule [%s => %s]"
                     typename
                     (semantic-tag-name importrule))
            ;; (message "import rule")
            ;; (pp importrule)
            ;; (message "import rule type/prototype")
            (setq type (car (semantic-tag-get-attribute importrule :members)))
            (message "semantic-php-dereference-type: Dereferenced [%s => %s]"
                     typename
                     type)
            (setq result (list type type-declaration))
            ))
        (message "semantic-php-dereference-type: import lookup loop, done.")
        )

      (unless result
        (message "semantic-php-dereference-type: no import rule found for %s" typename))

    (if result
        (list result result)
      (list type type-declaration))))

(defun semantic-php-dereference-type-alias (type namespace)
  "Dereference TYPE in NAMESPACE, given that NAMESPACE is an alias.
Checks if NAMESPACE is an alias and if so, returns a new type
with a fully qualified name in the original namespace.  Returns
nil if NAMESPACE is not an alias."
  (message "semantic-php-dereference-type-alias: Qualifying type %s in NS %s" type namespace)
  (when (eq (semantic-tag-get-attribute namespace :kind) 'alias)
    (let ((typename (semantic-analyze-split-name (semantic-tag-name type)))
	  ns nstype originaltype newtype)
      ;; Make typename unqualified
      (if (listp typename)
	  (setq typename (last typename))
	(setq typename (list typename)))
      (when
	  (and
	   ;; Get original namespace and make sure TYPE exists there.
	   (setq ns (semantic-tag-name
		     (car (semantic-tag-get-attribute namespace :members))))
	   (setq nstype (semanticdb-typecache-find ns))
	   (setq originaltype (semantic-find-tags-by-name
			       (car typename)
			       (semantic-tag-get-attribute nstype :members))))
	;; Construct new type with name in original namespace.
	(setq ns (semantic-analyze-split-name ns))
	(setq newtype
	      (semantic-tag-clone
	       (car originaltype)
	       (semantic-analyze-unsplit-name
		(if (listp ns)
		    (append ns typename)
		  (append (list ns) typename)))))))))

(define-mode-local-override semantic-tag-protection
  php-mode (tag &optional parent)
  "Return protection information about TAG with optional PARENT.
This function returns on of the following symbols:
   'public    - Anyone can access this TAG.
   'private   - Only methods in the local scope can access TAG.
   'protected - Like private for outside scopes, like public for child
                classes.

This is overridden to avoid interaction with Semantic C and PHP."
  (cond
   ((member "public" (semantic-tag-modifiers tag))
    'public)
   ((member "private" (semantic-tag-modifiers tag))
    'private)
   ((member "protected" (semantic-tag-modifiers tag))
    'protected)))

(define-mode-local-override semantic-tag-alias-definition php-mode (tag)
  "Return the definition TAG is an alias.
The returned value is a tag of the class that
`semantic-tag-alias-class' returns for TAG.
The default is to return the value of the :definition attribute.
Return nil if TAG is not of class 'alias."
  (message "semantic-tag-alias-definition: Alias definition for tag?" (car tag)))

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
            (setq vars (append (semantic-parse-region (point)
                                               (save-excursion (semantic-end-of-context) (point))
                                               'local_variables
                                               nil
                                               t) vars))))

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
  ;; TODO limit to current 'scope', also a better alternative to
  ;; flattening the tag table, since we could simply get the outermost
  ;; tag (and handle global namespace separately, until I figure out
  ;; whether or not I'll always wrap tags in a namespace, i.e. even
  ;; with \\\\)
  (unless table (setq table (current-buffer)))
  (semantic-find-tags-by-class 'include (semantic-flatten-tags-table table)))

(defun semantic-php-name-nonnamespace (name)
  "Returns the non-namespace part of NAME."
  (cdr (semantic-php-name-parts name)))

(defun semantic-php-name-namespace (name)
  "Returns the namespace part of NAME."
  (car (semantic-php-name-parts name)))

(defun semantic-php-name-parts (name)
  "Splits a type name into a cell (namespace . type).
Qualified names will have non-nil namespace."
  (let* ((parts (nreverse (remq "" (split-string name "\\\\"))))
         (namespace (nreverse (cdr parts)))
         (type (car parts)))
    (cond
     ((not parts) nil)
     (namespace
      (cons (mapconcat 'identity namespace "\\") type))
     ((not namespace)
      (cons (if (semantic-php-name-fully-qualified-p name) "\\" nil) type)))))

(defun semantic-php-name-fully-qualified-p (name)
  ""
  (when (and (stringp name) (> (length name) 1))
    (string-equal "\\" (substring name 0 1))))

(defun semantic-php-tag-expand-using (using-tag)
  "Expand a PHP use declaration into new tags to aid name resolution.

Two new tags are produced whenever the parser produces a `using', these
new tags are of type `alias' and `include'.

The `include' tags produced by this expansion function are used to
associate the buffer with explicit external dependencies.

The `alias' tags are used to associate the last part of the qualified
name to its local name. In the case of aliased PHP use declarations,
the local name will match that alias, e.g. the following PHP code:

use My\\Ns\\SomeClass;
use My\\Ns\\AnotherClass as AliasedClass;

will produce the following tags:
- alias `SomeClass' and
- alias `AliasedClass'
- include `My\\Ns\\SomeClass'
- include `My\\Ns\\AnotherClass'"
  (let* ((symbol-alias (semantic-tag-name using-tag))
         (type-tag (semantic-tag-get-attribute using-tag :type))
         (fully-qualified-name (semantic-tag-name type-tag))

         ;; Mmh, Let's produce a new type tag to link the local
         ;; type to the fully qualified name.
         ;;
         ;; TODO Maybe this isn't needed considering the dereference
         ;; function. I need to dig that conversation between Eric
         ;; L. and David E.  concerning the use of metatypes to
         ;; support the analyzer. I think this simplifies the process
         ;; of determining scoped types, however this may negatively
         ;; affect performance.
         ;;
         ;; NOTE Changes to this alias will probably require changes to the
         ;; functions:
         ;;
         ;; 1. semantic-ctxt-scoped-types
         ;; 2. semantic-analyze-dereference-type
         (alias-tag (semantic-tag-new-alias symbol-alias 'type fully-qualified-name))
         (include-tag (semantic-tag-new-include fully-qualified-name nil)))

    (message "semantic-php-tag-expand-using: expanded imported type [%s => %s]"
             symbol-alias
             fully-qualified-name)

    (semantic-tag-set-bounds include-tag (semantic-tag-start using-tag) (semantic-tag-end using-tag))
    (list using-tag include-tag alias-tag)))

(defun semantic-php-tag-expand (tag)
  ""
  ;; (message "semantic-php-tag-expand: expanding tag %s" (car tag))

  ;; Until another solution is found, remove leading dollar sign from
  ;; instance attributes. This will allow autocompletion in from of a
  ;; $instance-> prefix.
  ;;
  ;; TODO Don't change static attributes, those must be completed with
  ;; the $ prefix.
  (when (and (semantic-tag-of-class-p tag 'variable)
             (not (member "static" (semantic-tag-modifiers tag)))
             (member (semantic-tag-protection tag) '(public private protected)))
    (semantic-tag-set-name tag (substring (semantic-tag-name tag) 1)))

  ;; Split the compound using and include statements.
  ;;
  ;; TODO handle variables(constants), functions and classes/namespaces
  ;; accordingly.
  (when (semantic-tag-of-class-p tag 'using)
    (semantic-php-tag-expand-using tag))
  )

;;;###autoload
(defun grammar-setup ()
  "Setup a new grammar to process PHP buffers using Semantic."

  (grammar--install-parser)

  (setq semantic-tag-expand-function 'semantic-php-tag-expand)
  (setq semantic-lex-analyzer #'grammar-lexer)

  (setq-mode-local php-mode
                   semanticdb-find-default-throttle
                   '(project unloaded system recursive))

  (semantic-add-system-include "/Users/andreaturso/Dev/my-semantic-php/system")

  ;; TODO: Don't ignore comment to allow docblock to scan types?
  ;; Separators to use when finding context prefix
  (setq parse-sexp-ignore-comments t
        semantic-type-relation-separator-character '("::" "->" "\\")
        semantic-command-separation-character ";")

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
   (setq senator-step-at-tag-classes '(function variable type))
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
