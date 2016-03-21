;;; setup.el --- An iterative style parser for PHP 5.5 buffers
;;; Commentary:
;;

;;; Code:

(require 'semantic/wisent)
(require 'semantic/analyze)
(require 'semantic/db-find)

;; to help with debugging.
(require 'semantic/analyze/debug)
(require 'semantic/db-debug)
(require 'data-debug)

;; Import the PHP grammar automaton.
(require 'grammar)

(define-mode-local-override semantic-tag-protection
  php-mode (tag &optional parent)
  "Return protection information about TAG with optional PARENT.
This function returns on of the following symbols:
   'public    - Anyone can access this TAG.
   'private   - Only methods in the local scope can access TAG.
   'protected - Like private for outside scopes, like public for child
                classes."

  (let ((access (or (member "public" (semantic-tag-modifiers tag))
                    (member "private" (semantic-tag-modifiers tag))
                    (member "protected" (semantic-tag-modifiers tag)))))
    (if access
      (intern (car access)))))

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
      (error "Could not find source file for [%s]" (semantic-tag-name tag))
      class-name)))

(define-mode-local-override semantic-analyze-split-name php-mode (name)
  "Split up tag NAME into multiple components."
  (let* ((nameparts (and (stringp name) (semantic-php-name-parts name)))
         (namespace (car nameparts))
         (typename (cdr nameparts)))
    (cond
     ;; In case something called this function with a nil.
     ((null nameparts)
      (error "semantic-analyze-split-name-php-mode: Nil name provided."))

     ;; The doesn't contain any namespace separators.
     ((null (car nameparts)) name)

     ((and (stringp namespace)
           (string-match-p "^\\\\\\w+" namespace))
      ;; Fully qualified names.
      (if (equal "\\" namespace)
          typename
        (list namespace typename)))

     (t
      ;; Partially qualified names.
      (list namespace typename)))))

(define-mode-local-override semantic-analyze-unsplit-name php-mode (nameparts)
  "Joins the two parts of split NAME into a qualified name."
  (let* ((namespace (car nameparts))
         (typename (cadr nameparts)))
    (if (null namespace)
        typename
      (concat namespace "\\" typename))))

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
      (let ((file-name
             (ede-php-autoload-find-class-def-file
              (ede-current-project)
              (semantic-tag-name includetag))))
        (message "semanticdb-find-table-for-include: [%s] candidate file [%s]" (car includetag) file-name)
        (semantic-file-tag-table file-name))
    (message "semanticdb-find-table-for-include: falling back to default fcn.")
    (semanticdb-find-table-for-include-default includetag table)))

;; TAKEN VERBATING FROM c.el
(define-mode-local-override semanticdb-find-table-for-include php-mode
  (includetag &optional table)
  "NOTE: TAKEN FROM c.el

For a single INCLUDETAG found in TABLE, find a `semanticdb-table' object
INCLUDETAG is a semantic TAG of class 'include.
TABLE is a semanticdb table that identifies where INCLUDETAG came from.
TABLE is optional if INCLUDETAG has an overlay of :filename attribute.

For C++, we also have to check if the include is inside a
namespace, since this means all tags inside this include will
have to be wrapped in that namespace."
  (let ((inctable (semanticdb-find-table-for-include-default includetag table))
	(inside-ns (semantic-tag-get-attribute includetag :inside-ns))
	tags newtags namespaces prefix parenttable newtable)
    (if (or (null inside-ns)
	    (not inctable)
	    (not (slot-boundp inctable 'tags)))
	inctable
      (when (and (eq inside-ns t)
		 ;; Get the table which has this include.
		 (setq parenttable
		       (semanticdb-find-table-for-include-default
			(semantic-tag-new-include
			 (semantic--tag-get-property includetag :filename) nil)))
		 table)
	;; Find the namespace where this include is located.
	(setq namespaces
	      (semantic-find-tags-by-type "namespace" parenttable))
	(when (and namespaces
		   (slot-boundp inctable 'tags))
	  (dolist (cur namespaces)
	    (when (semantic-find-tags-by-name
		   (semantic-tag-name includetag)
		   (semantic-tag-get-attribute cur :members))
	      (setq inside-ns (semantic-tag-name cur))
	      ;; Cache the namespace value.
	      (semantic-tag-put-attribute includetag :inside-ns inside-ns)))))
      (unless (semantic-find-tags-by-name
	       inside-ns
	       (semantic-find-tags-by-type "namespace" inctable))
	(setq tags (oref inctable tags))
	;; Wrap tags inside namespace tag
	(setq newtags
	      (list (semantic-tag-new-type inside-ns "namespace" tags nil)))
	;; Create new semantic-table for the wrapped tags, since we don't want
	;; the namespace to actually be a part of the header file.
	(setq newtable (semanticdb-table "include with context"))
	(oset newtable tags newtags)
	(oset newtable parent-db (oref inctable parent-db))
	(oset newtable file (oref inctable file)))
      newtable)))

(define-mode-local-override semantic-ctxt-scoped-types php-mode (&optional point)
  "Return a list of type names currently in scope at POINT.
The return value can be a mixed list of either strings (names of
types that are in scope) or actual tags (type declared locally
that may or may not have a name.)"
  ;; NOTE seems that the original version of this function should
  ;; handle nested definitions. See semantic-ctxt-scoped-types c++-mode in c.el
  (if point (goto-char point))

  (let* (;; If we're in a namespace (semantic-php--guess-current-ns)
         ;; will return a partially qualified name, or "\\" if we're
         ;; in the global namespace.
         ;;
         ;; TODO handle the global namespace.
         (currentns (semantic-php--guess-current-ns))
         returntags)

    ;; 1. all types declared in namespaces within this buffer.
    (setq tmp (semantic-find-tags-by-class 'type (current-buffer)))
    (setq tmp (semantic-find-tags-by-type "namespace" tmp))
    (setq returntags tmp)
    ;; (message "semantic-ctxt-scoped-types: namespaces in this file [%s]" tmp)

    ;; 2. ll types imported and aliased with using.
    (setq tmp (semantic-find-tags-by-class 'using (semantic-flatten-tags-table currentns)))
    (setq returntags
          (append returntags
                  (mapcar 'semantic-tag-type tmp)))

    ;; (message "semantic-ctxt-scoped-types: types imported in this file [%s]" (mapcar 'semantic-tag-type tmp))

    ;; 2. all types implicitly imported
    ;;
    ;; a) types in the same namespace
    ;; b) types in imported namespace
    returntags))

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
  ;; NOTE this entire dereference list logic isn't needed
  ;; and I'm only using to test multiple dereferencers.
  ;; This function could use heavy simplification.
  (let* ((dereferencer-list '(semantic-php-dereference-metatype))
         (dereferencer (pop dereferencer-list))
         (type-tuple)
         (original-type type)
         (tmp type-declaration)) ;; tmp used to print debug.

    (while dereferencer
      (setq type-tuple (funcall dereferencer type scope type-declaration)
            type (car type-tuple)
            type-declaration (cadr type-tuple))
      (if (not (eq type original-type))
          ;; we found a new type so break the dereferencer loop now !
          ;; (we will be recalled with the new type expanded by
          ;; semantic-analyze-dereference-metatype-stack).
          (setq dereferencer nil)
        ;; no new type found try the next dereferencer :
        (setq dereferencer (pop dereferencer-list))))

    (when (and (or type type-declaration) (not (eq type original-type)))
      (ignore-errors
        (message "Dereferenced metatype [%s => %s]"
                 (or (car-safe original-type) (format "<declaration:%s>" tmp))
                 (car-safe type))))

    (list type type-declaration)))

(defun semantic-php-dereference-metatype (type scope &optional type-declaration)
  "Finds a concrete type corresponding to the metatype TYPE.
This dereferenced works only when TYPE-DECLARATION is provided, which means
it's resolving a name used in a parameter or return type hint, an assignment,
but not a use statement."
  (let ((scopetypes (oref scope scopetypes))
        currentns namespaces typename result importrules)

    (when (and (semantic-tag-p type-declaration)
               (semantic-tag-of-class-p type-declaration 'metatype))
      (setq typename (semantic-analyze-split-name (semantic-tag-name type-declaration)))
      (when (stringp typename)
        (setq typename (list typename)))

      ;; (car typename) will be either the non-qualified name, or the
      ;; namespace part of a partially qualified name.
      (if (or (null (car typename))
              (not (string-match-p "^\\\\\\w+" (car typename))))
          ;; Name not qualified at all, or its namespace part is not
          ;; fully qualified (i.e. it does not begin with \\).

          (progn (setq typename (car (last typename)))
                 (message "Preparing lookup up P_QN [%s] in SCOPE imported types." typename)
                 (setq namespaces (semantic-find-tags-by-class 'type scopetypes))
                 (setq namespaces (remove nil
                             (mapcar (lambda (ct)
                                       (if (equal (semantic-tag-type ct) "namespace") ct))
                                     namespaces))))

        ;; So we only have to search one namespace.
        (message "Preparing lookup of F_QN [%s] in TYPECACHE." (car typename))
        (error "^ FIXME unchecked path.")
        (setq namespaces (semanticdb-typecache-find typename))

        ;; Make sure it's really a namespace.
        (if (string= (semantic-tag-type namespaces) "namespace")
            (setq namespaces (list namespaces))
          (setq namespaces nil)))

        (setq result nil)

        ;; Iterate over all the namespaces we have to check.
        (while (and namespaces
                    (null result))
          (setq currentns (car namespaces))

          (setq result (semantic-php-dereference-imported-type type-declaration currentns))

          (unless result
            (unless importrules
              (setq importrules (semantic-flatten-tags-table (current-buffer)))
              (setq importrules (remove nil
                                        (mapcar (lambda (ct)
                                                  (if (semantic-tag-of-class-p ct 'using) ct))
                                                  importrules))))
            (message "semantic-php-dereference-metatype: Checking imported name [%s]" typename)
            (setq result (semantic-php-dereference-import-rules typename importrules)))

          (unless result
              (message "semantic-php-dereference-metatype: No matches yet [%s]" typename))

          (setq namespaces (cdr namespaces))
        )
      )

    (if result
        (list result result)
      (list type type-declaration))))

(defun semantic-php-dereference-imported-type (type namespace)
  "Finds the concrete type matching TYPE the import rules in NAMESPACE.
This is not a dereferencer in and on itself, rather a support
a dereferencer function."
  (let ((typename (semantic-analyze-split-name (semantic-tag-name type)))
        (table (semantic-flatten-tags-table namespace))
        localname importname importtype importfound)

    ;; NOTE this is a nace way of dequalifying the name, which
    ;; I think I should reuse wherever I used (car (last name))
    (when (listp (setq localname (semantic-analyze-split-name
                                  (semantic-tag-name type))))
      (setq localname (car (last localname))))

    (setq importrules (semantic-find-tags-by-name localname table))
    (setq importrules (remove nil (mapcar
                                   (lambda (ct)
                                     (if (semantic-tag-of-class-p ct 'using)
                                         ct))
                                   importrules)))

    (semantic-php-dereference-import-rules localname importrules)))

(defun semantic-php-dereference-import-rules (localname importrules)
  "Finds the concrete type matching TYPE in IMPORTRULES.
This is not a dereferencer in and on itself, rather a support
a dereferencer function."
  (let (importname importtype importfound)
    (while (and (null importfound) importrules)
      (setq importname (semantic-analyze-split-name
                        (semantic-tag-name (car importrules)))
            importtype (semantic-tag-type (car importrules)))

      (message "Analysing [%s] against imported type [%s => %s (%s %s)]"
               localname
               importname
               (car importtype)
               (semantic-tag-class importtype)
               (semantic-tag-type importtype))

      (unless (listp importname)
        (setq importname (list importname)))

      (if (and (equal localname (car (last importname)))
               (member (semantic-tag-type importtype) (list "class")))
          (setq importfound importtype)
        (cond
         ((equal localname (car (last importname)))
          (setq importfound (semanticdb-typecache-find (car (last importtype))))

          (if importfound
              (message "semantic-php-dereference-import-rules: found import [%s => %s]" localname importname)
            (message "semantic-php-dereference-import-rules: bail out, can't find [%s => %s]" localname importname))
          )

         (t (message "semantic-php-dereference-import-rules: can't handle [%s => %s]" localname importname))))
      (setq importrules (cdr importrules)))
    (message "semantic-php-dereference-import-rules: returning with [%s]" importfound)
    importfound))

(defun semantic-php-name-nonnamespace (name)
  "Returns the non-namespace part of NAME."
  (cdr (semantic-php-name-parts name)))

(defun semantic-php-name-namespace (name)
  "Returns the namespace part of NAME."
  (car (semantic-php-name-parts name)))

(defun semantic-php-name-parts (name)
  "Splits a type name into a cell (namespace . type).
Qualified names will have non-nil namespace."
  (let* ((parts (nreverse (split-string name "\\\\")))
         ;; removes fqn: (parts (nreverse (remq "" (split-string name "\\\\"))))
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
- include My\\Ns\\SomeClass
- include My\\Ns\\AnotherClass
- using SomeClass
- using AliasClass
- metatype SomeClass
- metatype AliasedClass"
  (let* ((type-tag (semantic-tag-get-attribute using-tag :type))
         ;; fq-type-name is the part after the PHP `use' keyword and
         ;; the following semicolon or `as' keyword.
         (fq-type-name (semantic-tag-name type-tag))
         ;; local-type-name is collected by the parser and it's either
         ;; the last part of a qualified name, or the alias after the
         ;; PHP `as' keyword. This will be the name of the type in the
         ;; local scope.
         (local-type-name (semantic-tag-name using-tag))
         ;; actual-type-name is the last part of the fully
         ;; qualified name before the `as' keyword. We'll test this
         ;; value against local-type-name to know when imported types
         ;; are getting aliased.
         (actual-type-name (semantic-php-name-nonnamespace fq-type-name))
         ;; TEST: Produce a new `alias' tag to link the local type to the
         ;; fully qualified name.
         ;;
         ;; TODO Maybe this isn't needed considering the dereference
         ;; function. I need to dig that conversation between Eric
         ;; L. and David E.  concerning the use of metatypes to
         ;; support the analyzer. I think this simplifies the process
         ;; of determining scoped types, however this may negatively
         ;; affect performance.
         ;;
         ;; NOTE Changes to the data in this tag will probably require
         ;; changes to the functions:
         ;;
         ;; 1. semantic-ctxt-scoped-types
         ;; 2. semantic-analyze-dereference-type
         (alias-tag (semantic-tag-new-alias local-type-name 'type fq-type-name))

         ;; Produce a new `include' tag to hint Semantic of the external
         ;; dependency.
         (include-tag (semantic-tag-new-include fq-type-name nil)))
    (semantic-tag-set-bounds include-tag (semantic-tag-start using-tag) (semantic-tag-end using-tag))
    (list using-tag include-tag alias-tag)))

(defun semantic-php-tag-expand (tag)
  ""
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
    (semantic-php-tag-expand-using tag)))

(defun semantic-php-current-ns-name (&optional point)
  "Guess the PHP namespace at point, or POINT if it is given."
  (let ((currentns (semantic-php--guess-current-ns point)))
    (if currentns
        currentns
      "\\")))

;; TODO Will need update once I'm decided whether the global namespace
;; gets its own tag or a scope.
(defun semantic-php--guess-current-ns (&optional point)
  "Guess the PHP namespace at point, or POINT if it is given.
Returns the namespace tag, or nil when it's hard to guess or the
point is in the global namespace."
  (if point (goto-char point))
  (let ((table (or (semantic-find-tag-by-overlay) (current-buffer)))
        outermost)
    (when (listp table)
      (setq outermost (car (cdr (nreverse table)))))
    (if (and (semantic-tag-p outermost)
             (equal (semantic-tag-type outermost) "namespace"))
        outermost
      (unless (semantic-tag-get-attribute outermost :namespace)
        outermost))
    outermost))

;;;###autoload
(defun grammar-setup ()
  "Setup a new grammar to process PHP buffers using Semantic."

  (grammar--install-parser)

  (setq semantic-tag-expand-function 'semantic-php-tag-expand)
  (setq semantic-lex-analyzer #'grammar-lexer)

  (setq-mode-local php-mode
                   semanticdb-find-default-throttle
                   '(project unloaded system recursive))

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

   (when (and (featurep 'ede-php-autoload) (ede-current-project))
     (semantic-add-system-include (ede-php-autoload-project-root)))

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
