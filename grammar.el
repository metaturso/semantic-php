;;; grammar.el --- Generated parser support file

;; Copyright (C) 2016 Andrea Turso

;; Author: Andrea Turso <trashofmasters@gmail.com>
;; Created: 2016-03-20 02:50:56+0000
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file grammar.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;

;;; Declarations
;;
(defconst grammar--keyword-table
  (semantic-lex-make-keyword-table
   '(("as" . T_AS)
     ("use" . T_USE)
     ("const" . T_CONST)
     ("null" . T_NULL)
     ("true" . T_TRUE)
     ("false" . T_FALSE)
     ("new" . T_NEW)
     ("namespace" . T_NAMESPACE)
     ("class" . T_CLASS)
     ("interface" . T_INTERFACE)
     ("abstract" . T_ABSTRACT)
     ("final" . T_FINAL)
     ("extends" . T_EXTENDS)
     ("implements" . T_IMPLEMENTS)
     ("public" . T_PUBLIC)
     ("private" . T_PRIVATE)
     ("protected" . T_PROTECTED)
     ("static" . T_STATIC)
     ("function" . T_FUNCTION)
     ("array" . T_ARRAY)
     ("var" . T_VAR)
     ("array" . T_TYPE_ARRAY)
     ("int" . T_TYPE_INT)
     ("bool" . T_TYPE_BOOL)
     ("float" . T_TYPE_FLOAT)
     ("string" . T_TYPE_STRING)
     ("callable" . T_TYPE_CALLABLE)
     ("self" . T_TYPE_SELF)
     ("parent" . T_TYPE_PARENT))
   'nil)
  "Table of language keywords.")

(defconst grammar--token-table
  (semantic-lex-make-type-table
   '(("punctuation"
      (T_NS_SEPARATOR . "\\")
      (T_SCOPE_RES . "::")
      (T_COMMA . ",")
      (T_EQUAL . "=")
      (T_SEMICOLON . ";")
      (T_COLON . ":"))
     ("scope"
      (S_NS_SCOPE))
     ("code"
      (T_CLOSE_TAG . "[?]\\>")
      (T_OPEN_TAG . "\\<[?]\\(php\\)?"))
     ("block"
      (BRACK_BLOCK . "(LBRACK RBRACK)")
      (BRACE_BLOCK . "(LBRACE RBRACE)")
      (PAREN_BLOCK . "(LPAREN RPAREN)"))
     ("close-paren"
      (RBRACK . "]")
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACK . "[")
      (LBRACE . "{")
      (LPAREN . "("))
     ("mb"
      (mbstring))
     ("variable"
      (T_VARIABLE))
     ("encapsed-and-whitespace"
      (T_ENCAPSED_AND_WHITESPACE))
     ("quoted-string"
      (T_CONSTANT_ENCAPSED_STRING))
     ("string"
      (T_STRING))
     ("number"
      (T_NUMBER)))
   '(("keyword" :declared t)
     ("punctuation" syntax "\\(\\s.\\|\\s$\\|\\s'\\|[$]\\|[\\]\\)+")
     ("punctuation" matchdatatype string)
     ("punctuation" :declared t)
     ("code" :declared t)
     ("block" :declared t)
     ("mb" syntax "[[:nonascii:]]+")
     ("mb" :declared t)
     ("variable" syntax "[$]\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)")
     ("variable" :declared t)
     ("encapsed-and-whitespace" :declared t)
     ("quoted-string" syntax "\\s\"")
     ("quoted-string" matchdatatype sexp)
     ("quoted-string" :declared t)
     ("string" syntax "\\([A-z_]+[A-z0-9_]*\\)")
     ("string" :declared t)
     ("number" :declared t)))
  "Table of lexical tokens.")

(defconst grammar--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((T_NUMBER T_STRING T_CONSTANT_ENCAPSED_STRING T_ENCAPSED_AND_WHITESPACE T_VARIABLE mbstring LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK T_OPEN_TAG T_CLOSE_TAG S_NS_SCOPE T_COLON T_SEMICOLON T_EQUAL T_COMMA T_SCOPE_RES T_NS_SEPARATOR T_AS T_USE T_CONST T_NULL T_TRUE T_FALSE T_NEW T_NAMESPACE T_CLASS T_INTERFACE T_ABSTRACT T_FINAL T_EXTENDS T_IMPLEMENTS T_PUBLIC T_PRIVATE T_PROTECTED T_STATIC T_FUNCTION T_ARRAY T_VAR T_TYPE_ARRAY T_TYPE_INT T_TYPE_BOOL T_TYPE_FLOAT T_TYPE_STRING T_TYPE_CALLABLE T_TYPE_SELF T_TYPE_PARENT)
       nil
       (grammar
        ((namespace_declaration))
        ((use_declaration))
        ((class_declaration))
        ((function_declaration))
        ((interface_declaration)))
       (local_variables
        ((T_VARIABLE local_variable_initialiser)
         (wisent-raw-tag
          (semantic-tag-new-variable $1
                                     (wisent-raw-tag
                                      (semantic-tag
                                       (car $2)
                                       'metatype))
                                     (cdr $2)))))
       (local_variable_initialiser
        ((T_EQUAL T_NULL)
         (cons "null" "null"))
        ((T_EQUAL boolean)
         (cons "boolean" $2))
        ((T_EQUAL BRACK_BLOCK)
         (cons "array" $2))
        ((T_EQUAL class_instantiation)
         (cons $2 $2))
        ((T_EQUAL T_CONSTANT_ENCAPSED_STRING)
         (cons "string" $2))
        ((T_EQUAL T_NUMBER)
         (cons "number" $2)))
       (namespace_declaration
        ((T_NAMESPACE qualified_name namespace_body)
         (let
             ((namespace
               (wisent-raw-tag
                (semantic-tag-new-type $2 "namespace" nil nil)))
              members)
           (dolist
               (tag $3 members)
             (unless
                 (memq
                  (semantic-tag-class tag)
                  '(include using namespace alias))
               (semantic-tag-put-attribute tag :namespace $2))
             (setq members
                   (cons tag members)))
           (semantic-tag-put-attribute namespace :members
                                       (nreverse members))
           namespace)))
       (namespace_body
        ((S_NS_SCOPE)
         (if $region1
             (semantic-parse-region
              (car $region1)
              (cdr $region1)
              'namespace_subparts nil)
           (error "Invalid form (EXPANDFULL %s %s)" $region1 'namespace_subparts)))
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'namespace_subparts 1)))
       (namespace_subparts
        (nil nil)
        ((T_SEMICOLON)
         nil)
        ((use_declaration))
        ((const_declaration))
        ((class_declaration))
        ((function_declaration))
        ((interface_declaration)))
       (use_declaration
        ((T_USE use_type qualified_name T_AS T_STRING T_SEMICOLON)
         (wisent-raw-tag
          (semantic-tag $5 'using :type
                        (wisent-raw-tag
                         (semantic-tag-new-type $3 $2 nil nil :kind 'alias :prototype t)))))
        ((T_USE use_type qualified_name T_SEMICOLON)
         (wisent-raw-tag
          (semantic-tag
           (semantic-php-name-nonnamespace $3)
           'using :type
           (wisent-raw-tag
            (semantic-tag-new-type $3 $2 nil nil :kind 'alias :prototype t))))))
       (const_declaration
        ((T_CONST T_STRING const_initialiser)
         (wisent-raw-tag
          (semantic-tag-new-variable $2
                                     (car $3)
                                     (cdr $3)
                                     :constant-flag t))))
       (const_initialiser
        ((T_EQUAL T_NULL)
         (cons "null" "null"))
        ((T_EQUAL boolean)
         (cons "boolean" $2))
        ((T_EQUAL T_CONSTANT_ENCAPSED_STRING)
         (cons "string"
               (substring $2 1 -1)))
        ((T_EQUAL T_NUMBER)
         (cons "number" $2)))
       (use_type
        ((T_CONST)
         (identity "variable"))
        ((T_FUNCTION)
         (identity "function"))
        (nil
         (identity "class")))
       (class_declaration
        ((class_opt T_CLASS T_STRING extends_opt implements_opt class_body)
         (wisent-raw-tag
          (semantic-tag-new-type $3 "class" $6
                                 (if
                                     (or $4 $5)
                                     (cons $4 $5))
                                 :typemodifiers $1))))
       (class_opt
        (nil)
        ((T_ABSTRACT)
         (list "abstract"))
        ((T_FINAL)
         (list "final")))
       (extends_opt
        (nil)
        ((T_EXTENDS qualified_name)
         (identity $2)))
       (implements_opt
        (nil)
        ((T_IMPLEMENTS qualified_name_list)
         (nreverse $2)))
       (class_body
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'class_member_declaration 1)))
       (class_member_declaration
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((block)
         nil)
        ((method_declaration))
        ((attribute_declaration)))
       (attribute_declaration
        ((attribute_opt T_VARIABLE attribute_initialiser)
         (wisent-raw-tag
          (semantic-tag-new-variable $2
                                     (car $3)
                                     (cdr $3)
                                     :typemodifiers $1))))
       (attribute_opt
        (nil
         (list "public"))
        ((T_VAR)
         (list "public"))
        ((static_or_access_modifiers)))
       (attribute_initialiser
        (nil
         (cons "mixed" "*empty*"))
        ((T_EQUAL T_NULL)
         (cons "null" "null"))
        ((T_EQUAL boolean)
         (cons "boolean" $2))
        ((T_EQUAL BRACK_BLOCK)
         (cons "array" $2))
        ((T_EQUAL T_CONSTANT_ENCAPSED_STRING)
         (cons "string" $2))
        ((T_EQUAL T_NUMBER)
         (cons "number" $2)))
       (class_instantiation
        ((T_NEW qualified_name)
         (identity $2)))
       (method_declaration
        ((method_opt function_declarator T_COLON method_return_type_hint function_body)
         (wisent-raw-tag
          (semantic-tag-new-function
           (car $2)
           (wisent-raw-tag
            (semantic-tag $4 'metatype))
           (cdr $2)
           :typemodifiers $1)))
        ((method_opt function_declarator function_body)
         (wisent-raw-tag
          (semantic-tag-new-function
           (car $2)
           "mixed"
           (cdr $2)
           :typemodifiers $1))))
       (function_declaration
        ((function_declarator T_COLON function_return_type_hint function_body)
         (wisent-raw-tag
          (semantic-tag-new-function
           (car $1)
           (wisent-raw-tag
            (semantic-tag $3 'metatype))
           (cdr $1))))
        ((function_declarator function_body)
         (wisent-raw-tag
          (semantic-tag-new-function
           (car $1)
           "mixed"
           (cdr $1)))))
       (function_declarator
        ((T_FUNCTION T_STRING formal_parameter_list)
         (cons $2 $3)))
       (method_opt
        ((static_or_access_modifiers)))
       (function_body
        ((T_SEMICOLON))
        ((block)))
       (formal_parameter_list
        ((PAREN_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'formal_parameters 1)))
       (formal_parameters
        ((LPAREN)
         nil)
        ((RPAREN)
         nil)
        ((formal_parameter T_COMMA))
        ((formal_parameter RPAREN)))
       (formal_parameter
        ((type_hint T_VARIABLE formal_parameter_initialiser)
         (wisent-raw-tag
          (semantic-tag-new-variable $2
                                     (wisent-raw-tag
                                      (semantic-tag
                                       (or $1
                                           (car $3))
                                       'metatype))
                                     (cdr $3)))))
       (formal_parameter_initialiser
        (nil
         (cons "mixed" "*empty*"))
        ((T_EQUAL T_NULL)
         (cons "null" "null"))
        ((T_EQUAL boolean)
         (cons "boolean" $2))
        ((T_EQUAL BRACK_BLOCK)
         (cons "array" $2))
        ((T_EQUAL T_CONSTANT_ENCAPSED_STRING)
         (cons "string" $2))
        ((T_EQUAL T_NUMBER)
         (cons "number" $2)))
       (access_modifier
        ((T_PUBLIC))
        ((T_PROTECTED))
        ((T_PRIVATE)))
       (type_hint
        (nil)
        ((required_type_hint)))
       (function_return_type_hint
        ((required_type_hint)))
       (method_return_type_hint
        ((required_type_hint))
        ((T_TYPE_SELF))
        ((T_TYPE_PARENT)))
       (required_type_hint
        ((T_TYPE_ARRAY))
        ((T_TYPE_INT))
        ((T_TYPE_BOOL))
        ((T_TYPE_FLOAT))
        ((T_TYPE_STRING))
        ((T_TYPE_CALLABLE))
        ((qualified_name)))
       (block
           ((BRACE_BLOCK)))
       (boolean
        ((T_TRUE))
        ((T_FALSE)))
       (qualified_name
        ((T_NS_SEPARATOR partial_qualified_name)
         (concat "\\" $2))
        ((partial_qualified_name)
         (identity $1)))
       (partial_qualified_name
        ((partial_qualified_name T_NS_SEPARATOR T_STRING)
         (concat $1 "\\" $3))
        ((T_STRING)
         (identity $1)))
       (qualified_name_list
        ((qualified_name_list T_COMMA qualified_name)
         (cons $3 $1))
        ((qualified_name)
         (list $1)))
       (static_or_access_modifiers
        ((T_STATIC access_modifier)
         (list $1 $2))
        ((access_modifier T_STATIC)
         (list $2 $1))
        ((access_modifier)
         (list $1)))
       (interface_declaration
        ((T_INTERFACE T_STRING extends_opt implements_opt interface_body)
         (wisent-raw-tag
          (semantic-tag-new-type $2 "interface" $5
                                 (if
                                     (or $3 $4)
                                     (cons $3 $4))))))
       (interface_body
        ((BRACE_BLOCK)
         (semantic-parse-region
          (car $region1)
          (cdr $region1)
          'interface_member_declaration 1)))
       (interface_member_declaration
        ((LBRACE)
         nil)
        ((RBRACE)
         nil)
        ((block)
         nil)
        ((interface_method_declaration)))
       (interface_method_declaration
        (nil)
        ((method_opt function_declarator T_COLON function_return_type_hint T_SEMICOLON)
         (wisent-raw-tag
          (semantic-tag-new-function
           (car $2)
           (wisent-raw-tag
            (semantic-tag $4 'metatype))
           (cdr $2)
           :typemodifiers $1)))
        ((method_opt function_declarator T_SEMICOLON)
         (wisent-raw-tag
          (semantic-tag-new-function
           (car $2)
           "mixed"
           (cdr $2)
           :typemodifiers $1)))))
     '(grammar formal_parameters namespace_subparts class_member_declaration interface_member_declaration local_variables)))
  "Parser table.")

(defun grammar--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic--parse-table grammar--parse-table
        semantic-debug-parser-source #("grammar.wy" 0 10 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu"))
        semantic-flex-keywords-obarray grammar--keyword-table
        semantic-lex-types-obarray grammar--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(define-lex-sexp-type-analyzer grammar--<quoted-string>-sexp-analyzer
  "sexp analyzer for <quoted-string> tokens."
  "\\s\""
  'T_CONSTANT_ENCAPSED_STRING)

(define-lex-regex-type-analyzer grammar--<variable>-regexp-analyzer
  "regexp analyzer for <variable> tokens."
  "[$]\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
  nil
  'T_VARIABLE)

(define-lex-block-type-analyzer grammar--<block>-block-analyzer
  "block analyzer for <block> tokens."
  "\\s(\\|\\s)"
  '((("(" LPAREN PAREN_BLOCK)
     ("{" LBRACE BRACE_BLOCK)
     ("[" LBRACK BRACK_BLOCK))
    (")" RPAREN)
    ("}" RBRACE)
    ("]" RBRACK))
  )

(define-lex-string-type-analyzer grammar--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\|[$]\\|[\\]\\)+"
  '((T_NS_SEPARATOR . "\\")
    (T_SCOPE_RES . "::")
    (T_COMMA . ",")
    (T_EQUAL . "=")
    (T_SEMICOLON . ";")
    (T_COLON . ":"))
  'punctuation)

(define-lex-regex-type-analyzer grammar--<mb>-regexp-analyzer
  "regexp analyzer for <mb> tokens."
  "[[:nonascii:]]+"
  nil
  'mbstring)

(define-lex-regex-type-analyzer grammar--<number>-regexp-analyzer
  "regexp analyzer for <number> tokens."
  semantic-lex-number-expression
  nil
  'T_NUMBER)

(define-lex-sexp-type-analyzer grammar--<string>-sexp-analyzer
  "sexp analyzer for <string> tokens."
  "\\([A-z_]+[A-z0-9_]*\\)"
  'T_STRING)

(define-lex-keyword-type-analyzer grammar--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")


;;; Epilogue
;;
(define-lex-regex-analyzer grammar-lex-open-tag
  "Detects and converts a php opening tag to a T_OPEN_TAG token"
  "<[?]\\(php\\)?\\([[:space:]]+\\|$\\)"
  (let ((start (match-beginning 0))
        (end   (match-end 0)))
    (semantic-lex-push-token
     (semantic-lex-token 'T_OPEN_TAG start end))))

(define-lex-regex-analyzer grammar-lex-close-tag
  "Detects and converts a php closing tag to a T_CLOSE_TAG token"
  "[?]>"
  (let ((start (match-beginning 0))
        (end   (match-end 0)))
    (semantic-lex-push-token
     (semantic-lex-token 'T_CLOSE_TAG start end))))

(define-lex-analyzer grammar-lex-ns-block
  "Detects namespace blocks enclosed with non-braced namespace
declarations."
  (looking-at "namespace\\s-+\\(\\(\\\\?\\w+\\)+\\)\\s-*;")

  (semantic-lex-push-token
   (semantic-lex-token
    'T_NAMESPACE
    (match-beginning 0)
    (+ (match-beginning 0) (length "namespace"))))

  (semantic-lex-push-token
   (semantic-lex-token
    'T_STRING
    (match-beginning 1)
    (match-end 1)))

  ;; Move the point forward to avoid infinite parsing loops.
  (search-forward ";")

  (semantic-lex-push-token
   (semantic-lex-token
    'S_NS_SCOPE
    (match-end 0)
    (save-excursion
      (semantic-lex-unterminated-syntax-protection 'S_NS_SCOPE
        (re-search-forward "namespace\\s-+\\(\\\\?\\w+\\)+\\s-*;\\|\\'")
        (setq semantic-lex-end-point (point))
        (match-beginning 0)))))
  nil)

(define-lex grammar-lexer
  "Lexical analyzer that handles PHP buffers.
It ignores whitespaces, newlines and comments."
  grammar-lex-open-tag
  grammar-lex-close-tag
  semantic-lex-ignore-newline
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-comments
  grammar--<variable>-regexp-analyzer
  grammar--<punctuation>-string-analyzer
  grammar-lex-ns-block
  grammar--<keyword>-keyword-analyzer
  grammar--<block>-block-analyzer
  grammar--<number>-regexp-analyzer
  grammar--<string>-sexp-analyzer
  grammar--<quoted-string>-sexp-analyzer
  grammar--<mb>-regexp-analyzer)

(defun semantic-php--resolve-symbol-for-tag (symbol &optional ns-scope)
  "Fully qualifies a symbol name tag based on namespace and import rules."
  (unless (or (null ns-scope) (equal ns-scope "\\"))
    (setq ns-scope (concat "\\" ns-scope "\\")))
  ;; (message "Parser qualifying symbol in NS %s: %s => %s" ns-scope symbol (concat ns-scope symbol))
  (concat ns-scope symbol))

(provide 'grammar)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; grammar.el ends here
