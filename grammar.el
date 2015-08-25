;;; grammar.el --- Generated parser support file

;; Copyright (C) 2015 Andrea Turso

;; Author: Andrea Turso <andreaturso@proxima.local>
;; Created: 2015-08-24 23:45:13+0100
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
   '(("use" . T_USE)
     ("null" . T_NULL)
     ("true" . T_TRUE)
     ("false" . T_FALSE)
     ("new" . T_NEW)
     ("namespace" . T_NAMESPACE)
     ("class" . T_CLASS)
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
     ("var" . T_VAR))
   'nil)
  "Table of language keywords.")

(defconst grammar--token-table
  (semantic-lex-make-type-table
   '(("ns-separator"
      (T_NS_SEPARATOR))
     ("punctuation"
      (T_SCOPE_RES . "::")
      (T_COMMA . ",")
      (T_EQUAL . "=")
      (T_SEMICOLON . ";"))
     ("code"
      (T_CLOSE_TAG . "?>")
      (T_OPEN_TAG . "<?php"))
     ("close-paren"
      (RBRACK . "]")
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACK . "[")
      (LBRACE . "{")
      (LPAREN . "("))
     ("block"
      (BRACK_BLOCK . "(LBRACK RBRACK)")
      (BRACE_BLOCK . "(LBRACE RBRACE)")
      (PAREN_BLOCK . "(LPAREN RPAREN)"))
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
     ("ns-separator" syntax "\\\\")
     ("ns-separator" matchdatatype regexp)
     ("ns-separator" :declared t)
     ("punctuation" syntax "\\(\\s.\\|\\s$\\|\\s'\\|[$]\\|[\\]\\)+")
     ("punctuation" matchdatatype string)
     ("punctuation" :declared t)
     ("code" :declared t)
     ("block" :declared t)
     ("mb" syntax "[[:nonascii:]]+")
     ("mb" :declared t)
     ("variable" syntax "\\([$][a-zA-Z_]+[a-zA-Z0-9_]*\\)")
     ("variable" :declared t)
     ("encapsed-and-whitespace" :declared t)
     ("quoted-string" syntax "\\s\"")
     ("quoted-string" matchdatatype sexp)
     ("quoted-string" :declared t)
     ("string" syntax "\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)")
     ("string" :declared t)
     ("number" :declared t)))
  "Table of lexical tokens.")

(defconst grammar--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((T_NUMBER T_STRING T_CONSTANT_ENCAPSED_STRING T_ENCAPSED_AND_WHITESPACE T_VARIABLE mbstring PAREN_BLOCK BRACE_BLOCK BRACK_BLOCK LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK T_OPEN_TAG T_CLOSE_TAG T_SEMICOLON T_EQUAL T_COMMA T_SCOPE_RES T_NS_SEPARATOR T_USE T_NULL T_TRUE T_FALSE T_NEW T_NAMESPACE T_CLASS T_ABSTRACT T_FINAL T_EXTENDS T_IMPLEMENTS T_PUBLIC T_PRIVATE T_PROTECTED T_STATIC T_FUNCTION T_ARRAY T_VAR)
       nil
       (line
        ((T_SEMICOLON))
        ((use_declaration))
        ((namespace_declaration))
        ((class_declaration))
        ((function_declaration)))
       (use_declaration
        ((T_USE qualified_name T_SEMICOLON)
         (wisent-raw-tag
          (semantic-tag-new-include $2 nil))))
       (namespace_declaration
        ((T_NAMESPACE qualified_name T_SEMICOLON)
         (wisent-raw-tag
          (semantic-tag-new-package $2 nil))))
       (class_declaration
        ((class_opt T_CLASS qualified_name extends_opt implements_opt class_body)
         (wisent-raw-tag
          (semantic-tag-new-type $3 $2 $6
                                 (if
                                     (or $4 $5)
                                     (cons $4 $5))
                                 :typemodifiers
                                 (list $1)))))
       (class_opt
        (nil)
        ((T_ABSTRACT))
        ((T_FINAL)))
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
                                     :typemodifiers
                                     (list $1)))))
       (attribute_opt
        (nil
         (identity "public"))
        ((T_VAR)
         (identity "public"))
        ((access_modifier)))
       (attribute_initialiser
        (nil
         (cons "mixed" "*uninitialised*"))
        ((T_EQUAL T_NULL)
         (cons "null" "null"))
        ((T_EQUAL boolean)
         (cons "boolean" $2))
        ((T_EQUAL BRACK_BLOCK)
         (cons "array" $2))
        ((T_EQUAL class_instantiation)
         (cons
          (semantic-tag-name $2)
          (semantic-tag-name $2)))
        ((T_EQUAL T_CONSTANT_ENCAPSED_STRING)
         (cons "string" $2))
        ((T_EQUAL T_NUMBER)
         (cons "number" $2)))
       (class_instantiation
        ((T_NEW qualified_name)
         (wisent-raw-tag
          (semantic-tag-new-include $2 nil))))
       (method_declaration
        ((method_opt function_declarator function_body)
         (wisent-raw-tag
          (semantic-tag-new-function
           (car $2)
           ""
           (cdr $2)
           :typemodifiers
           (list $1)))))
       (function_declaration
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
        (nil
         (identity "public"))
        ((access_modifier)))
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
                                     (or $1
                                         (car $3))
                                     (cdr $3)))))
       (formal_parameter_initialiser
        (nil
         (cons "mixed" "*uninitialised*"))
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
        ((qualified_name))
        ((T_ARRAY)))
       (block
           ((BRACE_BLOCK)))
       (boolean
        ((T_TRUE))
        ((T_FALSE)))
       (type_constant
        ((qualified_name T_SCOPE_RES T_STRING)
         (concat $1 $2 $3)))
       (dims
        ((dims BRACK_BLOCK)
         (concat $1 "[]"))
        ((BRACK_BLOCK)
         (identity "[]")))
       (dims_opt
        (nil
         (identity ""))
        ((dims)))
       (qualified_name
        ((T_NS_SEPARATOR partial_qualified_name)
         (identity $2))
        ((partial_qualified_name)))
       (partial_qualified_name
        ((partial_qualified_name T_NS_SEPARATOR T_STRING)
         (concat $1 $2 $3))
        ((T_STRING)))
       (qualified_name_list
        ((qualified_name_list T_COMMA qualified_name)
         (cons $3 $1))
        ((qualified_name)
         (list $1))))
     '(line qualified_name qualified_name_list class_declaration class_body class_opt extends_opt implements_opt class_member_declaration method_declaration method_opt formal_parameter formal_parameters formal_parameter_list formal_parameter_initialiser attribute_declaration attribute_opt attribute_initialiser type_hint type_constant dims dims_opt block boolean use_declaration namespace_declaration class_instantiation function_declaration function_body function_declarator)))
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
  "\\([$][a-zA-Z_]+[a-zA-Z0-9_]*\\)"
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
  '((T_SCOPE_RES . "::")
    (T_COMMA . ",")
    (T_EQUAL . "=")
    (T_SEMICOLON . ";"))
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

(define-lex-regex-type-analyzer grammar--<ns-separator>-regexp-analyzer
  "regexp analyzer for <ns-separator> tokens."
  "\\\\"
  nil
  'T_NS_SEPARATOR)

(define-lex-sexp-type-analyzer grammar--<string>-sexp-analyzer
  "sexp analyzer for <string> tokens."
  "\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
  'T_STRING)

(define-lex-keyword-type-analyzer grammar--<keyword>-keyword-analyzer
  "keyword analyzer for <keyword> tokens."
  "\\(\\sw\\|\\s_\\)+")


;;; Epilogue
;;
(define-lex-regex-analyzer grammar-lex-open-tag
  "Detects and converts a php opening tag to a T_OPEN_TAG token"
  "<[?]\\(php\\)?\\([[:space:]]+\\|$\\)"
  ;; Zing to the end of this brace block.
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

(define-lex grammar-lexer
  "Lexical analyzer that handles PHP buffers.
It ignores whitespaces, newlines and comments."
  semantic-lex-ignore-whitespace
  semantic-lex-ignore-newline
  semantic-lex-ignore-comments

  grammar-lex-open-tag
  grammar-lex-close-tag

  grammar--<ns-separator>-regexp-analyzer
  grammar--<variable>-regexp-analyzer
  grammar--<punctuation>-string-analyzer
  grammar--<keyword>-keyword-analyzer
  grammar--<block>-block-analyzer
  grammar--<number>-regexp-analyzer
  grammar--<string>-sexp-analyzer
  grammar--<quoted-string>-sexp-analyzer
  grammar--<mb>-regexp-analyzer)

(provide 'grammar)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; grammar.el ends here
