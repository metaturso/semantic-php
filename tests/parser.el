;; parse some PHP code and verify the tags generated.

;; classes
;; interfaces
;; traits
;; use statement
;; use statement with alias
;; use statement for const
;; use statement for function
;; method
;; method arguments
;; method arguments type hints
;; method return type hint
;; function
;; function arguments
;; function arguments type hints
;; function return type hint
;; namespace declaration
;; braced namespace declaration
;; namespace constant
;; class constant

(require 'semantic/tag)
(require 'semantic/tag-ls)

(ert-deftest semantic-php-test-parser-class nil
  "Tests the production of tags for class declarations inside a
buffer without any namespace."
  (with-php-buffer
   "class ClassName {}"

   (should (semantic-tag-similar-p
            (semantic-tag "ClassName" 'type :type "class")
            (car buffer-tags)))
   )

  (with-php-buffer
   (concat
    "class FirstClass {} "
    "class SecondClass {} ")

   (should (semantic-tag-similar-p
            (semantic-tag "FirstClass" 'type :type "class")
            (car buffer-tags)))

   (should (semantic-tag-similar-p
            (semantic-tag "SecondClass" 'type :type "class")
            (cadr buffer-tags)))
   )
  )

(ert-deftest semantic-php-test-parser-class-solo-namespace nil
  "Tests the production of tags for class declarations inside a
buffer with a single namespace."
  (with-php-buffer
   "namespace NsName; class ClassName {}"

   ;; the top-level tag is the namespace.
   (should (semantic-tag-similar-p
            (semantic-tag "NsName" 'type :type "namespace")
            (car buffer-tags)
            :members))

   ;; the namespace contains the class.
   (should (semantic-tag-similar-p
            (semantic-tag "ClassName" 'type :type "class" :namespace "NsName")
            (car (semantic-tag-type-members
                  (car buffer-tags)))))
   )
  )

(ert-deftest semantic-php-test-parser-namespace-constant nil
  "Tests the production of tags for namespace constants"
  (with-php-buffer
   (concat
    ;; FIXME? A missing semicolon after const_declaration causes
    ;; a endless parsing loop.
    "namespace NsName;"
    "const CONST_NUM_I  = 1;"
    "const CONST_NUM_D  = 3.14;"
    "const CONST_STR_S  = 'hello';"
    "const CONST_STR_D  = \"hello\";"
    "const CONST_BOOL_T = true;"
    "const CONST_BOOL_F = false;"
    "const CONST_NULL   = null;")

   (let ((consts (semantic-tag-type-members (car buffer-tags))))

     (should (semantic-tag-similar-p
              (semantic-tag "CONST_NUM_I" 'variable
                            :constant-flag t
                            :namespace "NsName"
                            :type "number"
                            :default-value "1")
              (nth 0 consts)))

     (should (semantic-tag-similar-p
              (semantic-tag "CONST_NUM_D" 'variable
                            :constant-flag t
                            :namespace "NsName"
                            :type "number"
                            :default-value "3.14")
              (nth 1 consts)))


     (should (semantic-tag-similar-p
              (semantic-tag "CONST_STR_S" 'variable
                            :constant-flag t
                            :namespace "NsName"
                            :type "string"
                            :default-value "hello")
              (nth 2 consts)))

     (should (semantic-tag-similar-p
              (semantic-tag "CONST_STR_S" 'variable
                            :constant-flag t
                            :namespace "NsName"
                            :type "string"
                            :default-value "hello")
              (nth 2 consts)))

     (should (semantic-tag-similar-p
              (semantic-tag "CONST_STR_D" 'variable
                            :constant-flag t
                            :namespace "NsName"
                            :type "string"
                            :default-value "hello")
              (nth 3 consts)))


     (should (semantic-tag-similar-p
              (semantic-tag "CONST_BOOL_T" 'variable
                            :constant-flag t
                            :namespace "NsName"
                            :type "boolean"
                            :default-value "true")
              (nth 4 consts)))

     (should (semantic-tag-similar-p
              (semantic-tag "CONST_BOOL_F" 'variable
                            :constant-flag t
                            :namespace "NsName"
                            :type "boolean"
                            :default-value "false")
              (nth 5 consts)))

     (should (semantic-tag-similar-p
              (semantic-tag "CONST_NULL" 'variable
                            :constant-flag t
                            :namespace "NsName"
                            :type "null"
                            :default-value "null")
              (nth 6 consts)))
     )
   )
  )
;; (ert-deftest semantic-php-test-parser-class-multi-namespace nil

(provide 'parser)
