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
  ""
  (with-php-buffer
   "class ClassName {}"
   (should (semantic-tag-similar-p
            (semantic-tag "ClassName" 'type :type "class")
            (car buffer-tags)))
   ))

(ert-deftest semantic-php-test-parser-class-in-namespace nil
  ""
  (with-php-buffer
   "namespace NsName; class ClassName {}"
   (should (semantic-tag-similar-p
            (semantic-tag "NsName" 'type :type "namespace")
            (car buffer-tags)
            :members))
  ))


(provide 'parser)
