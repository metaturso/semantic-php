;; parse some PHP code and verify the tags generated.

;; - [x] classes
;; - [ ] interfaces
;; - [ ] traits
;; - [ ] use statement
;; - [ ] use statement with alias
;; - [ ] use statement for const
;; - [ ] use statement for function
;; - [ ] method
;; - [ ] method arguments
;; - [ ] method arguments type hints
;; - [ ] method return type hint
;; - [ ] class use trait
;; - [ ] class use trait with alias
;; - [ ] closure
;; - [ ] closure arguments
;; - [ ] closure use variables
;; - [ ] function
;; - [ ] function arguments
;; - [ ] function arguments type hints
;; - [ ] function return type hint
;; - [x] namespace declaration
;; - [ ] braced namespace declaration
;; - [x] namespace constant
;; - [x] class constants

(require 'cl)
(require 'semantic/tag)
(require 'semantic/tag-ls)
(require 'support-functions)

;; Classes
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

(ert-deftest semantic-php-test-parser-class-constants nil
  "Tests the production of tags for class declarations inside a
buffer without any namespace."
  (with-php-buffer
   "class ClassName { const CONST_NAME = 1; }"

   (should (semantic-tag-similar-p
            (semantic-tag "ClassName" 'type :type "class")
            (car buffer-tags)
            :members))

   (should (semantic-tag-similar-p
            (semantic-tag "CONST_NAME" 'variable
                          :constant-flag t
                          :type "number"
                          :default-value "1")
            (nth 0 (semantic-tag-type-members (car buffer-tags)))
            :members))
   )
  )

(ert-deftest semantic-php-test-parser-class-implements nil
  ""
  (with-php-buffer
   (concat
    "class ChildClass extends ParentClass {}"
    "class ExceptionName extends \\Exception {}"
    "class ClassName extends Ns\\SubNs\\ClassName {}")
   (should (equal "ParentClass"
                  (car (semantic-tag-type-superclasses (nth 0 buffer-tags)))))

   (should (equal "\\Exception"
                  (car (semantic-tag-type-superclasses (nth 1 buffer-tags)))))

   (should (equal "Ns\\SubNs\\ClassName"
                  (car (semantic-tag-type-superclasses (nth 2 buffer-tags)))))
   )
  )

(ert-deftest semantic-php-test-parser-class-implements nil
  ""
  (with-php-buffer
   (concat
    "class Date implements \\DateTimeImmutable {}"
    "class ClassName implements FirstIface, Ns\\SecondIface {}")
   (let ((date-implements (semantic-tag-get-attribute (nth 0 buffer-tags) :interfaces))
         (class-implements (semantic-tag-get-attribute (nth 1 buffer-tags) :interfaces)))

   (should (equal "\\DateTimeImmutable"
                  (semantic-tag-name (nth 0 date-implements))))
   (should (equal "FirstIface"
                  (semantic-tag-name(nth 0 class-implements))))
   (should (equal "Ns\\SecondIface"
                  (semantic-tag-name(nth 1 class-implements))))
   ))
  )

(ert-deftest semantic-php-test-parser-class-methods-protection nil
  ""
  (with-php-buffer
   (concat
    "class ClassName {"
    "  public function pubFun() {}"
    "  private function priFun() {}"
    "  protected function proFun() {}"
    "}")
   (let ((members (semantic-tag-type-members (first buffer-tags))))
     (should (equal 'public (semantic-tag-protection (nth 0 members))))
     (should (equal 'private (semantic-tag-protection (nth 1 members))))
     (should (equal 'protected (semantic-tag-protection (nth 2 members))))
     )
   )
  )

(ert-deftest semantic-php-test-parser-class-methods-protection-abstract-first nil
  ""
  (with-php-buffer
   (concat
    "class ClassName {"
    "  abstract public function pubFun();"
    "  abstract private function priFun();"
    "  abstract protected function proFun();"
    "  abstract static public function stPubFun();"
    "  abstract static private function stPriFun();"
    "  abstract static protected function stProFun();"
    "}")
   (let ((members (semantic-tag-type-members (first buffer-tags))))
     ;; abstract <access_modifier>
     (should (equal 'public (semantic-tag-protection (nth 0 members))))
     (should (member "abstract" (semantic-tag-modifiers (nth 0 members))))

     (should (equal 'private (semantic-tag-protection (nth 1 members))))
     (should (member "abstract" (semantic-tag-modifiers (nth 1 members))))

     (should (equal 'protected (semantic-tag-protection (nth 2 members))))
     (should (member "abstract" (semantic-tag-modifiers (nth 2 members))))

     ;; abstract static <access_modifier>
     (should (equal 'public (semantic-tag-protection (nth 3 members))))
     (should (member "abstract" (semantic-tag-modifiers (nth 3 members))))
     (should (member "static" (semantic-tag-modifiers (nth 3 members))))

     (should (equal 'private (semantic-tag-protection (nth 4 members))))
     (should (member "static" (semantic-tag-modifiers (nth 4 members))))
     (should (member "abstract" (semantic-tag-modifiers (nth 4 members))))

     (should (equal 'protected (semantic-tag-protection (nth 5 members))))
     (should (member "static" (semantic-tag-modifiers (nth 5 members))))
     (should (member "abstract" (semantic-tag-modifiers (nth 5 members))))
     )
   )
  )

(ert-deftest semantic-php-test-parser-class-methods-protection-static-first nil
  ""
  ;; FIXME: Parsing of <access or stati> abstract ...
  :expected-result :failed
  (with-php-buffer
   (concat
    "class ClassName {"
    "  static public function pubFun();"
    "  static private function priFun();"
    "  static protected function proFun();"
    "  static abstract public function abPubFun();"
    "  static abstract private function abPriFun();"
    "  static abstract protected function abProFun();"
    "}")
   (let ((members (semantic-tag-type-members (first buffer-tags))))
     ;; static <access_modifier>
     (should (equal 'public (semantic-tag-protection (nth 0 members))))
     (should (member "static" (semantic-tag-modifiers (nth 0 members))))

     (should (equal 'private (semantic-tag-protection (nth 1 members))))
     (should (member "static" (semantic-tag-modifiers (nth 1 members))))

     (should (equal 'protected (semantic-tag-protection (nth 2 members))))
     (should (member "static" (semantic-tag-modifiers (nth 2 members))))

     ;; static abstract <access_modifier>
     (should (equal 'public (semantic-tag-protection (nth 3 members))))
     (should (member "static" (semantic-tag-modifiers (nth 3 members))))
     (should (member "abstract" (semantic-tag-modifiers (nth 3 members))))

     (should (equal 'private (semantic-tag-protection (nth 4 members))))
     (should (member "static" (semantic-tag-modifiers (nth 4 members))))
     (should (member "abstract" (semantic-tag-modifiers (nth 4 members))))

     (should (equal 'protected (semantic-tag-protection (nth 5 members))))
     (should (member "static" (semantic-tag-modifiers (nth 5 members))))
     (should (member "abstract" (semantic-tag-modifiers (nth 5 members))))
     )
   )
  )

;; Namespaces
(ert-deftest semantic-php-test-parser-namespace-constants nil
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

(ert-deftest semantic-php-test-parser-class-inside-single-namespace nil
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

(ert-deftest semantic-php-test-parser-classes-inside-many-namespaces nil
  "Tests the production of multiple namespace tags in a buffer
with consecutive namespace declarations."
  (with-php-file
   "multi-ns.php"
   (let (currentns)
     ;; 1) Check the namespace tag.
     ;; 2) Class with the same name across multiple namespace.
     ;; 3) Class with unique name.

     ;; 1 Simple namespace declaration.
     (should (semantic-tag-similar-p
              (semantic-tag "FirstNs" 'type :type "namespace")
              (setq currentns (nth 0 buffer-tags))
              :members))

     ;; 2
     (should (semantic-tag-similar-p
              (semantic-tag "ClassName" 'type :type "class" :namespace "FirstNs")
              (nth 0 (semantic-tag-type-members currentns))
              :members))

     ;; 3
     (should (semantic-tag-similar-p
              (semantic-tag "FirstClass" 'type :type "class" :namespace "FirstNs")
              (nth 1 (semantic-tag-type-members currentns))
              :members))

     ;; 1 Sub-namespace declarations.
     (should (semantic-tag-similar-p
              (semantic-tag "FirstNs\\SubNs" 'type :type "namespace")
              (setq currentns (nth 1 buffer-tags))
              :members))

     ;; 2
     (should (semantic-tag-similar-p
              (semantic-tag "ClassName" 'type :type "class" :namespace "FirstNs\\SubNs")
              (nth 0 (semantic-tag-type-members currentns))
              :members))

     ;; 3
     (should (semantic-tag-similar-p
              (semantic-tag "FirstSubClass" 'type :type "class" :namespace "FirstNs\\SubNs")
              (nth 1 (semantic-tag-type-members currentns))
              :members))

     ;; 1 Check the last namespace in the buffer.
     (should (semantic-tag-similar-p
              (semantic-tag "LastNs" 'type :type "namespace")
              (setq currentns (nth 2 buffer-tags))
              :members))

     ;; 2
     (should (semantic-tag-similar-p
              (semantic-tag "ClassName" 'type :type "class" :namespace "LastNs")
              (nth 0 (semantic-tag-type-members currentns))
              :members))

     ;; 3
     (should (semantic-tag-similar-p
              (semantic-tag "LastClass" 'type :type "class" :namespace "LastNs")
              (nth 1 (semantic-tag-type-members currentns))
              :members))
     )
   )
  )

;; Use statements and tag expansion
(ert-deftest semantic-php-test-tag-expansion-use-class nil
  ""
  (with-php-buffer
   "use DateTime; use Ns\\SubNs\\ClassName;"
   (let ((imports (semantic-find-tags-by-class 'using buffer-tags))
         (includes (semantic-find-tags-by-class 'include buffer-tags)))

     (should (equal
              (list "DateTime" "DateTime" "class")
              (imported-type-names (nth 0 imports))))

     (should (semantic-tag-similar-p
              (semantic-tag "DateTime" 'include)
              (nth 0 includes)))

     (should (equal
              (list "ClassName" "Ns\\SubNs\\ClassName" "class")
              (imported-type-names (nth 1 imports))))

     (should (semantic-tag-similar-p
              (semantic-tag "DateTime" 'include)
              (nth 0 includes)))

     (should (semantic-tag-similar-p
              (semantic-tag "Ns\\SubNs\\ClassName" 'include)
              (nth 1 includes)))
     )
   )
  )

(ert-deftest semantic-php-test-tag-expansion-use-class-with-alias nil
  ""
  (with-php-buffer
   (concat
    "use DateTime as Date;"
    "use Ns\\SubNs\\ClassName as AliasedName;"
    "use Ns\\SubNs\\SomeClass;")
   (let ((imports (semantic-find-tags-by-class 'using buffer-tags))
         (includes (semantic-find-tags-by-class 'include buffer-tags)))

     (should (equal
              (list "Date" "DateTime" "class")
              (imported-type-names (nth 0 imports))))

     (should (semantic-tag-similar-p
              (semantic-tag "DateTime" 'include)
              (nth 0 includes)))

     (should (equal
              (list "AliasedName" "Ns\\SubNs\\ClassName" "class")
              (imported-type-names (nth 1 imports))))

     (should (equal
              (list "SomeClass" "Ns\\SubNs\\SomeClass" "class")
              (imported-type-names (nth 2 imports))))

     (should (semantic-tag-similar-p
              (semantic-tag "DateTime" 'include)
              (nth 0 includes)))

     (should (semantic-tag-similar-p
              (semantic-tag "Ns\\SubNs\\ClassName" 'include)
              (nth 1 includes)))

     (should (semantic-tag-similar-p
              (semantic-tag "Ns\\SubNs\\SomeClass" 'include)
              (nth 2 includes)))
     )
   )
  )

(provide 'test-parser)
