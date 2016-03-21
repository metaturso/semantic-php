(require 'semantic/tag)
(require 'semantic/tag-ls)
(require 'support-functions)

(ert-deftest semantic-php-test-scoped-types nil
  ""
  (with-php-buffer
   (concat
    "use ClassName;"
    "use Ns\\ClassName as AnotherName;"
    "class LocalClass {}"
    "function localFunction() {}")
   (let ((scopetypes (semantic-ctxt-scoped-types)))
     (should (semantic-tag-similar-p
              (semantic-tag "ClassName" 'type :kind 'alias :type "class")
              (nth 0 scopetypes)
              :prototype :members))

     (should (semantic-tag-similar-p
              (semantic-tag "AnotherName" 'type :kind 'alias :type "class")
              (nth 1 scopetypes)
              :prototype :members))

     (should (semantic-tag-similar-p
              (semantic-tag "LocalClass" 'type :type "class")
              (nth 2 scopetypes)))

     (should (semantic-tag-similar-p
              (semantic-tag "localFunction" 'function :type "mixed")
              (nth 3 scopetypes)))
     )
  ))

(ert-deftest semantic-php-test-class-methods-from-parent nil
  "Test that we can access public and protected methods from the
parent class."
  (with-php-buffer
   (concat
    "class ParentClass { public function stuff() {} }"
    "class ChildClass extends ParentClass { private $figureThisOut; }"
    ;; ^^^ NOTE FIXME TODO NEXT: why are classes with empty body not
    ;; showing up in (semantic-find-tags-by-class 'type (current-buffer))?
    "function test() {"
    "  $child = new ChildClass();"
    "  $child->"
    "}")
   (search-forward "->")
   ;; (let* ((scopetypes (semantic-analyze-scoped-types (point)))
   ;;        (parents (semantic-analyze-scope-nested-tags (point) scopetypes)))
   ;;   (pp scopetypes))
   (semantic-complete-analyze-inline)
   ;; (message "%s" (semantic-analyze-scope-lineage-tags-php-mode (list (nth 1 buffer-tags)) nil))
   )
  )

(provide 'test-analyser)
