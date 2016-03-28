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
  (with-php-file
   "class-inheritance.php"
   (search-forward "->")
   (let* ((completions (mapcar 'semantic-tag-name (semantic-analyze-possible-completions (point))))
          (scopetypes (semantic-analyze-scoped-types (point)))
          (parents (semantic-analyze-scope-nested-tags (point) scopetypes)))

     (should (member "ChildClass" (mapcar 'semantic-tag-name scopetypes)))
     (should (member "ParentClass" (mapcar 'semantic-tag-name scopetypes)))

     (should (member "childPublicAttr" completions))
     (should (member "childPublicMth" completions))

     (should (member "parentPublicAttr" completions))
     (should (member "parentPublicMth" completions))
     )
   )
  )

(ert-deftest semantic-php-scoped-types-same-ns nil
  (with-php-file
   "Ns/ChildClass.php"
   (search-forward "{")
   (semantic-ctxt-scoped-types)
   )
  )

(provide 'test-analyser)
