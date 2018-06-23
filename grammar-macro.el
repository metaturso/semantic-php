(require 'semantic/wisent/grammar)

(defun grammar-macro-NAMESPACE (name body)
  "Create a new namespace tag with NAME with BODY.
All tags in BODY gets their :namespace attribute set to NAME."
  `(wisent-raw-tag
    (semantic-tag-new-type
     ,name
     "namespace"
     (let ((result ,body))
       (dolist (tag result result)
         ;; TODO Check if 'include and 'using shouldn't be added the :namespace attribute.
         (unless (memq (semantic-tag-class tag) '(include using namespace alias))
           ;; NOTE I'm not using the tags' `parent' attribute to avoid having to
           ;; mess about with method definitions. Plus, I set this attribute to
           ;; the namespace type tag ecb-sync-tags breaks, probably because of
           ;; circular references in the data.
           (semantic-tag-put-attribute tag :namespace ,name))
         ) result)
     nil)))

(provide 'grammar-macro)
