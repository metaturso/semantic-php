(defmacro with-php-file (fixture-file &rest body)
  "Creates a new php-mode buffer, inserts FILE and evaluates BODY there.
FILE is a filename or a path relative to the directory specified
by `semantic-php-fixture-directory'.
BODY has access to the following variables:
- buffer-tags: the buffer's tag list.
"
  `(with-temp-buffer
     (insert-file-contents (concat semantic-php-fixture-directory ,fixture-file))
     (set-visited-file-name (concat semantic-php-fixture-directory ,fixture-file) t nil)
     (goto-char 0)
     (php-mode)
     (semantic-clear-toplevel-cache)
     (let ((buffer-tags (semantic-fetch-tags)))
       ,@body)))

(defmacro with-php-buffer (code &rest body)
  "Creates a php-mode buffer, inserts CODE after the <?php
opening tag, and evaluates BODY there."
  `(with-temp-buffer
     (insert (concat "<?php " ,code))
     (goto-char 0)
     (php-mode)
     (semantic-clear-toplevel-cache)
     (let ((buffer-tags (semantic-fetch-tags)))
       ,@body)))

(defun imported-type-names (tag)
  "Extracts the name and type of symbol imported with a php use
declaration. The return list is in the form: (local symbol name
or alias, fully qualified symbol name, imported symbol type).

If TAG is not a using tag nil is returned."
  (when (semantic-tag-of-class-p tag 'using)
    (list (semantic-php-name-nonnamespace (semantic-tag-name (semantic-tag-type tag)))
          (semantic-tag-name tag)
          (semantic-tag-type (semantic-tag-type tag)))))

(provide 'support-functions)
