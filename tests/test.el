(defmacro with-php-file (fixture-file &rest body)
  "Creates a new php-mode buffer, inserts FILE and evaluates BODY there.
FILE is a filename or a path relative to the directory specified
by `semantic-php-fixture-directory'.
BODY has access to the following variables:
- buffer-tags: the buffer's tag list.
"
  `(with-temp-buffer
     (insert-file-contents (concat semantic-php-fixture-directory ,fixture-file))
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

(provide 'test)
