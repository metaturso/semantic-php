(defvar semantic-php-project-root
  (expand-file-name
   (concat
    (file-name-directory
     (or load-file-name
         (buffer-file-name))) "/../")))

(defvar semantic-php-test-directory
  (concat semantic-php-project-root "tests/"))

(defvar semantic-php-fixture-directory
  (concat semantic-php-test-directory "fixtures/"))

(unless (file-exists-p
         (concat semantic-php-project-root "php-mode/"))
  (message "Could not load php-mode, attempting to download submodule.")
  (shell-command "git submodule update"))

(add-to-list 'load-path semantic-php-project-root)
(add-to-list 'load-path semantic-php-test-directory)
(load (concat semantic-php-project-root "php-mode/php-mode.el"))

(require 'semantic)
(require 'semantic/grammar)

(defun semantic-php-recompile-grammar nil
  "Recompiles the grammar before running the tests."
  (with-temp-buffer
    (message "Recompiling semantic-php grammar...")
    (find-file (concat semantic-php-project-root "grammar.wy"))
    (semantic-mode 1)
    (semantic-grammar-create-package)))

(semantic-php-recompile-grammar)

(require 'php-mode)
(require 'grammar-setup)
(require 'test-parser)
(require 'test-analyser)

(ert-run-tests-batch t)
