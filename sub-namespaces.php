<?php

namespace Foo;

use External;
use External\Another as Abaca;
use const Math\Pi as Pie;
use function Math\Sin;

function hello()
{
    // (pp (semantic-find-tags-included (current-buffer)))
    // (semantic-find-tags-by-class 'include (current-buffer))
    // (dolist (tag (semantic-flatten-tags-table (current-buffer))) (message "tag: %s, class: %s" (semantic-tag-name tag) (semantic-tag-class tag)))
    // (dolist (T tmp) (setq namereturn (cons (semantic-tag-type T) namereturn)))
}

class ClassName
{
    public function __construct()
    {
        for ($i = 1; $i < 10; $i++) {
            $lambda = function () {

            };
        }

        new \C\AnotherClass;
    }
}

namespace C;

use Foo;

class AnotherClass
{
    public function __construct()
    {
    }

    protected function doSomething()
    {
    }

    private function doSomethingPrivate()
    {
    }
}

// (pp (semantic-fetch-tags))
// (bovinate)

// (dolist (tag (semantic-fetch-tags))
//  (when (eq 'using (semantic-tag-class tag)) (pp tag)))
// namespace Braced;

// (semantic-find-tags-by-class 'class)
// (semantic-find-tags-by-class 'package)

// (semantic-parse-region (point-min) (point-max) 'top_level 0 t)
// (semantic-parse-region (point-min) (point-max) 'namespace_brace_context 0 t)
// (semantic-parse-region (point-min) (point-max) 'brace_namespace_declaration 0 t)
//
// (semantic-parse-region (point-min) (point-max) 'namespace_semicolon_context 0 t)
// (semantic-parse-region (point-min) (point-max) 'semicolon_namespace_declaration 0 t)

namespace ANother;

class UnderAnother
{
}
