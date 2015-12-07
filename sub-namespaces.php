<?php
/**
 * Useful debugging commands
 *
 * (pp (semantic-find-tags-included))
 *
 * Some of Semantic relies on this search returning tags, therefore
 * it's important that it works: (semantic-find-tags-by-class 'include
 * (current-buffer))
 *
 * That's a macro that relies on:
 * (semantic-flatten-tags-table (current-buffer))
 *
 * (dolist (tag (semantic-flatten-tags-table (current-buffer)))
 *   (message "tag: %s, class: %s" (semantic-tag-name tag) (semantic-tag-class tag)))
 *
 * (pp (semantic-fetch-tags))
 * (progn (goto-char (point-min)) (pp (bovinate)))
 *
 * (dolist (tag (semantic-fetch-tags))
 *  (when (eq 'using (semantic-tag-class tag)) (pp tag)))
 */
namespace FirstNs;

use External;
use External\Another as AnotherAliased;
use const Math\Pi as Pie;
use function Math\Sin;
use Carbon\Carbon;

function hello()
{
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

namespace SecondNs;

use External\AnotherOneStill;

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

namespace ThirdNs;
