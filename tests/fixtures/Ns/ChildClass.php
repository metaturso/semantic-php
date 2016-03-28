<?php

namespace Ns;

class ChildClass extends ParentClass
{
    const CHILD_CONST = 1;

    public $childPublicAttr;
    private $childPrivateAttr;
    protected $childProtectedAttr;

    public function childPublicMth() {}
    private function childPrivateMth() {}
    protected function childProtectedMth() {}
}
