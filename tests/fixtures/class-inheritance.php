<?php

class ParentClass
{
    const PARENT_CONST = 1;

    public $parentPublicAttr;
    private $parentPrivateAttr;
    protected $parentProtectedAttr;

    public function parentPublicMth() {}
    private function parentPrivateMth() {}
    protected function parentProtectedMth() {}
}

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

/** TODO local variables in top-level */
function test()
{
    $child = new ChildClass;
    $child->;
}
