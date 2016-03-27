<?php

class ParentClass
{
    const PARENT_CONST = 1;

    public $parentPublic;
    private $parentPrivate;
    protected $parentProtected;

    public function parentPublic() {}
    private function parentPrivate() {}
    protected function parentProtected() {}
}

class ChildClass extends ParentClass
{
    const CHILD_CONST = 1;

    public $childPublic;
    private $childPrivate;
    protected $childProtected;

    public function childPublic() {}
    private function childPrivate() {}
    protected function childProtected() {}
}

/** TODO local variables in top-level */
function test()
{
    $child = new ChildClass;
    $child->;
}
