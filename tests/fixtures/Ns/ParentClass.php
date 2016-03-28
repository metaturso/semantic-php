<?php

namespace Ns;

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
