<?php

class ClassName
{
    const CLASS_CONST = 1;

    public $publicAttribute = self::CLASS_CONST;
    protected $protectedAttribute = self::CLASS_CONST;
    private $privateAttribute = self::CLASS_CONST;

    static public $publicStaticAttribute = self::CLASS_CONST;
    static protected $protectedStaticAttribute = self::CLASS_CONST;
    static private $privateStaticAttribute = self::CLASS_CONST;

    public $arrayAttribute = [];
    public $integerAttribute = 1;
    public $loadAttribute = 1.0;
}
