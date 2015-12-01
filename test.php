<?php

namespace Foo\Ns;

use Foo\Bar\Baz;
use RuntimeException;

class DateTime
{
    public function format()
    {
        return '';
    }
}

class TestClass
{
    public $pub;
    private $pri;
    protected $pro;
    private $uninitialised; // $uninitialised: *uninitialised*
    private $string = 'helo'; // $string: string. Works
    private $partiallyQualified = new Partially\Qualified\Name(); // $partiallyQualified: Partially\Qualified\Name Works
    private $fullyQualified = new \Fully\Qualified\Name(); // $fullyQualified: Fully\Qualified\Name Works
    private $float = 1.1; // $float: number
    private $int = 1; // $int: number
    private $hex = 0xBADF00D;  // $hex: number

    public function __construct()
    {
        // (company-semantic-completions "$this->")
        // (company-semantic "$this->" "")
    }

    function impliedPublicMethod()
    {
        // (semantic-up-context) takes you between ) {
        if (true) {
            $list = [];
            $this->publicMethod();
            $date = new \DateTime();

            foreach ($list as $index => $value) {
                if ($index == 1) {
                    // (semantic-beginning-of-context) takes you to {
                    // (semantic-end-of-scope) takes you to }
                    // (semantic-narrow-to-context) also works
                }
            }
        }
    }

    public function publicMethod()
    {
    }

    protected function protectedMethod()
    {
    }

    private function privateMethod()
    {
    }

    public function setterWithOneArrayHintedParameter(array $arrayInput)
    {
    }

    private function partiallyTypeHinted(Partially\Qualified\TypeHint $typeHinted)
    {
    }

    private function fullyTypeHinted(\Fully\Qualfied\TypeHint $fullyTypeHinted)
    {
    }

    private function defaultNullParameter($null = null)
    {
    }

    private function defaultArrayParameter($param1, array $param2 = [], array $par)
    {
        $this->
    }

    private function defaultBooleanParameter($false = false, $true = true)
    {
    }

    static public function publicStaticFunction()
    {
    }

    public static function publicStaticFunctionAlternative()
    {
    }
}
