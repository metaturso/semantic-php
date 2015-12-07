<?php

namespace FirstNs
{
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
}

namespace SecondNs
{
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
}

namespace ThirdNs
{
}
