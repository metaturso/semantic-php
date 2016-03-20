<?php
/**
 * Compiles builtin PHP symbols like classes, interfaces and functions
 * to feed the Semantic database with data common to all PHP scripts.
 */
const BUILD_DIR = __DIR__ . "/build/";
const CLASS_DIR = BUILD_DIR . "classes/";

$declaredClasses = get_declared_classes();
$declaredInterfaces = get_declared_interfaces();

require __DIR__ . '/vendor/autoload.php';

use Zend\Code\Generator\ClassGenerator;
use Zend\Code\Reflection\ClassReflection;

@mkdir(BUILD_DIR);
@mkdir(CLASS_DIR);

foreach ($declaredInterfaces + $declaredClasses as $className) {
    /**
     * Reverse engineer all built-in classes and save the generated source
     * code in the build directory.
     */
    try {
        $class = ClassGenerator::fromReflection(
            new ClassReflection($className)
        );

        echo "Generating {$className}..." . PHP_EOL;
        if (! file_put_contents(
            CLASS_DIR . $className . ".php",
            $class->generate()
        )) {
            throw new RuntimeException("Failed to generate: {$className}!");
        }
    } catch (Exception $e) {
        echo $e->getMessage() . PHP_EOL;
    }
}

/**
 * Generate a summary file containing the name of all builtin classes.
 */
file_put_contents(
    "build/classes/summary.el",
    sprintf(
        "(defvar semantic-php-builtin-classes '(\n\t\"%s\"\n))",
        implode("\"\n\t\"", $declaredClasses)
    )
);
