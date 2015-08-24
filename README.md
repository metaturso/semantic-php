# semantic-php

A PHP 5.6 Wisent grammar for Emacs.

This experimental package contains an grammar which adds Emacs the
ability to extract information from php-mode buffers and provide project
navigation facilities and autocompletion.

The intent is to investigate different approaches to writing a tagging
parser, and its use to write refactoring tools for PHP.  This is not a
fully compliant PHP parser to check for parse errors, for that there's
[flymake](http://www.emacswiki.org/emacs/FlymakePhp).

## Dependencies

- **Emacs 24+**

- [**php-mode**](github.com/ejmr/php-mode)

	This projects adds behaviour to PHP mode, so this is a necessary
requirement.

	Although not strictly required for the parsing php-mode takes care
of font-locking and all has all other whistle and bells that make it
useful.

- [**ede-php-autoload**](https://github.com/stevenremot/ede-php-autoload)

	This project uses ede-php-autoload to integrate with the EDE
project manager.

	The `ede-php-autoload-find-class-def-file` function is used to
resolve fully qualified names to a path using PSR mappings.

	**Note to Emacs 25 users with CEDET 2** You will encounter
	warnings caused by deprecated stuff in the Emacs EIEIO object
	system. This requires changes in ede-php-autoload, but this should
	not prevent this parser from working.

- [**CEDET**](https://sourceforge.net/p/cedet/git/ci/master/tree)

	This package is built and tested with the CEDET version of Emacs
25, but I expect it to work without a hitch on Emacs 24.4 as well.

## Useful Resources

Here's a list of resources which I have found essential to setup a
workable grammar development workflow. Some of the links point to what
I believe are old articles so a bit of reconciliation will be
required, as functions have certainly been renamed or deprecated in
different versions of CEDET and Emacs.

- [http://cedet.sourceforge.net/addlang.shtml](http://cedet.sourceforge.net/addlang.shtml)
- [http://www.randomsample.de/cedetdocs/semantic-langdev/index.html](http://www.randomsample.de/cedetdocs/semantic-langdev/index.html)
- [http://fossies.org/linux/emacs/doc/misc/wisent.texi](http://fossies.org/linux/emacs/doc/misc/wisent.texi)
- [http://www.randomsample.de/cedetdocs/grammar-fw/Grammar-Development-Cycle.html#Grammar-Development-Cycle](http://www.randomsample.de/cedetdocs/grammar-fw/Grammar-Development-Cycle.html#Grammar-Development-Cycle)
- [http://www.gnu.org/software/emacs/manual/html_node/semantic/Analyzer-Debug.html#Analyzer-Debug](http://www.gnu.org/software/emacs/manual/html_node/semantic/Analyzer-Debug.html#Analyzer-Debug)
