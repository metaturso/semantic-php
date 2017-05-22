# semantic-php

[![Join the chat at https://gitter.im/trashofmasters/semantic-php](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/trashofmasters/semantic-php?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

A PHP 5.6 Wisent grammar for Emacs.

This experimental package contains an grammar which adds Emacs the
ability to extract information from php-mode buffers and provide project
navigation facilities and autocompletion.

The intent is to investigate different approaches to writing a tagging
parser, and its use to write refactoring tools for PHP.  This is not a
fully compliant PHP parser to check for parse errors, for that there's
[flymake](http://www.emacswiki.org/emacs/FlymakePhp).

## Usage

To simplify testing a new prebuilt parser is provided with every
commit. Download the source code somewhere in your filesystem and
evaluate the following code in your `*scratch*` buffer to install a
php-mode hook that will enable the parsing of PHP code:

```lisp
(add-to-list 'load-path "path/to/semantic-php")
(require 'grammar-setup)
```

If you are using [Helm](https://github.com/emacs-helm/helm) the
command `helm-semantic-or-imenu` is available to search and jump to
the tags in the buffer, and it's normally bound to the key `C-x c i`.

## Dependencies

- **Emacs 24+**

- [**php-mode**](https://github.com/ejmr/php-mode)

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
