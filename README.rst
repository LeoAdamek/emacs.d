=====================
 Emacs Configuration
=====================

ABOUT
=====

This repository contains my emacs configuration.
I decided to craft my own configuration from scratch for simplicity and stability.
Read this to understand how it works.

Some things aren't perfect, but I'm still somewhat new to elsip :wink:


STRUCTURE
=========

The following structure is used here:


* *init.el* is, by emacs' design, where the magic starts, this file will call out to other files mostly.
* *lisp/options.el* contains (mostly simple) elisp to set options
  It also contains some definitions for custom key-bndings,
  and extra definitions for a few useful minor mode.
* *lisp/functions.el* contains useful universal functions
* *lisp/packages.el* Contains definitions for packages to be installed, and packages to be loaded


Hopefully this helps.

KEYBINDINGS
===========

This emacs is equipped with some custom keybindings for some added awesomeness.

+--------------------+--------------------+
| **Key Binding**    | **Action**         |
|                    |                    |
+--------------------+--------------------+
| <kbd>C-c g</kbd>   | Show Magit Summary |
|                    |                    |
+--------------------+--------------------+
| <kbd>C-c f f</kbd> | Toggle Fold        |
|                    |                    |
+--------------------+--------------------+
| <kbd>C-c f a</kbd> | Toggle all Folds   |
+--------------------+--------------------+




PACKAGES
========

The file *packages.el* defines a list of default packages called *package-list*
By default these packages will all be installed. The following repositories are also added.

* MELPA (I know this isn't a popular choice, and I will look at using git submodules in place of MELPA)
* Marmalade Repo
* ELPA (the default repo)
