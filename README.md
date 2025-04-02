 Emacs Configuration
=====================

ABOUT
-----

This repository contains my emacs configuration.
I decided to craft my own configuration from scratch for simplicity and stability.
Read this to understand how it works.


STRUCTURE
---------

The following structure is used here:


* ``init.el`` is, by emacs' design, where the magic starts, this file will call out to other files mostly.
* ``lisp/options.el`` contains (mostly simple) elisp to set options
  It also contains some definitions for custom key-bndings,
  and extra definitions for a few useful minor mode.
* ``lisp/functions.el`` contains useful universal functions
* ``lisp/packages.el`` Contains definitions for packages to be installed, and packages to be loaded


Hopefully this helps.

KEYBINDINGS
-----------

This emacs is equipped with some custom keybindings for some added awesomeness.


| **Key Binding**    | **Action**         |
| :----------------: | :----------------: |
| <kbd>C-c g</kbd>   | Show Magit Summary |
| <kbd>C-c f f</kbd> | Toggle Fold        |
| <kbd>C-c f a</kbd> | Toggle all Folds   |
| <kbd>C-c f F</kbd> | Toggle fold        |
|                    | recusively         |
| <kbd>C-c f A</kbd> | Toggle all folds   |
|                    | recursively        |
| <kbd>C-c p</kbd>   | Projectile prefix  |


PACKAGES
--------

The file *packages.el* defines a list of default packages called *package-list*
By default these packages will all be installed. The following repositories are also added.

* MELPA 
* ELPA (the default repo)

### Really Useful™ Packages Included

The following packages are included by default which are really useful:

* `magit` -- Magical Git integration
* `yasnippet` -- Snippet system + custom snippets
* `origami` -- Flexible code folding
* `moe-theme` -- Excellent theme, so kawaii~
* `nxml-mode` -- XML editing tools.
* `projectile` -- Project handling for emacs
* `company` -- Great auto completion for emacs.
