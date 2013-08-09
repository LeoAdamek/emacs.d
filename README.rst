================
    EMACS.D
================

--------
 ABOUT
--------

This repository contains my emacs configuration.
I decided to craft my own configuration from scratch for simplicity and stability.
Read this to understand how it works.


--------------
 STRUCTURE
-------------

The following structure is used here:

*init.el* is, by emacs' design, where the magic starts, this file will call out to other files mostly.
*options.el* contains (mostly simple) elisp to set options
*functions.el* contains useful universal functions
*mode-helpers/* will contain files used for various modes as helpers. .e.g for "php-mode" the file will be *mode-helpers/php.el*


Hopefully this helps.
