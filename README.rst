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
--------------

The following structure is used here:

*init.el* is, by emacs' design, where the magic starts, this file will call out to other files mostly.
*options.el* contains (mostly simple) elisp to set options
*functions.el* contains useful universal functions
*mode-helpers/* will contain files used for various modes as helpers. .e.g for "php-mode" the file will be *mode-helpers/php.el* (it doesn't do anything yet)


Hopefully this helps.

-------------
 PACKAGES
-------------

The file *packages.el* defines a list of default packages called *package-list*
By default these packages will all be installed. The following repositories are also added.

* MELPA
* Marmalade Repo
* ELPA (the default repo)

The following packages are listed in *package-list*

+--------------------+--------------------+
| *Package Name*     | *Description*      |
|                    |                    |
+--------------------+--------------------+
| *php-mode*         | Major mode for PHP |
|                    | editing            |
+--------------------+--------------------+
| *nxml*             | Mssibely improved  |
|                    | XML editing        |
|                    |                    |
|                    |                    |
+--------------------+--------------------+
| *web-mode*         | Super-amazing mdoe |
|                    |for editing web     |
|                    |pages, allows for   |
|                    |embedded languages  |
|                    | like Javascript,   |
|                    | PHP and CSS inside 
|                    | of HTML            |
|                    |                    |
+--------------------+--------------------+
| *mmm-mode*         | Enables usage of   |
|                    | multiple major     |
|                    | modes              |
|                    |                    |
+--------------------+--------------------+
