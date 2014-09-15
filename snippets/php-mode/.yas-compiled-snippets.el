;;; Compiled snippets and support files for `php-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'php-mode
                     '(("class" "/**\n * $1 Class\n *\n * $1 Class Documentation\n *\n * @package $2\n * @author Leo Adamek\n */\nclass $1 {\n\n}\n      \n " "PHP Class Header" nil nil nil nil nil nil)
                       ("gplv2" "/**\n * © `(format-time-string \"%Y\")` `(if (boundp 'file-header-copyright) (file-header-copyright))`\n *\n * This program is free software; you can redistribute it and/or\n * modify it under the terms of the GNU General Public License\n * as published by the Free Software Foundation; either version 2\n * of the License, or (at your option) any later version.\n *\n * This program is distributed in the hope that it will be useful,\n * but WITHOUT ANY WARRANTY; without even the implied warranty of\n * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n * GNU General Public License for more details.\n *\n * You should have received a copy of the GNU General Public License\n * along with this program; if not, write to the Free Software\n * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, US.A\n *\n * @license GPLv2+ <https://www.gnu.org/licenses/gpl-2.0.html>\n * @copyright © `(format-time-string \"%Y\")` `(if (boundp 'file-header-copyright) (file-header-copyright))`\n * @author `(if (boundp 'file-header-author) (file-header-author))`\n * \n * `(if (boundp 'file-header-comment) (file-header-comment))`\n */" "GPLv2 Notice with Copyright" nil nil nil nil nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'php-mode
                     '(("dbo" "$db = JFactory::getDbo();\n$q  = $db->getQuery(true);\n\n$q->select(${1:\"*\"})\n  ->from(\"#__$2\")\n" "Joomla Database Query" nil
                        ("Joomla Development")
                        nil nil nil nil)
                       ("jexec" "/*\n * Disallow direct access to this file\n * Ensure it is called from within Joomla.\n */\ndefined('_JEXEC') or die('Direct execution of this file disallowed');\n" "Joomla _JEXEC Check" nil
                        ("PHP")
                        nil nil nil nil)))


;;; Do not edit! File generated at Mon Sep 15 11:32:56 2014
