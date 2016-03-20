;;; grammar-wy.el --- Generated parser support file

;; Copyright (C) 2016 Andrea Turso

;; Author: Andrea Turso <trashofmasters@gmail.com>
;; Created: 2016-03-12 17:16:15+0000
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file grammar.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;

;;; Declarations
;;
(defconst grammar-wy--keyword-table
  (semantic-lex-make-keyword-table 'nil 'nil)
  "Table of language keywords.")

(defconst grammar-wy--token-table
  (semantic-lex-make-type-table 'nil 'nil)
  "Table of lexical tokens.")

(defconst grammar-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '(nil nil)
     'nil))
  "Parser table.")

(defun grammar-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic--parse-table grammar-wy--parse-table
        semantic-debug-parser-source #("grammar.wy" 0 10 (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu"))
        semantic-flex-keywords-obarray grammar-wy--keyword-table
        semantic-lex-types-obarray grammar-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;

;;; Epilogue
;;

(provide 'grammar-wy)

;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; End:

;;; grammar-wy.el ends here
