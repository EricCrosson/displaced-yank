;;; displaced-yank.el --- Yank and move point
;; Version: 0.0.20140311

;; Copyright (C) 2015  esc

;; Author: Eric Crosson <esc@ericcrosson.com>
;; Keywords: yank
;; Package-Version: 0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bind the generated functions to a key and you're good to go

;;; Usage:

;; here is how I define many displaced-yank commands at once

;; (mapc (lambda (function)
;; 	(let ((funcname (car function))
;; 	      (data     (cdr function)))
;; 	  (eval `(define-displaced-yank ,funcname ,data))))
;;       '((parens              "()")
;; 	(braces              "{}")
;; 	(brackets            "[]")
;; 	(brackets-with-colon "[:]")
;; 	(pipes               "||")
;; 	(chevrons            "<>")
;; 	(quotes              "\"\"")
;; 	(single-quotes       "''")
;; 	(stars               "**")
;; 	(dollars             "$$")
;; 	(equals              "==")
;; 	;; a good example of code reuse
;; 	(ticks               "`'")
;; 	(little-arrow        "->" 0)
;; 	(doxygen-comment     "/*!  */" 3)))


;; I then bind the generated defuns like so

;; (bind-key "C-M--"  'yank-displaced-little-arrow)
;; (bind-key "C-M-j"  'yank-displaced-parens)
;; (bind-key "C-M-k"  'yank-displaced-braces)
;; (bind-key "C-M-|"  'yank-displaced-pipes)
;; (bind-key "C-M-l"  'yank-displaced-brackets)
;; (bind-key "C-M-,"  'yank-displaced-chevrons)
;; (bind-key "C-M-'"  'yank-displaced-single-quotes)
;; (bind-key "C-M-\"" 'yank-displaced-quotes)
;; (bind-key "C-M-g"  'yank-displaced-dollars)
;; (bind-key "C-M-;"  'yank-displaced-stars)
;; (bind-key "C-M-:"  'yank-displaced-doxygen-comment)
;; (bind-key "C-M-="  'yank-displaced-equals)
;; (bind-key "C-M-`"  'yank-displaced-ticks))

;;; Code:

;;;###autoload
(defmacro define-displaced-yank (funcname data)
  "Create a defun of name FUNCNAME that yanks and moves according
to DATA. DATA is of the form (STR, MOVE). STR is the string to
yank and MOVE is the number of chars to move backward.

Note that negative values of MOVE are valid."
  (let ((funsymbol (intern (format "yank-displaced-%s" funcname)))
        (docstring (format "(insert \"%s\") and (backward-char %d).
This command can be prefixed, and will iterate N times."
			   (car data) (or (cadr data) 1)))
        (char (car data))
        (back (or (cadr data) 1)))
    `(defun ,funsymbol (&optional N)
       ,docstring
       (interactive "p")
       (dotimes (i (or N 1))
         (insert ,char)
         (backward-char ,back)))))

(provide 'displaced-yank)

;;; displaced-yank.el ends here
