;;; short-lambda.el --- Clojure-style anonymous function literal for Elisp

;; Copyright (C) 2015 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/short-lambda
;; Version: 0.1.0
;; Keywords: extensions, internal

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package allows to do this:
;;
;; (cl-mapcar #(concat %1 " are " %2)
;;            '("roses" "violets")
;;            '("red" "blue"))
;; ;; => '("roses are red" "violets are blue")
;;
;; Or this:
;;
;; (mapc #(put % 'disabled nil)
;;       '(upcase-region downcase-region narrow-to-region))
;;
;; This assumes that there is a reader macro in the Emacs C code that
;; translates #(STRUCTURE) to (short-lambda STRUCTURE), in the same
;; way as for the `backquote' macro.

;;; Code:

;;;###autoload
(defmacro short-lambda (structure)
  "Generate a lambda with a body of STRUCTURE.

The lambda arguments are auto-generated, based on these rules:

1. Up to nine arguments of the form `%1', ..., `%9' are allowed,
in addition to a &rest-style argument `%&'.

2. In the case when `%1' is the only argument, besides possibly
`%&`, it may be abbreviated to `%'.

3. The lower-ranking arguments are auto-added, even if they are
not present in STRUCTURE, i.e:

    (short-lambda (list %3)) => (lambda (%1 %2 %3) (list %3))
"
  `(lambda ,(sl--arguments structure) ,structure))

(defun sl--arguments (tree)
  "Collect a list of unique occurences of %, %1, ..., %9, %& in TREE."
  (let ((args (delete-duplicates
               (sl--collect-rec tree))))
    (cond ((equal args '(%))
           '(%))
          ((equal args '(%&))
           '(&rest %&))
          ((or (equal args '(% %&))
               (equal args '(%& %)))
           '(% &rest %&))
          ((member '% args)
           (error "%% should not be in %s, use %%1 instead" args))
          (t
           (let* ((max-arg
                   (cl-reduce (lambda (a b)
                                (let ((nb (symbol-name b)))
                                  (if (string< a nb)
                                      nb
                                    a)))
                              args
                              :initial-value "%"))
                  (max-int-arg (string-to-int (substring max-arg 1)))
                  (num-args (mapcar (lambda (x) (intern (format "%%%d" x)))
                                    (number-sequence 1 max-int-arg))))
             (if (memq '%& args)
                 `(,@num-args &rest %&)
               num-args))))))

(defun sl--collect-rec (tree)
  "Collect a list of occurences of %, %1, ..., %9, %& in TREE."
  (if (consp tree)
      (nconc (sl--collect-rec (car tree))
             (sl--collect-rec (cdr tree)))
    (when (and (symbolp tree)
               (string-match "^%[1-9&]?$" (symbol-name tree)))
      (list tree))))

(provide 'short-lambda)

;;; short-lambda.el ends here
