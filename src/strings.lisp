; Fileutils -- A Common Lisp File Utilities Library
; Copyright (C) 2014 Kookamara LLC
;
; Contact:
;
;   Kookamara LLC
;   11410 Windermere Meadows
;   Austin, TX 78759, USA
;   http://www.kookamara.com/
;
; License: (An MIT/X11-style license)
;
;   Permission is hereby granted, free of charge, to any person obtaining a
;   copy of this software and associated documentation files (the "Software"),
;   to deal in the Software without restriction, including without limitation
;   the rights to use, copy, modify, merge, publish, distribute, sublicense,
;   and/or sell copies of the Software, and to permit persons to whom the
;   Software is furnished to do so, subject to the following conditions:
;
;   The above copyright notice and this permission notice shall be included in
;   all copies or substantial portions of the Software.
;
;   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;   DEALINGS IN THE SOFTWARE.
;
; Original author: Jared Davis <jared@kookamara.com>
;
; Additional copyright notice (for strings.lisp only):
;
;   This file is adapted from the ACL2 String Library, Copyright (c) 2009-2013
;   by Centaur Technology <http://www.centtech.com>, which is also released
;   under an MIT/X11 style license.

(in-package "FILEUTILS")

(declaim (inline rchars-to-string))
(defun rchars-to-string (rchars)
  ;; rchars is a list of characters
  ;; we reverse it and turn it into a string
  (nreverse (the string (coerce (the list rchars) 'string))))

(defun strtok-aux (x n xl delimiters curr acc)
  ;; x is the string we're tokenizing, xl is its length
  ;; n is our current position in x
  ;; delimiters are the list of chars to split on
  ;; curr is the current word we're accumulating in reverse order
  ;; acc is the string list of previously found words
  (declare (type string x)
           (type fixnum n xl))
  (if (eql n xl)
      (if curr
          (cons (rchars-to-string curr) acc)
        acc)
    (let* ((char1  (char x n))
           (matchp (member char1 delimiters)))
      (strtok-aux x
                  (the fixnum (+ 1 n))
                  xl
                  delimiters
                  (if matchp nil (cons char1 curr))
                  (if (and matchp curr)
                      (cons (rchars-to-string curr) acc)
                    acc)))))

(declaim (inline strtok))
(defun strtok (x delimiters)
  ;; X is a string and delimiters is a list of characters to split on.  Splits
  ;; X into a list of strings.  All characters in delimiters are removed and no
  ;; empty strings are ever found in strtok's output.
  (declare (type string x))
  (nreverse (strtok-aux x 0 (length x) delimiters nil nil)))
