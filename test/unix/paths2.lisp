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

(in-package "CL-USER")
(ql:quickload "fileutils")

(defmacro defunc (&rest args)
  `(compile (defun . ,args)))

(defunc should-exist (paths)
  (loop for path in paths do
        (format t "Should exist: ~S~%" path)
        (unless (fileutils:path-exists-p path)
          (error "~S: Expected path to exist.~%" path))))

(defunc should-not-exist (paths)
  (loop for path in paths do
        (format t "Should not exist: ~S~%" path)
        (when (fileutils:path-exists-p path)
          (error "~S: Expected path to not exist.~%" path))))

(defvar *home*    (fileutils:homedir))
(defvar *testdir* (fileutils:catfile *home* ".fileutils-test-temp"))

(defunc test-path-exists ()
  (should-exist
   (list "/"
         "Makefile"
         "./"
         "../"
         "./Makefile"
         "./../unix/Makefile"
         *home*
         (fileutils:catfile *home* ".fileutils-test-temp")
         (fileutils:catfile *home* ".fileutils-test-temp/")
         (fileutils:catfile *testdir* "foo.txt")
         (fileutils:catfile *testdir* "bar.txt")
         (fileutils:catfile *testdir* ".emacs")
         (fileutils:catfile *testdir* "silly.")
         (fileutils:catfile *testdir* ".hiddendir")
         (fileutils:catfile *testdir* ".hiddendir/")
         (fileutils:catfile *testdir* "sillydir.")
         (fileutils:catfile *testdir* "sillydir./")
         (fileutils:catfile *testdir* "..")
         (fileutils:catfile *testdir* "../.fileutils-test-temp")
         ))

  (should-not-exist
   (list
    (fileutils:catfile *testdir* "baz.txt")
    "~"
    "~/.fileutils-test-temp"
    "~/.fileutils-test-temp/foo.txt")))

(defunc tests-top ()
  (format t "Starting tests.~%")
  (test-path-exists)
  (format t "All tests passed, writing .ok file.~%")
  (with-open-file (out "paths2.ok"
                       :direction :output
                       :if-exists :supersede)
                  (format out "OK: Everything seems fine.~%")))

(tests-top)
(uiop:quit)
