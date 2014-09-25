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

(defun read-entries (stream)
  (let ((entry1 (read stream nil nil)))
    (if (not entry1)
        nil
      (cons entry1
            (read-entries stream)))))

(defvar *entries*
  (with-open-file (in "unix.spec")
    (read-entries in)))

(defun check-entry (entry)
  (let ((path       (cdr (assoc :path entry)))
        (absolute-p (cdr (assoc :absolute-p entry)))
        (clean      (cdr (assoc :clean entry)))
        (split      (cdr (assoc :split entry))))
    (unless (stringp path)
      (error "Path isn't even a string? ~S" entry))
    (unless (equal absolute-p (fileutils:absolute-path-p path))
      (error "~S: Expected absolute-p ~S, but got ~S.~%"
             path absolute-p (fileutils:absolute-path-p path)))
    (unless (equal clean (fileutils:clean-path path))
      (error "~S: Expected clean ~S, but got ~S.~%"
             path clean (fileutils:clean-path path)))

  (multiple-value-bind (vol dirs file)
    (fileutils:split-path path)
    (unless (equal vol (first split))
      (error "~S: split-path: expected volume ~S, but got ~S.~%"
             path vol (first split)))
    (unless (equal dirs (second split))
      (error "~S: split-path: expected dirs ~S, but got ~S.~%"
             path dirs (second split)))
    (unless (equal file (third split))
      (error "~S: split-path: expected filename ~S, but got ~S.~%"
             path file (third split))))))

(defun should-exist (paths)
  (loop for path in paths do
        (unless (fileutils:path-exists-p path)
          (error "~S: Expected path to exist.~%" path))))

(defun should-not-exist (paths)
  (loop for path in paths do
        (when (fileutils:path-exists-p path)
          (error "~S: Expected path to not exist.~%" path))))

(defvar *home*    (fileutils:homedir))
(defvar *testdir* (fileutils:catfile *home* ".fileutils-test-temp"))

(defun test-path-exists ()
  (should-exist
   (list "/"
         "Makefile"
         "./"
         "../"
         "./Makefile"
         "./../test/Makefile"
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

(progn
  (loop for entry in *entries* do (check-entry entry))
  (test-path-exists)
  (with-open-file (out "unix.ok"
                       :direction :output
                       :if-exists :supersede)
    (format out "OK: Checked ~a entries and all tests passed.~%"
            (length *entries*))))

