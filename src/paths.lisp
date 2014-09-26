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

(in-package "FILEUTILS")

(defmacro define-constant (name value &optional doc)
  ;; Stupid macro to avoid having defconstant forms evaluated more than once,
  ;; which I guess is technically illegal and causes problems on SBCL.
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defconstant os-kind
  ;; These features should be correctly configured by the trivial-features
  ;; library.
  #+windows :windows
  #+unix    :unix
  #+(and (not windows) (not unix))
  (error "Unrecognied host operating system."))


(defun absolute-path-p-unix (x)
  (declare (type string x))
  (let ((xl (length x)))
    (declare (type fixnum xl))
    (and (< 0 xl)
         (eql (char x 0) #\/))))

(defun absolute-path-p-windows (x)
  (declare (type string x))
  (error "Implement absolute-path-p on windows.")
  (let ((xl (length x)))
    (declare (type fixnum xl))
    ;; BOZO almost certainly wrong
    (and (< 1 xl)
         (eql (char x 1) ":"))))

(defun absolute-path-p (x)
  (check-type x string)
  (case os-kind
    (:unix     (absolute-path-p-unix x))
    (:windows  (absolute-path-p-windows x))
    (otherwise (error "Unknown operating system ~S" os-kind))))

(defun relative-path-p (x)
  (not (absolute-path-p x)))



(defun remove-leading-occurrences (lst x)
  (cond ((atom x)
         nil)
        ((member (car x) lst :test 'equal)
         (remove-leading-occurrences lst (cdr x)))
        (t
         x)))

(defun clean-path-unix (x)
  (declare (type string x))
  (if (absolute-path-p-unix x)
      (let* ((parts (strtok x '(#\/)))
             (clean (remove "." parts :test 'equal))
             (clean (remove-leading-occurrences '("/" "..") clean)))
        (concatenate 'string "/" (join clean "/")))
    (let* ((parts (strtok x '(#\/)))
           (clean (remove "." parts :test 'equal))
           (clean (if (atom clean)
                      (list ".")
                    clean)))
      (join clean "/"))))

(defun clean-path-windows (x)
  (declare (type string x))
  (error "Implement clean-path for windows."))

(defun clean-path (x)
  (check-type x string)
  (case os-kind
    (:unix     (clean-path-unix x))
    (:windows  (clean-path-windows x))
    (otherwise (error "Unknown operating system ~S" os-kind))))

;; ((:path . "/.") (:absolute-p . T) (:clean . "/") (:split . ("" "/." "")))
(defun split-path-unix (x)
  (declare (type string x))
  (let* ((last-slash (strrpos "/" x)))
    (if (not last-slash)
        (values "" "" x)
      (let* ((cutoff (+ 1 last-slash))
             (pre    (subseq x 0 cutoff))
             (post   (subseq x cutoff nil))
             (special-case-p (or (equal post ".")
                                 (equal post ".."))))
        (if special-case-p
            (values "" x "")
          (values "" pre post))))))

(defun split-path-windows (x)
  (declare (type string x))
  (error "Implement split-path for windows"))

(defun split-path (x)
  (check-type x string)
  (case os-kind
    (:unix     (split-path-unix x))
    (:windows  (split-path-windows x))
    (otherwise (error "Unknown operating system ~S" os-kind))))



(defun path-to-lisp-unix (path)
  (declare (type string path))
  ;; Horrible, nasty, probably CCL-specific code to translate a vanilla path
  ;; into a Lisp pathname that can be given to Lisp functions.
  (let* ((path  (clean-path-unix path))
         (parts (strtok path '(#\/)))
         (fixup (substitute :up ".." parts))
         (kind  (if (absolute-path-p-unix path)
                    :absolute
                  :relative))
         (final (last fixup)))
    (if (equal final :up)
        (make-pathname :directory (cons kind fixup)
                       :name nil)
      (make-pathname :directory (cons kind (butlast fixup 1))
                     :name (car (last fixup))))))

(defun path-to-lisp-windows (path)
  (declare (type string path))
  (error "Implement path-to-lisp-windows."))

(defun path-to-lisp (path)
  (check-type path string)
  (case os-kind
    (:unix    (path-to-lisp-unix path))
    (:windows (path-to-lisp-windows path))
    (otherwise
     (error "Unknown operating system ~a" os-kind))))

(defun homedir ()
  ;; BOZO totally wrong for non-unix hosts
  ;; probably even for unix
  (osicat:environment-variable "HOME"))

(defun catfile-unix (x y)
  (declare (type string x y))
  (let* ((xl (length x)))
    (if (and (< 0 xl)
             (eql (char x (- xl 1)) #\/))
        ;; Already ends with a slash.
        (concatenate 'string x y)
      (concatenate 'string x "/" y))))

(defun catfile-windows (x y)
  (declare (type string x y))
  (error "Implement catfile on windows."))

(defun catfile (x y)
  (check-type x string)
  (check-type y string)
  (case os-kind
    (:unix     (catfile-unix x y))
    (:windows  (catfile-windows x y))
    (otherwise (error "Unknown operating system ~S" os-kind))))






(define-constant +path-kinds+
  '(nil
    :regular-file
    :directory
    :symbolic-link
    :broken-symbolic-link
    :pipe
    :socket
    :character-device
    :block-device))

(defun path-type (path &key follow-symlinks)
  (check-type path string)
;  #+(or cmucl allegro)
;  (error "fileutils:path-type doesn't work on allegro currently; see
;          https://github.com/osicat/osicat/issues/2 for possible updates")
  (multiple-value-bind
      (main-kind broken-p)
      (osicat:file-kind (path-to-lisp path)
                        :follow-symlinks follow-symlinks)
    (cond ((and (eq main-kind :symbolic-link)
                broken-p)
           :broken-symbolic-link)
          ((member main-kind +path-kinds+)
           main-kind)
          (t
           (error "Unrecognized result from osicat:file-kind for ~S: ~S"
                  path main-kind)))))

(defun path-exists-p (path)
  (check-type path string)
  (if (path-type path)
      t
    nil))

(defun regular-file-p (path)
  (check-type path string)
  (eq (path-type path) :regular-file))

(defun directory-p (path)
  (check-type path string)
  (eq (path-type path) :directory))
