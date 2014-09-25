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

(defconstant os-kind
  ;; These features should be correctly configured by the trivial-features
  ;; library.
  #+windows :windows
  #+unix    :unix
  #+(and (not windows) (not unix))
  (error "Unrecognied host operating system."))

;; (defconstant get-os
;;   #+unix :unix
;;   #+(and (not unix) mswindows 

(defstruct config
  ;; Kind of file system we are working with.

  (kind
   ;; :UNIX or :WINDOWS.
   ;; Governs:
   ;;   - Do we look for volumes like C:\
   ;;   - What are the valid directory separators (windows: \ and /; unix: /)
   os-kind)

  (case-sensitive-p
   ;; T or NIL
   t)

  ;; BOZO other kinds of stuff?
  )

(defvar *default-config*
  (make-config))

(defun absolute-path-p-unix (x)
  (declare (type string x))
  (let ((xl (length x)))
    (declare (type fixnum xl))
    (and (< 0 xl)
         (eql (char x 0) #\/))))

(defun absolute-path-p-windows (x)
  (declare (type string x))
  (let ((xl (length x)))
    (declare (type fixnum xl))
    ;; BOZO almost certainly wrong
    (and (< 1 xl)
         (eql (char x 1) ":"))))

(defun absolute-path-p (x &key (config *default-config*))  ;; Public
  (check-type x string)
  (check-type config config)
  (let ((kind (config-kind config)))
    (case kind
      (:unix     (absolute-path-p-unix x))
      (:windows  (absolute-path-p-windows x))
      (otherwise (error "Unknown file system configuration ~a" kind)))))

(defun relative-path-p (x &key (config *default-config*))  ;; Public
  (not (absolute-path-p x :config config)))


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

(defun clean-path (x &key (config *default-config*)) ;; Public
  (check-type x string)
  (check-type config config)
  (let ((kind (config-kind config)))
    (case kind
      (:unix     (clean-path-unix x))
      (:windows  (clean-path-windows x))
      (otherwise (error "Unknown file system configuration ~a" kind)))))

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

(defun split-path-windows
  (declare (type string x))
  (error "Implement split-path for windows"))

(defun split-path (x &key (config *default-config*)) ;; Public
  (check-type x string)
  (check-type config config)
  (let ((kind (config-kind config)))
    (case kind
      (:unix     (split-path-unix x))
      (:windows  (split-path-windows x))
      (otherwise (error "Unknown file system configuration ~a" kind)))))






(defun make-lisp-pathname-unix (path)
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

(defun make-lisp-pathname-windows (path)
  (declare (type string path))
  (error "Implement make-lisp-pathname-windows."))

(defun make-lisp-pathname (path)
  (check-type path string)
  (case os-kind
    (:unix    (make-lisp-pathname-unix path))
    (:windows (make-lisp-pathname-windows path))
    (otherwise
     (error "Unknown operating system ~a" os-kind))))



(defconstant path-kinds
  '(nil
    :regular-file
    :directory
    :symbolic-link
    :broken-symbolic-link
    :pipe
    :socket
    :character-device
    :block-device))

(defun path-type (path)
  (check-type path string)
  (multiple-value-bind
      (main-kind broken-p)
      (osicat:file-kind (make-lisp-pathname-unix path)
                        :follow-symlinks t)
    (cond ((and (eq main-kind :symbolic-link)
                broken-p)
           :broken-symbolic-link)
          ((member main-kind path-kinds)
           main-kind)
          (t
           (error "Unrecognized result from osicat:file-kind for ~S: ~S"
                  path main-kind)))))

(defun path-exists-p (path)
  (check-type path string)
  (if (path-type path)
      t
    nil))


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

(defun catfile (x y &key (config *default-config*))
  (check-type x string)
  (check-type y string)
  (check-type config config)
  (let ((kind (config-kind config)))
    (case kind
      (:unix     (catfile-unix x y))
      (:windows  (catfile-windows x y))
      (otherwise (error "Unknown file system configuration ~a" kind)))))




#|| 

; perl file::spec

;; canonpath -- no physical check of the filesystem, but a logical cleanup of path
;; does not canonicalize x/../y into y by design: apparently symlinks can futz
;; with where .. points, so a naive ../ removal isn't really correct.  to really
;; resolve this sort of thing you have to look at the file system

; catdir
; concatenate directories to form a complete path ending with a directory

; catfile - concatenate directory names and a filename, to form a complete path
; ending with a filename

; curdir - string representation of the current directory

; devnull - string representation of the null device

; rootdir - string representation of the root directory

; tmpdir - string representation of the first writable directory from a list of possible
; temporary directories.  current directory if no writable temporary directories are
; found.  depends on the OS, e.g., unix checks env[tmpdir] and /tmp

; updir -- string representation of the parent directory

; no_upwards -- filter from a directory list those that refer to the parent directory?

; case_tolerant -- true or false, indicating whether alphabetic case is significant
; when comparing files.  cygwin and win32 accept an optional drive argument


file_name_is_absolute
Takes as its argument a path, and returns true if it is an absolute path.

path
Takes no argument. Returns the environment variable PATH (or the local platform's equivalent) as a list.

join
join is the same as catfile.

splitpath - Splits a path in to volume, directory, and filename portions. On
systems with no concept of volume, returns '' for volume.

For systems with no syntax differentiating filenames from directories, assumes
that the last file is a path unless $no_file is true or a trailing separator or
/. or /.. is present. On Unix, this means that $no_file true makes this
return ( '', $path, '' ).

The directory portion may or may not be returned with a trailing '/'.
The results can be passed to catpath() to get back a path equivalent to (usually identical to) the original path.

splitdir
The opposite of catdir.
    @dirs = File::Spec->splitdir( $directories );
$directories must be only the directory portion of the path on systems that have the concept of a volume or that have path syntax that differentiates files from directories.
Unlike just splitting the directories on the separator, empty directory names ('' ) can be returned, because these are significant on some OSes.

catpath()
Takes volume, directory and file portions and returns an entire path. Under Unix, $volume is ignored, and directory and file are concatenated. A '/' is inserted if need be. On other OSes, $volume is significant.
    $full_path = File::Spec->catpath( $volume, $directory, $file );

abs2rel
Takes a destination path and an optional base path returns a relative path from the base path to the destination path:
    $rel_path = File::Spec->abs2rel( $path ) ;
    $rel_path = File::Spec->abs2rel( $path, $base ) ;
If $base is not present or '', then Cwd::cwd() is used. If $base is relative, then it is converted to absolute form using rel2abs(). This means that it is taken to be relative to Cwd::cwd().
On systems with the concept of volume, if $path and $base appear to be on two different volumes, we will not attempt to resolve the two paths, and we will instead simply return $path . Note that previous versions of this module ignored the volume of $base , which resulted in garbage results part of the time.
On systems that have a grammar that indicates filenames, this ignores the $base filename as well. Otherwise all path components are assumed to be directories.
If $path is relative, it is converted to absolute form using rel2abs(). This means that it is taken to be relative to Cwd::cwd().
No checks against the filesystem are made. On VMS, there is interaction with the working environment, as logicals and macros are expanded.
Based on code written by Shigio Yamaguchi.

rel2abs()
Converts a relative path to an absolute path.
    $abs_path = File::Spec->rel2abs( $path ) ;
    $abs_path = File::Spec->rel2abs( $path, $base ) ;
If $base is not present or '', then Cwd::cwd() is used. If $base is relative, then it is converted to absolute form using rel2abs(). This means that it is taken to be relative to Cwd::cwd().
On systems with the concept of volume, if $path and $base appear to be on two different volumes, we will not attempt to resolve the two paths, and we will instead simply return $path . Note that previous versions of this module ignored the volume of $base , which resulted in garbage results part of the time.
On systems that have a grammar that indicates filenames, this ignores the $base filename as well. Otherwise all path components are assumed to be directories.
If $path is absolute, it is cleaned up and returned using canonpath.
No checks against the filesystem are made. On VMS, there is interaction with the working environment, as logicals and macros are expanded.
Based on code written by Shigio Yamaguchi.

(defun normalize-parts (parts)
  (if (endp parts)
      nil
  (

(define canonicalize (p)
  (check-type p path)
  
||#
