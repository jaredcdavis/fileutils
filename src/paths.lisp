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


(defstruct config
  ;; Kind of file system we are working with.

  (kind
   ;; :UNIX or :WINDOWS.
   ;; Governs:
   ;;   - Do we look for volumes like C:\
   ;;   - What are the valid directory separators (windows: \ and /; unix: /)
   :unix)

  (case-sensitive-p
   ;; T or NIL
   t))



(defstruct path
  ;; Internal (parsed) representation of a single path.  (There's no notion of
  ;; a "wild" pathname as in Lisp.)

  (type
   ;; One of :ABSOLUTE or :RELATIVE.
   ;;    Indicates whether this is an absolute or relative path.
   :relative)

  (vol
   ;; String or NIL.
   ;;    For Unix paths or relative paths on Windows, this is NIL.
   ;;    For absolute paths on Windows, this is the drive letter with a
   ;;    colon, e.g., "C:" or "D:".
   nil)

  (parts
   ;; String list.
   ;;    This is the list of file name components in the order you would
   ;;    expect.  For instance:
   ;;
   ;;     - For a path like "/home/jared/hello.txt", this would be
   ;;         ("home" "jared" "hello.txt").
   ;;
   ;;     - For a relative path like "../share/images", this would be
   ;;         (".." "share" "images")
   ;;
   ;; The parts need not be canonical.  That is, it is valid to have
   ;; parts such as:
   ;;      ("home" "jared" "Downloads" ".." "Documents")
   ;; instead of just:
   ;;      ("home" "jared" "Documents")
   ;;
   ;; The empty part-list is valid.  It should be interpreted as (".")
   nil))

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
