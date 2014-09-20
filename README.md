Common Lisp File Utilities
==========================

### Work in progress, not ready for public consumption

 * [installation instructions](INSTALL.md)
 * [documentation](doc/index.html)

I want a sensible way to work with Unix and Windows file systems from
Common Lisp.

Possible starting points:

* Read about [files and file i/o](http://www.gigamonkeys.com/book/files-and-file-io.html)
  from Peter Seibel's excellent [Practical Common Lisp](http://www.gigamonkeys.com/book/)
  book.

* Use [Quicklisp](http://www.quicklisp.org/) to install libraries such as

  * [CL-FAD](http://weitz.de/cl-fad/) which provides some basic functions like
    `file-exists-p`, `pathname-as-file`, `list-directory`, `copy-file`, etc.  It
    has a nice manual.  It is based on pathnames.

  * [OSICAT](http://common-lisp.net/project/osicat/) which provides many similar
    functions, and also has operations like `file-permissions`, `make-link`,
    `read-link`, `file-kind`, and so forth.

  * UIOP, part of [ASDF](http://common-lisp.net/projects/asdf/), which
    allegedly has a README file but it's a broken link, but as of this
    writing there is a
    [manual](http://bimib.disco.unimib.it/people/Marco.Antoniotti/Projects/CL/HELAMBDAP/tests/asdf-uiop/docs/html/dictionary/dictionary.html)
    here, and it is allegedly [recommended](http://cliki.net/CL-FAD)
    over CL-FAD now by Fare, who also recommends
    [IOLIB](http://common-lisp.net/project/iolib/) except that doesn't
    make any sense because the [IOLIB
    manual](http://common-lisp.net/project/iolib/manual/) doesn't say
    anything about pathnames and the
    [tutorial](http://pages.cs.wisc.edu/~psilord/blog/data/iolib-tutorial/tutorial.html)
    that mentions that IOLIB includes a pathname library, but then
    never again even includes the word `pathname`, and so this is very
    frustrating.  Anyway, UIOP seems to have a lot of the CL-FAD/OSICAT
    functions and also new stuff like `pathname-parent-directory-pathname`.

Hey this might be really useful

 * http://fare.tunes.org/files/asdf3/asdf3-2014.html#%28part._pathnames%29





After trying to work with this stuff for a good while, I've concluded that

* I still don't understand a lot about Common Lisp pathnames.

* It is really hard and frustrating to try to implement functions like
  `basename` and `dirname` in any sensible way, even on a single Lisp.

* There are no guarantees and no way to know your code is portable,
  because the pathname construct gives CL implementations too much
  flexibility.

* It just shouldn't be this hard.


# Fileutils

It seems like we could do a lot better by trying to entirely avoid Lisp
pathnames.  I want something that is:

* Portable across many Lisps and operating systems.

* Relatively modest in its functionality.

* Well documented.

* Well tested.






