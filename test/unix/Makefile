# Fileutils -- A Common Lisp File Utilities Library
# Copyright (C) 2014 Kookamara LLC
#
# Contact:
#
#   Kookamara LLC
#   11410 Windermere Meadows
#   Austin, TX 78759, USA
#   http://www.kookamara.com/
#
# License: (An MIT/X11-style license)
#
#   Permission is hereby granted, free of charge, to any person obtaining a
#   copy of this software and associated documentation files (the "Software"),
#   to deal in the Software without restriction, including without limitation
#   the rights to use, copy, modify, merge, publish, distribute, sublicense,
#   and/or sell copies of the Software, and to permit persons to whom the
#   Software is furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in
#   all copies or substantial portions of the Software.
#
#   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
#   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
#   DEALINGS IN THE SOFTWARE.
#
# Original author: Jared Davis <jared@kookamara.com>

ifeq ($(LISP),)
$(error LISP is not set.)
endif

.PHONY: all clean
all:

all: paths1.spec
paths1.spec : paths1.txt paths1.pl
	./paths1.pl paths1.txt > paths1.spec

all: paths1.ok
paths1.ok : paths1.spec paths1.lisp
	@rm -f paths1.ok
	@$(LISP) < paths1.lisp
	@cat paths1.ok
	@ls -l paths1.ok
	@rm -rf $(TESTDIR)

TESTDIR := ~/.fileutils-test-temp

all: paths2.ok
paths2.ok : paths2.lisp
	@rm -rf $(TESTDIR)
	@mkdir -p $(TESTDIR)
	@touch $(TESTDIR)/foo.txt
	@touch $(TESTDIR)/bar.txt
	@touch $(TESTDIR)/.emacs
	@touch $(TESTDIR)/silly.
	@mkdir $(TESTDIR)/.hiddendir
	@mkdir $(TESTDIR)/sillydir.
	@$(LISP) < paths2.lisp
	@cat paths2.ok
	@ls -l paths2.ok
	@rm -rf $(TESTDIR)

clean:
	rm -f paths1.spec *.ok
