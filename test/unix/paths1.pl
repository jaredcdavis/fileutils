#!/usr/bin/env perl

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

use warnings;
use strict;
use File::Spec::Unix;

if (@ARGV != 1) {
    die "Usage: ./paths1.pl <INPUT-FILE>";
}

my $INFILE = $ARGV[0];
open(my $fd, $INFILE) or die("Can't open $INFILE: $!");

my @paths = ();

while (<$fd>) {
    my $path = $_;
    chomp($path);
    print "((:path . \"$path\")";
    my $abs = File::Spec::Unix->file_name_is_absolute($path);
    print " (:absolute-p . " . ($abs ? "T" : "NIL") . ")";
    my $clean = File::Spec::Unix->canonpath($path);
    print " (:clean . \"$clean\")";
    my ($vol, $dir, $file) = File::Spec::Unix->splitpath($path);
    print " (:split . (\"$vol\" \"$dir\" \"$file\"))";
    print ")\n";
    push(@paths, $path);
}

# foreach my $p1 (@paths)
# {
#     foreach my $p2 (@paths)
#     {
# 	my $cat = File::Spec::Unix->catpath($p1, $p2
#     }
# }



close($fd);

