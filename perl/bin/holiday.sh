#!/bin/sh

# ==============================================================================

# License
# =======

# Copyright (c) 2006-2010: Nagler & Company

# Permission is hereby granted, free of charge, to any person obtaining a copy of
# this software and associated documentation files (the "Software"), to deal in
# the Software without restriction, including without limitation the rights to
# use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
# of the Software, and to permit persons to whom the Software is furnished to do
# so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
# FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
# COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
# IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
# CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# ==============================================================================

# Author: Thorsten Rangwich, originally written for Nagler & Company.
#
# Starts up the holiday server. Called with --compile does a perl compile instead.
# No command line parameters except for --compile.

BaseDir="`dirname "$0"`"
BaseDir="`cd "$BaseDir/.." ; pwd`"
PERL5LIB="$BaseDir/lib/perl:$PERL5LIB"
export PERL5LIB
cd "$BaseDir/etc"
if [ "$1" = "--compile" ] ; then
  Interpreter="`head -1 "$BaseDir"/bin/holiday.pl | cut -c 3-`"
  $Interpreter -wc "$BaseDir"/bin/holiday.pl
else
  "$BaseDir"/bin/holiday.pl
fi
