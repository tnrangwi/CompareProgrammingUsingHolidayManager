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

# Short script that generates all documentation for the project.

#FIXME: Does not do any error handling up to now!
#FIXME: This is fully undocumented.

BaseDir="`dirname "$0"`"
BaseDir="`cd "$BaseDir/.." ; pwd`"
if [ ! -d "$BaseDir/doc/html" ] ; then mkdir -p "$BaseDir"/doc/html ; fi
if [ ! -d "$BaseDir/doc/html/main_pl" ] ; then mkdir "$BaseDir"/doc/html/main_pl ; fi
if [ ! -d "$BaseDir/doc/html/NC" ] ; then mkdir "$BaseDir"/doc/html/NC ; fi
if [ ! -d "$BaseDir/tmp" ] ; then mkdir "$BaseDir/tmp" ; fi

cd "$BaseDir/tmp"

for DocFile in $BaseDir/bin/*.pl ; do
  if [ -f "$DocFile" ] ; then
    title="`basename "$DocFile" .pl`"
    /usr/bin/perldoc -u -F "$DocFile" | /usr/bin/pod2html --title="$title" >"$BaseDir/doc/html/main_pl/$title.html"
  else
    echo "No file:$DocFile."
  fi
done

for DocFile in $BaseDir/lib/perl/NC/*.pm ; do
  if [ -f "$DocFile" ] ; then
    title="`basename "$DocFile" .pm`"
    /usr/bin/perldoc -u -F "$DocFile" | /usr/bin/pod2html --title="NC::$title" >"$BaseDir/doc/html/NC/$title.html"
  else
    echo "No file:$DocFile."
  fi
done
