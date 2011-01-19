
=head1 Name

Reimplementation of the ConfigReader package.

This is a (possibly less efficient) reimplementation of the ConfigReader interface to keep
some open source code intact.

This implementation should be enhanced to read the more complex configuration files the Lisp
and Haskell implementations can handle. Configuration files are "compatible to some degree".
This version will just ignore any section names and put anything into the flat hash.

=head1 License

Copyright (c): Thorsten Rangwich.

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

=cut

package NC::ConfigReader;
use strict;
use vars;
use warnings;


my %config = (); #The global hash carrying the configuration (if global configuration is used).

=head1 VARIABLES

=head2 configParsed

Indicator if any configuration file was read already. In fact it counts how often
a configuration was read. If the configuration read was not stored globally but
returned to the caller (readConfigFile in list context), the counter is not changed.

=cut

our $configParsed = 0;

=head1 FUNCTIONS

=head2 _parseLine(<line>)

Internal function returning an array of (key, value) of the parsed line or
an empty array (if line did not fit syntax - which includes comments and
section names). This is used via map to read in the configuration.

No prototype here, otherwise map blames about missing parameter.
The function reads $_ -- I do not know if this really is a parameter
or is externally set by map.

=cut

sub _parseLine
{
    #Only lines starting with comment sign are necessary to ignore.
    #The following match restricts that a bit more. Doesn't harm.
    #FIXME: Do not ignore ' and " but put result in a number instead if not
    #by using +. This would return the proper data type.

    #print "Parse line:$_";
    my @splitted = ( $_ =~ /^([a-zA-Z][_0-9a-zA-Z]*)[=]['"]?(.*)['"]?$/ );
    if (2 == @splitted) {
	#print "Match:$splitted[0],$splitted[1]\n";
	return ($splitted[0], $splitted[1]);
    } else {
	#print "No match. Dump splitted:\n";
	#print @splitted;
	#print "\nEnd of dump\n";
	return ();
    }
}

=head2 readConfigFile(<FileName>)

Reads in a Java properties style configuration file. Accepts a string file name
as input. In list context, returns a hash with the configuration ("properties").
This is useful if modules wish to read their own configuration file. It reads
Windows ini files by ignoring the sections. This of course does not work
properly.

Main usage will be to read in a global configuration file. This is done if scalar
or void context (latter one is not a good idea) is chosen. The result is stored in
a package variable to be retrieved using getVariables. It returns undef on error
and 1 in scalar context and the read in hash in list context.

=cut

sub readConfigFile($)
{
    my $cfgname = shift;
    my $isOpen = 0;

    #FIXME: This fails on filenames with a space in the name.
    open my $fd, "$cfgname" and ($isOpen = 1);
    if (!$isOpen) {
	print "Error opening file:$cfgname.\n";
	return undef;
    }
    my @lines = <$fd>;
    close $fd or return undef;

    #FIXME: What is the correct notation to pass a function pointer without letting Perl guess?
    my %cfg = map(_parseLine, @lines);
    #print "parsed, dump:\n";
    #print %cfg;
    #print $cfg{"xyz"};
    print "\n";
    if (wantarray()) {
	return %cfg;
    } else {
	print "ConfigReader: Configuration is re-read from file $cfgname\n" if ($configParsed);
	#FIXME: should catch when this integer overflows...
	$configParsed += 1;
	%config = %cfg;
	return 1;
    }
}

=head2 getVariables([key])

If a key is given, the value for the given key is returned (undef if it does not exist).
If no key is given, the whole has is returned.


If the hash was not set / read in up to now, undef is returned.

As parameters are optional, no prototype is defined (or I don't know how to do properly
without Perl complaining).

=cut

=for comment

I thought about deriving the result from the value expected from the caller,
i.e. returning the hash in list context and a variable in scalar context.
This is not a good idea. What do we do in scalar context when no parameter
is passed and what do we do in functions usually expecting list context
(like print) where obviuosly not the whole hash is meant and a parameter
is passed to get the particular value. This would make the interface too
complex for the caller.

=cut

sub getVariables(;$) {
    my $key = shift;
    if ($configParsed) {
	if ($key) {
	    #check if defined, otherwise Perl will create the entry!
	    if (defined($config{$key})) {
		return $config{$key};
	    } else {
		return undef;
	    }
	} else {
	    return %config;
	}
    } else {
	return undef;
    }
}

1;

__END__

=head1 Author

Thorsten Rangwich

=cut
