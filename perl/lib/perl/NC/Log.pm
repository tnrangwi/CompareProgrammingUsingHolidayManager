
=head1 License

Copyright (c) 2006-2010: Nagler & Company

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

package NC::Log;
require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw (logInit logPrint logCheckInit);
use warnings;
use strict;
use vars;
use FileHandle;

=head1 Name

NC::Log is a global log utility. It provides a demonstration interface to a logger.
There are much more powerful logging utilities. This one restricts to demonstration
purposes and simplicity.

=head1 Usage

Use C<logInit()> for initialisation and afterwards C<logPrint()> for output. Output is
global, so no handle has to be remembered by the caller. If output cannot be processed,
the functions do an explicit die, so this has to be catched above if the caller
wished to continue in this cases - which is unlikely.

=cut

my $fd;
my $log_level;
my %levels = (DEBUG => 0, INFO => 1, LOG => 2, WARNING => 3, ERROR => 4, SEVERE => 5, CRITICAL => 6, FATAL => 7);

=head1 Functions

=cut

=head2 logInit($ $ $)

Initialise log interface and open logfile.

=head3 Parameters

=over

=item
$ Full path to logfile.

=item
$ Log level to be used for output.

=item
$ Log mode.

=back

Everything below the log level won't be printed out. See Log Levels below.
Use 'a' for Log mode append, everything else will truncate the log file.

=cut

sub logInit ($ $ $)
  {
    return undef if(logCheckInit());
    my $time = localtime();
    my ($LOG,$level,$modus) = @_;
    if(!defined($LOG))
      {
	print STDERR "No Log-File-Name transmitted.\n";
	return undef;
      }
    if(!defined($level))
      {
	print STDERR "Log Level not given. Using default 1.\n";
	$log_level = 1;
      }
    elsif($level =~ /^[A-Z]/)
      {
	if(defined($levels{$level}))
	  {
	    $log_level = $levels{$level};
	  }
	else
	  {
	    print STDERR "Log Level name invalid. Using default 1.\n";
	    $log_level = 1;
	  }
      }
    else
      {
	$log_level = $level;
      }
    if($modus eq 'a')
      {
	open $fd, ">>$LOG" or die("Could not open Log file");
      }
    else
      {
	open $fd, ">$LOG" or die("Could not open Log file");
      }
    $fd->autoflush(1);
    print $fd "----INFO----\t$time\tStarted logging with level $level.\n";
    return 1;
  }

=head2 logPrint($ $)

Processes a line of output.

=head3 Parameters

=over

=item I< >
$ Log level for this message.

=item I< >
$ Log message.

=back

=cut

sub logPrint
  {
    my $time = localtime();
    my ($level, $message) = @_;
    
    if(not logCheckInit())
      {
	print STDERR "Log-File-Handle not set.\n";
	return undef;
      }
    unless(defined($level))
      {
	logPrint("WARNING", "No log level for message given.");
      }
    unless(exists($levels{$level}))
      {
	logPrint("WARNING", "Unrecognized log level $level for Message $message.") or return undef;
	return;
      }
    unless(defined($message))
      {
	logPrint("WARNING", "No message given.");
	return;
      }
    unless($message =~ /\n$/)
      {
	$message .= "\n";
      }
    if($levels{$level} < $log_level)
      {
	return;
      }
    print $fd "----$level----\t$time\t$message" or die "cannot log";
  }

=head2 logCheckInit(Z<>)

Checks if logger was successfully initialised before. This is useful if modules
wish to do use the logger but don't know anything about the log file.

=cut

sub logCheckInit()
  {
    if (defined($fd) and ref($fd) eq "GLOB")
      {
	return 1;
      }
    else
      {
	return undef;
      }
  }

=head2 logEnd(Z<>)

Ends log output. Useful if you want to re-initialise the log e.g. in a 24h service
that wishes to open a new log on certain times.

=cut

sub logEnd()
  {
    close($fd);
    $fd = undef;
  }

1;

__END__

=head1 Log Levels

Currently default level is LOG (will be used if given level is not defined). The defined log levels
are in ascending order:

=over

=item *
INFO 


=item *
LOG 

=item *
WARNING 

=item *
ERROR 

=item *
SEVERE

=item *
CRITICAL

=item *
FATAL

=back

=head1 Bugs

Currently seperate log files are not supported. It is assumed C<logInit()> is called
exactly once and you cannot have seperate log instances with different log levels
for logging.

However, if C<logInit()> is called again, logging will continue and the existing
log interface is used. You can use this to add log output to modules even if
you don't know if the interface was correctly initialised by just initialising
it again in your module. You will find the log in the global log (if given)
or in your locally new defined log otherwise.

=head1 AUTHOR

Asmus LE<ouml>wenhaupt, Nagler & Company.


=cut

