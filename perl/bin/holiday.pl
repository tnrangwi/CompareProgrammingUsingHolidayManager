#!/usr/bin/perl

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

use warnings;
use strict;
use vars;
use FileHandle;
use NC::Log qw(logInit logPrint);
use NC::ServerSocket;
use NC::ConfigReader;

my %htable = ();

$htable{'gast'} = { group => 'N' , times => { 12005 => 6, 3202005 => 2 } } ;

=head1 Name

This is the holiday server. It provides a generic interface to a holiday calender for
multiple users. Communication is done via an abstract interface, currently using NC::SocketServer.
See the man page holiday for further details. Some features regarding production environments
have been removed, but the code is the same.

This is the original imlementation. Thanks to Asmus LE<ouml>wenhaupt, it had been a pleasure
to setup this and use it for teaching software development and code versioning with you.

All the implementations in different languages are derived from this one. Therefore this one
has least features, does not support command line arguments, etc.

=cut

=head1 Functions

=head2 load(Z<>)

Loads currently saved holiday calenders for all users.

=cut 

sub load()
{
  for my $u (glob "*.usr")
  {
    open UFILE, "<$u";
    $u =~ s/.usr$//;
    my $line = <UFILE>;
    chomp $line;
    $htable{$u}->{group} = $line;
    while (<UFILE>)
    {
      $line = $_;
      chomp $line;
      my @d = split /\|/, $line;
      $htable{$u}->{times}->{$d[0]} = $d[1];
    }
    close UFILE;
  }
}

=head2 save(Z<>)

Save currently held holiday calenders for all users

=cut

sub save()
{
  for my $u (keys(%htable))
  {
    open UFILE, ">$u.usr";
    print UFILE $htable{$u}->{group} . "\n";
    for my $h (keys(%{$htable{$u}->{times}}))
    {  print UFILE $h . '|' . $htable{$u}->{times}->{$h} . "\n"; }
    close UFILE;
  }
}


# MAIN STARTS

NC::ConfigReader::readConfigFile("holiday.conf");
my $port = NC::ConfigReader::getVariables("port");
my $privileged = NC::ConfigReader::getVariables("privileged");
my $logfile="logfile.log";
my $LOG=logInit($logfile,"INFO",'a');

load();

my $sock;
if (defined($privileged) and $privileged ne "")
{
  my %sock_config = ( "privileged" => "$privileged" );
  $sock = NC::ServerSocket->new($port, \%sock_config);
}
else
{
  $sock = NC::ServerSocket->new($port);
}
my $time=localtime;
logPrint("INFO","Waiting for connection.");
for(;;)
  {
    my $line = $sock->waitConnection();
    die "invalid connection:$line,$!." unless ($line);
    $line =~ s/\r//g;
    chomp $line;
    my @data=split/\|/, $line;
    SWITCH:
    {
      if ($data[0] eq 'test')
      {
        $sock->printLine("response");
	$sock->printLine($line);
	last;
      }

      if ($data[0] eq 'addh')
      {
        if (not defined($htable{$data[1]}))
	{
	  $sock->printLine("User '$data[1]' does not exist, you are not allowed to do this.")
	}
	else
	{
          $htable{$data[1]}->{times}->{$data[2]} = $data[3];
	  logPrint "LOG", "Created/changed holiday: U:$data[1],S:$data[2],C:$data[3]";
	  $sock->printLine("");
	}
	last;
      }
      if ($data[0] eq 'delh')
      {
        logPrint "LOG", "Deleted holiday: U:$data[1],S:$data[2]";
        delete $htable{$data[1]}->{times}->{$data[2]};
	$sock->printLine("") ;
	last;
      }
      if ($data[0] eq 'getu')
      {
	my $href = $htable{$data[1]};
	if (defined($href) and defined($href->{times}))
	{
	  $href = $href->{times};
	  for my $h (keys(%{$href})) { $sock->printLine($h . '|' . $href->{$h}) ; }
	}
	last;
      }
      if ($data[0] eq 'geta')
      {
	for my $u (keys(%htable))
	{
	  my $href;
	  if (defined($htable{$u}->{times}))
	  {
	    my $href = $htable{$u}->{times};
	    for my $h (keys(%{$href})) { $sock->printLine($u . '|' . $h . '|' . $href->{$h}); }
	  }
	}
	last;
      }
      if ($data[0] eq 'addu')
      {
        if ($sock->isPrivileged())
	{
          $htable{$data[1]}->{group} = $data[2];
	  $sock->printLine("");
        }
	else
	{
	  logPrint "WARNING", "getu not allowed for this connection";
	  $sock->printLine("Non privileged connection, you are not allowed to do this.");
	}
	last;
      }
      if ($data[0] eq 'shutdown') { save(); $sock->printLine('done'); $sock->closeConnection(); exit 0; }
      $sock->printLine("unknown command:$data[0]");
    }
    $sock->closeConnection();
  }

__END__

=head1 Interface specification

Currently a socket interface at the port chosen in the config file is used to access the holiday server.
Communication is done by providing a line in this format:

C<Command|Arg1|Arg2>

terminated with C<CR LF>. Result data sets are provided in the format

C<Par1|Par2|...>

Results vary from zero to many lines of output. See the specification of NC::ServerSocket for technical
protocol details.

=head2 getu

Returns holiday calender for the given user.

=head3 Parameters

=over

=item *
User name.

=back

=head3 Return Lines

=over

=item *
Day of year (leading digits), year (trailing 4 digits).

=item *
Number of Days.

=back

=head2 geta

Returns holiday calender for all users. See C<getu> for details. This adds
a first return parameter containing the user name.

=head2 addh

Add/change holiday for a given user with a given first holiday date.

=head3 Parameters

=over

=item *
User name.

=item *
Day of year (leading digits), year (trailing 4 digits).

=item *
Number of days.

=back

=head3 Return Lines

Returns one return line containing either an empty string or the error message.

=head2 delh

Delete holiday for a given user with a given first holiday date.

=head3 Parameters

=over

=item *
User Name

=item *
Day of year (leading digits), year (trailing 4 digits).

=back

=head3 Return Lines

Returns one return line containing either an empty string or the error message.

=head2 shutdown

Requests holiday server to terminate, saving all holidays.

=head3 Return Lines

Returns one line either containing an error message or C<Done>.

=cut

=head2 addu

Add user with the given group to the system. This is only allowed via
privileged connections.

=head3 Parameters

=over

=item *
User name.

=item *
Group.

=back

=head3 Return Lines

Returns one return line containing either an empty string or the error message.

=head1 Author

Thorsten Rangwich (written for Nagler & Company, modified afterwards).

=head1 License

See file LICENSE for details.

=cut
