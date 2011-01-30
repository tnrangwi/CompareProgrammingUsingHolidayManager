
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

=head1 Name

Package NC::ServerSocket. Easy perl server socket connection interface.
Only usefull in single-threaded, non forked applications.
NC::ServerSocket provides a connection object that can be used to wait
for incoming connections and provides methods to send respones to
the client.

=cut

package NC::ServerSocket;
use warnings;
use Socket;
use strict;
use vars;
use FileHandle;
use NC::Log qw (logPrint logCheckInit);

my $empty = 0;

=head1 Methods

=cut


=head2 new($ ; \%);

Initialises socket connection.

=head3 Return Value

ServerSocket Object.

=head3 Parameters:

=over

=item I< >
$ Port number to open socket.

=item I< >
% Optional config hash (reserved for future extensions).

=back

=cut


sub new($;\%)
  {
    my $classname = shift;
    my %ref;
    my $time=localtime();
    $ref{port} = shift;
    $ref{config} = shift;
    unless (logCheckInit())
      {
	print STDERR "Log interface not initialised. Won't start\n";
	return undef;
      }
    if(!defined($ref{port}) || $ref{port} eq "")
      {
        logPrint("FATAL","No port.");
        return undef;
      }
    if (ref($ref{config}) eq "HASH")
    {
      logPrint("DEBUG", "config given");
      for my $k (keys(%{$ref{config}}))
      {
        SWITCH:
	{
	  if ($k eq "privileged")
	  {
	    for my $p (split / /, $ref{config}->{$k})
	    {
	      $ref{privileged}->{$p} = 1;
	    }
	    last;
	  }
	  else
	  {
	    logPrint("WARNING", "Unrecognised config file keyword:$k.");
	    last;
	  }
	}
      }
    }
    $ref{proto} = getprotobyname 'tcp';

    unless(socket $ref{server}, PF_INET, SOCK_STREAM, $ref{proto})
      {
        logPrint("FATAL","Could not open socket $!.\n");
        return undef;
      }
    unless(setsockopt $ref{server}, SOL_SOCKET, SO_REUSEADDR, 1)
      {
        logPrint("FATAL","Could not set socket options $!.\n");
        return undef;
      }
    unless(bind $ref{server}, sockaddr_in($ref{port}, INADDR_ANY))
      {
        logPrint("FATAL","Could not bind to socket. $!\n");
        return undef;
      }
    unless(listen $ref{server}, 1)
      {
        logPrint("FATAL","Could not start to listen.$\n");
        return undef;
      }
    $ref{server}->autoflush(1);
    logPrint("INFO","$time: Started Server on port $ref{port}\n");
    bless \%ref;
  }

=head2 waitConnection(Z<>)

Waits for next connect, returns the line and sets status to server answer.
Only one line must be provided (and only one line will be returned).
printLine() can be used after that to send response data,
closeConnection() to close connection.

=head3 Return Value

Line sent by client or undef if connection was terminated.

=cut

sub waitConnection()
  {
    my $ref = shift;
    return undef unless ref($ref);
    return undef if(defined($ref->{client}));
    $ref->{addr} = accept $ref->{client}, $ref->{server};
    return undef unless(defined($ref->{addr}));
    ($ref->{inport}, $ref->{packed_ip}) = sockaddr_in($ref->{addr});
    $ref->{incoming_addr} = inet_ntoa($ref->{packed_ip});
    logPrint("LOG","Connection from $ref->{incoming_addr}.");
    if (defined($ref->{privileged}->{$ref->{incoming_addr}}))
    {
      $ref->{connStatus} = 2;
      logPrint("LOG", "privileged connection...");
    }
    else
    {
      $ref->{connStatus} = 0;
    }
#perl compiler refuses to compile <$ref->{client}>, so we use this less convenient function
#T. Rangwich, 8.10.2006
    return readline $ref->{client};
  }

=head2 printLine($)

Prints one result line back to the client. Carriage Return and NewLine will be appended
automatically and must not be included (no check done currently).
Empty lines are converted to lines terminated with Carriage Return and NewLine, empty string
is converted to one 0 character (as specified in protocol document).

=head3 Parameters

=over

=item I< >
$ Line to return to client.

=back

=head3 Return Value

Return value from client write or undef on error.

=cut

sub printLine($)
  {
    my $ref = shift;
    my $line = shift;
    return undef unless(ref($ref) and defined($line));
    return undef unless ($ref->{client});
    if ($line eq "")
      {
        print {$ref->{client}} chr($empty) . "\r\n";
      }
    else
      {
        print {$ref->{client}} $line . "\r\n";
      }
#returns and sets error vars according to last command. Keep this in mind if you change code.
  }

=head2 closeConnection(Z<>)

Closes current socket connection associated to ServerSocket object.

=head3 Return Value

Undef or 1 on error, otherwise 0.

=cut

sub closeConnection()
  {
    my $ref = shift;
    return undef unless(ref($ref));
    my $rc = 0;
    if ($ref->{client})
      {
#looks strange, but this obviously seems to be a bug/strange behaviour in perl.
#print $handle $line would be correct, but the perl compiler would not recognise the
#handle without marking them as a reference using braces.
#However, for the close function this must not be used.
        print {$ref->{client}} "\r\n" or $rc = 1;
        close $ref->{client} or $rc = 1;
        delete $ref->{client};
        return $rc;
      }
    else
      {
        return undef;
      }
  }

=head2 isPrivileged(Z<>)

Returns privileged status of current connection.

=head3 Return Value

undef on no connection / error, 0 on non privileged, 1 on privileged connections.

=cut

sub isPrivileged()
{
  my $ref = shift;
  return undef unless(defined($ref) and ref($ref) and defined $ref->{client});
  return $ref->{connStatus};
}

=head2 DESTROY

Destructor to do cleanups.

=cut

sub DESTROY()
  {
    my $ref = shift;
    return unless ref($ref);
    close $ref->{client} if(defined($ref->{client}));
  }


1;

__END__

=head1 Protocol Specification

A very simple socket protocol is used. Only one line of command input is supported
and must be terminated by C<CR LF>. No other C<LF> must occur in input. More
C<CR>'s will work, but are not recommended, though. This is read by
I<waitConnection>. 

Result lines are almost freely defined. Every data set to be returned to the
client must not contain any C<CR LF> sequence. It will be terminated by
the I<printLine()> function by C<CR LF> and appear this way at client side.
Empty data sets (empty strings) will be converted to a single 0-Byte character,
the data set terminated by C<CR LF> as usual.

To terminate a connection use I<closeConnection()>. This will send a final
C<CR LF> to the client and close the connection afterwards. The client can check
for complete data transfer, because a transfer containing nothing consists of
C<CR LF> only and all other transfers end in C<CR LF CR LF>.

Raw access functions may be implemented on the SocketServer object in the future
to get around limitations like no C<CR LF> may be included or to support binary
data transfers.

=head1 Author

Thorsten Rangwich, Nagler & Company.

based on examples provided by Asmus LE<ouml>wenhaupt, Nagler & Company.

=cut

