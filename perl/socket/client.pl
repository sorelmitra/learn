#!/usr/bin/env perl

use strict;
use warnings;

use Socket;

my $sock;
my $name = "client-" . getppid();

sub sendMsg {
    my ($data) = @_;
    
    my $ret;
    my $len = length $data;
    
    $ret = syswrite($sock, $data);
    if (!defined($ret)) {
        print "ERROR writing to server " . fileno($sock) . ": $!\n";
        return -1;
    }
    if ($ret != $len) {
        print "WARNING: wrote only $ret of $len bytes!\n";
        return -2;
    }
    
    print "Sent '$data' to server.\n";
    return 0;
}

my @proto = getprotobyname("tcp");
# print "$proto[2]\n";

socket($sock, PF_INET, SOCK_STREAM, $proto[2])
    or die "cannot open $proto[1] socket: $!";
print "Client socket is " . fileno($sock) . "\n";

my $serverPort = 2323;
my $iaddr = inet_aton("localhost");
my $serverSockAddr = sockaddr_in($serverPort, $iaddr);
connect($sock, $serverSockAddr)
    or die "cannot connect to server port $serverPort: $!";

my $ret;

$ret = sendMsg "Hello from $name";
sleep 1;
$ret = sendMsg "I said 'hello', how do you do?";

close($sock);
exit $ret;
