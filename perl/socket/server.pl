#!/usr/bin/env perl

use warnings;
use strict;

use Socket;

my $listenSock;
my @clients;

sub newClient {
    my $newSock;
    my $newSockAddr = accept($newSock, $listenSock);
    if (!$newSockAddr) {
        print "Cannot accept new client: $!\n";
        next;
    }
    print "New client, " . fileno($newSock) . "\n";
    return $newSock;
}

sub fhbits {
    my @fhlist = @_;
    my $bits = "";
    for my $fh (@fhlist) {
        # print "fhbits: " . fileno($fh) . "\n";
        vec($bits, fileno($fh), 1) = 1;
    }
    return $bits;
}

sub processClient {
    my ($fh) = @_;
    my $buff = "";
    my $ret;
    
    $ret = sysread($fh, $buff, 1024);
    if (!defined($ret)) {
        print "ERROR reading from client " . fileno($fh) . ": $!\n";
        return -1;
    }
    if ($ret == 0) {
        print "Client " . fileno($fh) . " has left.\n";
        return -2;
    }
    
    print "Client " . fileno($fh) . " has sent: '$buff'\n";
    if ($buff =~ /^\s*hello/i) {
        print "    - this is the 'hello' from this client\n";
    }
}

sub processClients {
    my ($bits, @fhlist) = @_;
    for (my $i = 0; $i < @fhlist; $i++) {
        my $fh = $fhlist[$i];
        
        if (vec($bits, fileno($fh), 1) != 1) {
            next;
        }
        
        my $ret = processClient $fh;
        if ($ret < 0) {
            splice(@clients, $i, 1);
            print "Removed client ". fileno($fh) . "\n";
            close($fh);
        }
    }
}

sub checkClients {
    my $nfound;
    my $rin;
    
    $rin = fhbits(@clients);
    vec($rin, fileno($listenSock), 1) = 1;
    $nfound = select($rin, undef, undef, 10);
    
    if ($nfound > 0) {
        if (vec($rin, fileno($listenSock), 1) == 1) {
            push @clients, newClient;
        }
        
        processClients($rin, @clients);
    }
}

my @proto = getprotobyname("tcp");
# print "$proto[2]\n";

socket($listenSock, PF_INET, SOCK_STREAM, $proto[2])
    or die "cannot open $proto[1] socket: $!";
print "Server listen socket is " . fileno($listenSock) . "\n";

my $port = 2323;
my $sockAddr = pack('Sn4x8', AF_INET, $port, 0);

bind($listenSock, $sockAddr) or die "cannot bind to port $port: $!";

listen($listenSock, 10)
    or die "cannot listen on " . fileno $listenSock . ": $!";
    
print "Listening for incoming connection requests...\n";

while (1) {
    checkClients;
}

close($listenSock);

