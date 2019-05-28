#!/usr/bin/env perl
# Comment

# Lines are usually ended with ;
use warnings;

# Don't allow use of undefined and undeclared variables
use strict;


# Scalar variables are referenced with $
my $a=1;
print "Scalar a is $a\n";

# Array variable
my @b=("a", 1, "b");
print "Array b is @b\n";

# Hash (associative array) variable
my %c=(
    "name", "Sorel",
    "surname", "Mitra");
print "Hash c is %c - '%' is not a special char for print\n";
print "Hash c is ".%c." - second attempt using concatenation\n";
print "So you cannot use '%c' in scalar context\n";
foreach my $key (keys %c) {
    print "$key is $c{$key}\n";
}
print "Same hash with '=>' instead of commas\n";
%c=(
    name => "Sorel",
    surname => "Mitra");
foreach my $key (keys %c) {
    print "$key is $c{$key}\n";
}

print "------------------\n\n";


# Function declaration
sub f {
    # Parameters of a subroutine are a specially named array
    my ($x, $y)=@_;
    print "Inside f()\n";
    print "x=";
    print defined $x? $x: "UNDEF";
    print "\n";
    print "y=";
    print defined $y? $y: "UNDEF";
    print "\n";
    if (!defined($y)) {
        print "If you need to use 'defined' while concatenating a string, then ".
            "put paranthesis\n";
        print "y=".(defined $y? $y: "UNDEF")."\n";
    }
    print "\n";
}

print "You can call a function with a different nr. of arguments than it expects\n";
print "Check for variable being defined with 'defined'\n";
f();

f(1, "abc");

print "-------------------\n\n";


print "Statement modifiers\n";
$a=1;
print "Incorrect\n" if ($a==2);
print "Correct\n" unless ($a==2);

print "Alternative to if\n";
($a==2) or print "Expecting 2\n";

print "-------------------\n\n";


print "Loop control\n";
my @ary1=(5,3,8);
my @ary2=(7,1,6);

# Perl stupidity:
# If I put 'my' before $wid in the OUTER for, then $wid is not visible outside 
# the for block. If I do this:
# my $wid=0;
# before the OUTER for, I see $wid 0 at the end.
# So I need to use a second variable 'result'.
my $result=0;
OUTER: for my $wid (@ary1) {
    print "OUTER wid=$wid\n";
    INNER: for my $jet (@ary2) {
        print "INNER jet=$jet\n";
        next OUTER if $wid > $jet;
        $wid += $jet;
        $result=$wid;
    }
}
# print "wid=$wid\n";
print "result=$result\n";

print "-------------------\n\n";


print "
References: holding references to lists as values in a hash 
";
my @list=(
    "Chicago, USA",
    "Frankfurt, Germany",
    "Berlin, Germany",
    "Washington, USA",
    "Helsinki, Finland",
    "New York, USA",
);
my %table;
for (@list) {
    chomp;
    my ($city, $country) = split /, /;
    $table{$country} = [] unless exists $table{$country};
    push @{$table{$country}}, $city;
}
foreach my $country (sort keys %table) {
    print "$country: ";
    my @cities = @{$table{$country}};
    print join ', ', sort @cities;
    print ".\n";
}


# General processing loop for text

my $in=join "", <>;
my $out = "";
my $p1 = 0;
my $p2 = 0;
my $p3 = length($in);

my $a4 = -1;

pos($in) = 0;
while ($in =~ /((\#g)(\d+)( seal )(\d+))/gc) {
  $p3 = pos($in);
  $p2 = $p3 - length($1);

  my $a1 = $2;
  my $a2 = 1;
  my $a3 = $4;
  $a4++;
  $out .= substr($in, $p1, $p2 - $p1);
  $out .= "$a1$a2$a3$a4";

  $p1 = $p3;
}
$out .= substr($in, $p3, length($in) - $p3);

print $out;



