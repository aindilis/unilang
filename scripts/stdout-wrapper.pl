#!/usr/bin/perl -w

# intercept STDOUT and print a prefix

# the problem with this is it is always one line behind

use IO::Handle;

$io = new IO::Handle;
$io->fdopen(fileno(STDIN),"r");

my $prefix = shift;

my $fire = 0;
while (1) {
  my $char;
  $io->read($char,1);
  if ($fire) {
    print $prefix;
    $fire = 0;
  }
  print $char;
  if ($char eq "\n") {
    $fire = 1;
  }
}

