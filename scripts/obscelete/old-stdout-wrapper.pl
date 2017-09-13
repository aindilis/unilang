#!/usr/bin/perl -w

local $| = 1;

# intercept STDOUT and print a prefix

use Data::Dumper;
use IO::Select;
use IO::Handle;

$s = IO::Select->new();

$io = new IO::Handle;
$io->fdopen(fileno(STDIN),"r");
$s->add($io);

my $prefix = shift;
while (1) {
  @ready = $s->can_read(5);
  foreach my $it (@ready) {
    my $text;
    $it->read($text,1000000);
    if ($text) {
      $text =~ s/^/$prefix/mg;
      print $text;
    }
  }
}
