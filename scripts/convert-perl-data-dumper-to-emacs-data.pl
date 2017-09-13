#!/usr/bin/perl -w

use KBS::Util;
# use PerlLib::SwissArmyKnife;

my $item = shift;

my $result = PerlDataDedumperToStringEmacs
  (String => $item);

# my $io = IO::File->new();
# $io->open(">/tmp/tmp");
# print $io Dumper($result);
# $io->close();

if (defined $result and $result ne "\"undef\"") {
  print $result;
} else {
  print "()";
}
