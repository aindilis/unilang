#!/usr/bin/perl -w

use KBS2::Util;
# use PerlLib::SwissArmyKnife;

use File::Slurp;

my $fn = shift;
my $c = read_file($fn);

my $result = PerlDataDedumperToStringEmacs
  (String => $c);

# my $io = IO::File->new();
# $io->open(">/tmp/tmp");
# print $io Dumper($result);
# $io->close();

if (defined $result and $result ne "\"undef\"") {
  print $result;
} else {
  print "()";
}
