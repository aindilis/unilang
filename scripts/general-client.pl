#!/usr/bin/perl -w

use Data::Dumper;

use UniLang::Util::TempAgent;

my $agent = UniLang::Util::TempAgent->new;

while ($i = <>) {
  chomp $i;
  if ($i =~ /^(\S+),\s*(.+)$/) {
    # send this, no logging, to formalize
    $agent->Send
      (Receiver => $1,
       Contents => $2,
       Data => {_DoNotLog => 1});
  }
}
