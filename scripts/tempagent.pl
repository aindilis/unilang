#!/usr/bin/perl -w

use Data::Dumper;

use UniLang::Util::TempAgent;

my $agent = UniLang::Util::TempAgent->new;

while ($i = <>) {
  chomp $i;
  # send this, no logging, to formalize
  $agent->Send
    (Receiver => "Echo",
     Contents => "$i",
     Data => {_DoNotLog => 1});
}
