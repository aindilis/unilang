#!/usr/bin/perl -w

use Data::Dumper;

use UniLang::Util::TempAgent;

my $agent = UniLang::Util::TempAgent->new;

while ($i = <>) {
  chomp $i;
  # send this, no logging, to formalize
  print Dumper
    ($agent->MyAgent->QueryAgent
     (Agent => "Formalize",
      Contents => $i));
}
