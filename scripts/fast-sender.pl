#!/usr/bin/perl -w

use Data::Dumper;

use UniLang::Util::TempAgent;

my $agent = UniLang::Util::TempAgent->new;

foreach my $i (1..10) {
  $agent->Send
    (Receiver => "UniLang",
     Contents => "UniLang, echo $i",
     Data => {_DoNotLog => 1});
}
