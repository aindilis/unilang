#!/usr/bin/perl -w

use Data::Dumper;

use UniLang::Util::Message;
use UniLang::Util::TempAgent;

my $agent = UniLang::Util::TempAgent->new;

my $res = $agent->MyAgent->QueryAgent
    (Receiver => "Echo",
     Contents => "hi",
     Data => {_DoNotLog => 1});

if (ref($res) eq 'UniLang::Util::Message' and
    $res->Sender eq 'Echo' and
    $res->Contents eq 'hi') {
  exit(0);
} else {
  exit(1);
}


