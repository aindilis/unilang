#!/usr/bin/perl -w

use UniLang::Util::TempAgent;
use UniLang::Util::TestHarness;

use Data::Dumper;
use FileHandle;

my $testharness = UniLang::Util::TestHarness->new;
$testharness->StartTemporaryUniLangInstance;

my $tempagent = UniLang::Util::TempAgent->new
  (
   Host => "localhost",
   Port => "9010",
  );

while (1) {
  print "sending...\n";
  if (0) {
    print Dumper
      ($tempagent->MyAgent->QueryAgent
       (
	Sender => $tempagent->Name,
	Receiver => "Echo",
	Contents => "Hi there",
	Data => {
		 _DoNotLog => 1,
		},
       ));
  }
  print Dumper
    ($tempagent->Send
     (
      Contents => "quit",
     ));
}
