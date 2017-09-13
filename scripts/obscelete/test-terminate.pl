#!/usr/bin/perl -w

use UniLang::Util::TempAgent;
use UniLang::Util::TestHarness;

use Data::Dumper;
use FileHandle;

my $tempagent = UniLang::Util::TempAgent->new();

print Dumper($tempagent->Send
  (
   Contents => "UniLang, echo UniLang has been told to quit now",
  ));
