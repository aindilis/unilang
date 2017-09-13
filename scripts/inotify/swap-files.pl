#!/usr/bin/perl -w

use PerlLib::SwissArmyKnife;

my $unilang = "/var/lib/myfrdcsa/codebases/internal/unilang/unilang";
my $chased = `chase $unilang`;
chomp $chased;
my $basename = basename($chased);
if ($basename eq "unilang1") {
  system "rm $unilang";
  system "ln -s /var/lib/myfrdcsa/codebases/internal/unilang/unilang2 $unilang";
  # now do the same with the UniLang::Agent::Agent file

  system "rm /var/lib/myfrdcsa/codebases/internal/unilang/UniLang/Agent/Agent.pm";
  system "ln -s /var/lib/myfrdcsa/codebases/internal/unilang/UniLang/Agent/Agent2.pm /var/lib/myfrdcsa/codebases/internal/unilang/UniLang/Agent/Agent.pm";
  # now do the same with the UniLang::Agent::Agent file
  print "Now UniLang2\n";
} elsif ($basename eq "unilang2") {
  system "rm $unilang";
  system "ln -s /var/lib/myfrdcsa/codebases/internal/unilang/unilang1 $unilang";

  system "rm /var/lib/myfrdcsa/codebases/internal/unilang/UniLang/Agent/Agent.pm";
  system "ln -s /var/lib/myfrdcsa/codebases/internal/unilang/UniLang/Agent/Agent1.pm /var/lib/myfrdcsa/codebases/internal/unilang/UniLang/Agent/Agent.pm";
  # now do the same with the UniLang::Agent::Agent file
  print "Now UniLang1\n";
}

