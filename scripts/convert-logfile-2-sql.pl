#!/usr/bin/perl -w

use UniLang::Util::MySQL;

my $mysql = UniLang::Util::MySQL->new;

$mysql->LoadLogFileIntoDB
  (LogFile => $ARGV[0]);
