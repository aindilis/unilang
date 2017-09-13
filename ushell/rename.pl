#!/usr/bin/perl -w

while (<>) {
  chomp;
  if (/^e/) {
    ($t = $_) =~ s/^e/u/;
    system "mv $_ $t\n";
  }
}
