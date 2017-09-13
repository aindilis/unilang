#!/usr/bin/perl -w

use KBS::Util;
use PerlLib::Util;

use Data::Dumper;

sub Convert {
  my (%args) = @_;
  # print Dumper(%args);
  my @list;
  if (exists $args{DATA}) {
    my $ref = ref $args{DATA};
    if ($ref eq "ARRAY") {
      my @list;
      foreach my $arg (@{$args{DATA}}) {
	push @list, Convert(DATA => $arg);
      }
      return "(".join(" ",@list).")";
    } elsif ($ref eq "HASH") {
      my @list;
      foreach my $key (keys %{$args{DATA}}) {
	push @list, "(".Convert(DATA => $key)." . ".Convert(DATA => $args{DATA}->{$key}).")";
      }
      return "(".join(" ",@list).")";
    } elsif ($ref eq "GLOB") {
      if (DumperQuote($args{DATA}) =~ /^\\\*\{\'::(.+)\'\}$/) {
	return $1;
      }
    } else {
      $Data::Dumper::Useqq = 1;
      my $thing = Dumper($args{DATA});
      $Data::Dumper::Useqq = 0;
      chomp $thing;
      if ($thing =~ /^\$VAR1 = (\[\s+)?(\"(.*)\")(\s+\])?;$/sm) {
	return '"'.$3.'"';
      } elsif ($thing =~ /^\$VAR1 = (.+);$/) {
	return '"'.$1.'"';
      }
    }
  }
}

print Convert(DATA => DeDumper((shift @ARGV)));
