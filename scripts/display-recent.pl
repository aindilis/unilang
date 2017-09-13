#!/usr/bin/perl -w

# this displays the last several unilang-client entries

use PerlLib::MySQL;

use Data::Dumper;

my $number = $ARGV[0] || 50;
my $mysql = PerlLib::MySQL->new
  (DBName => "unilang");
my $id = $mysql->InsertID(Table => "messages");
if ($id) {
  print $id."\n";
  my $ret = $mysql->Do
    (Statement => "select *,UNIX_TIMESTAMP(Date) from messages where ID > ".($id - $number));
  foreach my $k1 (sort {$ret->{$a}->{'UNIX_TIMESTAMP(Date)'} <=>
			  $ret->{$b}->{'UNIX_TIMESTAMP(Date)'}} keys %$ret) {
    if (0) {
      print Dumper($ret->{$k1});
    } else {
      print "$k1\t".$ret->{$k1}->{Contents}."\n";
    }
  }
}
