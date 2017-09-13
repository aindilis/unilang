#!/usr/bin/perl -w

use INotifyClient;

use Time::HiRes qw(usleep);

my $client = INotifyClient->new;
$client->Register;

foreach my $i (1..10) {
  $client->Send
    (
     Receiver => "INotifyUniLang",
     Contents => "Hi $i",
    );
  usleep(500);
}

$client->QueryAgent
  (
   Receiver => "INotifyUniLang",
   Contents => "Hi $i",
  );

