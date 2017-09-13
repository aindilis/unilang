#!/usr/bin/perl -w

use BOSS::Config;
use KBS2::Util;
use PerlLib::SwissArmyKnife;
use UniLang::Util::Messages;

$specification = q(
	-n		Normal mode
	-q		Query agent mode

	-f <file>	The file containing the message
);

my $config =
  BOSS::Config->new
  (Spec => $specification);
my $conf = $config->CLIConfig;
# $UNIVERSAL::systemdir = "/var/lib/myfrdcsa/codebases/minor/system";

my $raw = read_file($conf->{'-f'});
$raw =~ s/^.*?<message>/<message>/s;
my $messages = UniLang::Util::Messages->new
  (Raw => $raw);

$raw =~ s/^\s*//s;
$raw =~ s/\s*$//s;

my @totalOutput;
foreach my $message (@{$messages->Messages}) {
  my $id = PerlDataStructureToStringEmacs(DataStructure => $message->ID);
  my $sender = PerlDataStructureToStringEmacs(DataStructure => $message->Sender);
  my $receiver = PerlDataStructureToStringEmacs(DataStructure => $message->Receiver);
  my $date = PerlDataStructureToStringEmacs(DataStructure => $message->Date);
  my $contents = PerlDataStructureToStringEmacs(DataStructure => $message->Contents);
  my $data = PerlDataStructureToStringEmacs(DataStructure => Dumper($message->Data));

  # # parse this message and convert to an emacs data structure

  my $output;
  if (exists $conf->{'-n'}) {
    $output = "(let
 (
  (id $id)
  (sender $sender)
  (receiver $receiver)
  (date $date)
  (contents $contents)
  (data $data)
 )
 (setq uea-output-kept nil)
 (uea-process-contents id sender receiver date contents)
)";
  } elsif (exists $conf->{'-q'}) {
    $output = "(let
 (
  (id $id)
  (sender $sender)
  (receiver $receiver)
  (date $date)
  (contents $contents)
  (data $data)
 )
 (setq uea-output-kept nil)
 ;; (uea-process-contents id sender receiver date contents)
 (setq uea-buffer-message-raw (list id sender receiver date contents data))
 (setq uea-buffer-message contents)
)";
  }
  push @totalOutput, $output;
}
print join("\n",@totalOutput);
