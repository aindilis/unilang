#!/usr/bin/perl -w

use BOSS::Config;
use KBS2::Util;
use PerlLib::SwissArmyKnife;
use UniLang::Util::Message;

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

my $message = UniLang::Util::Message->new
  (Raw => $raw);

$raw =~ s/^\s*//s;
$raw =~ s/\s*$//s;

my $id = PerlDataStructureToStringEmacs(DataStructure => $message->ID);
my $sender = PerlDataStructureToStringEmacs(DataStructure => $message->Sender);
my $receiver = PerlDataStructureToStringEmacs(DataStructure => $message->Receiver);
my $date = PerlDataStructureToStringEmacs(DataStructure => $message->Date);
my $contents = PerlDataStructureToStringEmacs(DataStructure => $message->Contents);
my $data = PerlDataStructureToStringEmacs(DataStructure => '$VAR1 = '.DumperQuote2($message->Data));

# my $fh = IO::File->new();
# $fh->open(">/tmp/1.txt");
# print $fh "hi\n";
# print $fh $data;
# $fh->close();

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
print $output;
