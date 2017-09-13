#!/usr/bin/perl -w
use strict;

{
  my $conf = UniLang::Conf->new;
  sub UNIVERSAL::get_conf {
    $conf;
  }
}

my $chat = UniLang::Chat->new;
$chat->Conversation;

################################################################################

package UniLang::Conf;
use strict;
use Carp;
use Config::General;
use Getopt::Mixed;

use vars qw($VERSION);

$VERSION = '1.00';

sub new {
  my $class = shift;
  my $self = bless {}, $class;
  my $readable = sub { return $_[0] if -r $_[0] };
  $self->{rcfile} = shift ||
    &$readable("/etc/unilang/unilang.conf") ||
      &$readable("/home/jasayne/.unilang.conf") || "";
  $self->init;
  $self;
}

sub init {
  my ( $self, %args ) = @_;
  my (%options, %config);

  # parse config file
  $self->{conf} = new Config::General($self->{rcfile});
  %config = $self->{conf}->getall;
  while ( my ( $key, $value ) = each %config ) {
    $self->{$key} = $value;
    print "<$key>\t<$value>\n";
  }

  # parse command line arguments
  Getopt::Mixed::init("logfile=s l>logfile");
  while ( my ( $option, $value ) = Getopt::Mixed::nextOption) {
    $self->{$option} = $value;
    print "<$option>\t<$value>\n";
  }
  Getopt::Mixed::cleanup;

  return $self;
}

################################################################################

package UniLang::Chat;
use vars qw/ $VERSION /;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw / parser/ ];

sub init {
  my ($self,%args) = (shift,@_);
  # open a file here
  $self->parser(UniLang::Parser->new);
}

sub Conversation {
  my ($self,%args) = (shift,@_);
  while ($self->parser->Converse) {

  }
}

################################################################################

package UniLang::Log;
use vars qw/ $VERSION /;
use File::Spec;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw / LogFile LogHandle / ];

sub init {
  my ($self, %args) = (shift, @_);

  $self->ChooseLogFile or
    die "No valid log file found.\n";
  my $HI;
  open($HI,">>".$self->LogFile->Name) or 
    die "Cannot open Log File ".$self->LogFile->Name."\n";
  $self->LogHandle($HI);
}

sub ChooseLogFile {
  my ($self, %args) = (shift, @_);
  my ($logfile, $file, $dontquit);
  my @files = (
	       $self->LogFile,
	       $self->get_conf->{LogFile},
	       "/var/lib/unilang",
	       "/home/jasayne/.unilang",
	       "/tmp",
	      );

  $dontquit = 1;
  while ($dontquit) {
    $file = undef;
    while (@files && ! defined $file) {
      $file = shift @files
    }
    if (defined $file) {
      print "<<<$file>>>\n";
      $logfile = UniLang::LogFile->new($file);
      if ($self->TryLog($logfile)) {
	$self->LogFile($logfile);
	print "<<<".$self->LogFile->Name.">>>\n";
	return $self->LogFile;
      }
    } else {
      $dontquit = 0;
    }
  }
  return;
}

sub TryLog {
  my ($self, $logfile) = (shift, shift);
  return unless $logfile;

  # test if it is a directory
  if ($logfile->Directory) {
    if ($logfile->Writable) {
      return $self->FindOrCreateValidLog($logfile);
    }
  } elsif ($logfile->Writable) {
    if ($self->ValidLog($logfile)) {
      return 1;
    } else {
      return $self->FindOrCreateValidLog($logfile->DirName);
    }
  }
  return;
}

sub FindOrCreateValidLog {
  my ($self, $logfile) = (shift, shift);
  # in a directory, choose a valid logfile and return true
  $logfile->Name(File::Spec->catpath('',$logfile->Name,"unilang.log"));
  return $self->ValidLog($logfile);
}

sub ValidLog {
  my ($self, $logfile) = (shift, shift);
  if ($logfile->Exists && ! $logfile->Writable) { # check that it exists and is writable

  } else {
    if ($logfile->Validate) {
      $self->LogFile($logfile);
      return 1;
    }
  }
  return;
}

sub Commit {
  my $self = shift;
  my $input = shift;

  my $H = $self->LogHandle;
  print "<".$input->Print.">\n";
  print $H "<".$input->Print.">\n";
}

################################################################################

package UniLang::Parser;
use vars qw/ $VERSION /;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw / Log Agents / ];

sub init {
  my ($self, %args) = (shift, @_);
  $self->Log(UniLang::Log->new);
  $self->Agents(UniLang::Agents->new);
}

sub GetInput {
  my $self = shift;
  my $input = UniLang::Input->new;
  $input->Get;
  $input;
}

sub Converse {
  my $self = shift;
  my $input = $self->GetInput();

  $self->Parse($input);

  $self->Log->Commit($input);
  1;
}

sub Parse {
  my $self = shift;
  my $input = shift;

  # we have  to parse the line here,  this is a where  a parser _is_
  # eventually  involved, however  for now,  we can  just  make some
  # nonsense

  my $l = $input->Contents;
  if ($l =~ /^(\w+?)[:,](.*)$/) {
    if ($self->Agents->IsaAgent(agent => $1)) {
      $self->Agents->Call(agent => $1, contents => $2);
    } else {
      print "No body here by that name.\n";
    }
  # } elsif ($input->contents =~ /./) {
#     # check whether it is a system command, or eshell command
#     my $com = $input->contents;
#     my $res= `which $com`;
#     chomp $res;
#     system "$com" if $res;
  }
}


################################################################################

package UniLang::Agents;
use vars qw/ $VERSION /;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw / agents / ];

sub init {
  my ($self, %args) = (shift,@_);
  $self->agents(
		 {
		  "Unilang" => "UniLang::Agent::Listen",
		  "Todo" => "Todo",
		  "Thoughts" => "Thoughts",
		 }
		);
}

sub Call {
  my ($self, %args) = (shift,@_);
  print "<<<".$args{agent}.">>>\n";
  print "<<<".$self->agents->{$args{agent}}.">>> ";
  print "<<<".$args{contents}.">>>\n";
}

sub IsaAgent {
  my ($self, %args) = (shift,@_);
  return exists $self->agents->{$args{agent}};
}

################################################################################

package UniLang::Input;
use vars qw/ $VERSION /;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw / Contents Date Head Body / ];

sub init {
  
}

sub Get {
  my ($self, %args) = (shift,@_);
  $self->GetContents;
  $self->GetDate;
}

sub GetContents {
  my ($self, %args) = (shift,@_);
  my $contents = <>;
  chomp $contents;
  $self->Contents($contents);
}

sub GetDate {
  my ($self, %args) = (shift,@_);
  my $date = `date`;
  chomp $date;
  $self->Date($date);
}

sub Print {
  my ($self, %args) = (shift,@_);
  "<".$self->Date."> <".$self->Contents.">";
}

sub XMLPrint {
  # prints in XML form, i.e.
  # <communique
  #   date = ""
  #   sender = ""
  #   receiver = ""
  #   contents = "">
}

################################################################################

package UniLang::File;
use vars qw/ $VERSION /;
$VERSION = '1.00';
use strict;
use File::Basename;
use Class::MethodMaker new_with_init => 'new',
  get_set       => [ qw / Name Spec / ];

sub init {
  my ($self,$file) = (shift,shift);
  $self->Name($file);
}

sub DirName {
  my ($self,%args) = (shift,@_);
  dirname($self->Name);
}

sub Open {
  my ($self,%args) = (shift,@_);
  $self->Spec->updir;
}

sub Writable {
  my ($self,%args) = (shift,@_);
  -W $self->Name;
}

sub Exists {
  my ($self,%args) = (shift,@_);
  -e $self->Name;
}

sub Directory {
  my ($self,%args) = (shift,@_);
  -d $self->Name;
}


################################################################################

package UniLang::LogFile;

use strict;
use vars qw($VERSION @ISA);

$VERSION = '1.00';
use File::Basename;
use Class::MethodMaker new_with_init => 'new',
  get_set       => [ qw / Name Spec / ];

sub init {
  my ($self,$file) = (shift,shift);
  $self->Name($file);
}

sub DirName {
  my ($self,%args) = (shift,@_);
  dirname($self->Name);
}

sub Open {
  my ($self,%args) = (shift,@_);
  $self->Spec->updir;
}

sub Writable {
  my ($self,%args) = (shift,@_);
  -W $self->Name;
}

sub Exists {
  my ($self,%args) = (shift,@_);
  -e $self->Name;
}

sub Directory {
  my ($self,%args) = (shift,@_);
  -d $self->Name;
}

sub ReadHeader {
  my ($self,%args) = (shift,@_);
  $self->Spec->updir;
}

sub Version {
  my ($self,%args) = (shift,@_);
  $self->Spec->updir;
}

sub Validate {
  my $self = shift;
  # check that it is of the right format
  print "<<<".$self->Name.">>>\n";
  if (0) {
    $self->Open;
    $self->ReadHeader;
    if ($self->Version eq $self->Version) {
      return 1;
    }
  }
  return 1;
}
