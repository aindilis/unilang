package UniLang2::Util::Log;

use PerlLib::MySQL;
use UniLang2::Util::LogFile;
use UniLang2::Util::Message;

use Data::Dumper;
use FileHandle;
use File::Spec;

use vars qw/ $VERSION /;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw / LogFile LogHandle MyMySQL Months / ];

sub init {
  my ($self, %args) = (shift, @_);
  $self->MyMySQL
    (PerlLib::MySQL->new
     (DBName => "unilang"));
  $self->Months({
		 "Jan" => 1,
		 "Feb" => 2,
		 "Mar" => 3,
		 "Apr" => 4,
		 "May" => 5,
		 "Jun" => 6,
		 "Jul" => 7,
		 "Aug" => 8,
		 "Sep" => 9,
		 "Oct" => 10,
		 "Nov" => 11,
		 "Dec" => 12,
		});
  if (0) {
    $self->ChooseLogFile or
      die "No valid log file found.\n";
    my $HI;
    open($HI,">>".$self->LogFile->Name) or
      die "Cannot open Log File ".$self->LogFile->Name."\n";
    $HI->autoflush(1);
    $self->LogHandle($HI);
  }
}

sub ChooseLogFile {
  my ($self, %args) = (shift, @_);
  my ($logfile, $file, $dontquit);
  my @files = (
	       $self->LogFile,
	       $UNIVERSAL::unilang2->Config->CLIConfig->{'-l'},
	       "/var/lib/pse",
	       "/home/jasayne/.pse",
	       "/tmp",
	      );

  $dontquit = 1;
  while ($dontquit) {
    $file = undef;
    while (@files && ! defined $file) {
      $file = shift @files
    }
    if (defined $file) {
      $logfile = UniLang2::Util::LogFile->new($file);
      if ($self->TryLog($logfile)) {
	$self->LogFile($logfile);
	print "Using logfile: ".$self->LogFile->Name."\n";
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
  $logfile->Name(File::Spec->catpath('',$logfile->Name,"pse.log"));
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
  my ($self, $message) = (shift, shift);
  return if exists $message->Data->{_DoNotLog};
  my @values;
  my $i = 1;
  foreach my $key (qw(Sender Receiver Date Contents Data)) {
    if ($key eq "Date") {
      my $date = $message->Date; #$_->child_text($i);
      if ($date =~ /^(\w+)\s+(\w+)\s+([0-9]+)\s+([0-9:]+)\s+(\w+)\s+([0-9]+)$/) {
	# Wed Dec 29 15:33:10 EST 2004
	my $newdate = sprintf("%04i-%02i-%02i %s",$6,$self->Months->{$2},$3,$4);
	push @values, $newdate;
      } elsif ($date =~ /^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}$/) {
	push @values, $date;
      } else {
	print "error\n";
      }
    } elsif ($key eq "Data") {
      push @values, Dumper($message->$key);
    } else {
      push @values, $message->$key;
    }
    ++$i;
  }
  my $s = "insert into messages values (NULL,".
    join(", ", map $self->MyMySQL->Quote($_), @values).
      ");";
  # print $s."\n";

  $self->MyMySQL->Do(Statement => $s);
  my $id = $self->MyMySQL->InsertID(Table => "messages");
  $message->ID($id2);
  return $id;

  # now try to have it classified by Corpus if the sender is UniLang-Client or something
  if (0) {
    my $H = $self->LogHandle;
    # print $message->Generate;
    print $H $message->Generate
  }
}

1;
