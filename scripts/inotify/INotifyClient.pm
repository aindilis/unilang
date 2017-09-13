package INotifyClient;

use PerlLib::SwissArmyKnife;
use UniLang::Util::Message;

use Event;
use Linux::Inotify2;
use Tk;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / MyINotify Name MessageDir IncomingFile ServerIncomingDir MessageCounter /

  ];

sub init {
  my ($self,%args) = @_;
  $self->Name($args{Name} || "New-Agent");
  $self->MessageDir("/var/lib/myfrdcsa/codebases/internal/unilang/scripts/inotify/messages");
  $self->MyINotify
    (
     Linux::Inotify2->new() or
     die "Unable to create new inotify object: $!"
    );
  Event->io
    (
     fd => $self->MyINotify->fileno,
     poll => 'r',
     cb => sub { $self->MyINotify->poll },
    );
  $self->IncomingFile(ConcatDir($self->MessageDir,"clients",$self->Name,"incoming"));
  MkDirIfNotExists
    (
     Directory => dirname($self->IncomingFile),
    );
  $self->ServerIncomingDir(ConcatDir($self->MessageDir,"server","0","incoming"));
  $self->MessageCounter(0);
  $self->MyINotify->watch
    (
     $self->IncomingFile,
     IN_ALL_EVENTS,
     sub {
       my $e = shift;
       print Dumper($e);
       my $name = $e->fullname;
       print "$name was modified\n" if $e->IN_MODIFY;
       print "$name was accessed\n" if $e->IN_ACCESS;
       print "$name is no longer mounted\n" if $e->IN_UNMOUNT;
       print "$name is gone\n" if $e->IN_IGNORED;
       print "events for $name have been lost\n" if $e->IN_Q_OVERFLOW;
       # cancel this watcher: remove no further events
       # $e->w->cancel;

       # go ahead and absorb all the messages in incoming truncate it

       # put a lock on the file somehow
       my $messages = read_file($self->IncomingFile);
       system "trunate 0 ".shell_quote($self->IncomingFile);
       print "Messages: $messages\n";
     },
    );
}

sub StartListening {
  my ($self,%args) = @_;

}

sub Listen {
  my ($self,%args) = @_;
  my $timeout = $args{Timeout};
  if ($args{Timeout}) {
    # need to listen for a certain length of time
  } else {
    $inotify->poll;
  }
}

sub Register {
  my ($self,%args) = @_;
  MkDirIfNotExists(Directory => ConcatDir($self->ServerIncomingDir,$self->Name));
}

sub Send {
  my ($self,%args) = @_;
  my $fh = IO::File->new();
  my $filename = ConcatDir($self->ServerIncomingDir,$self->Name."-".$self->MessageCounter);
  my $message = UniLang::Util::Message->new
    (
     ID => $self->MessageCounter,
     Sender => $self->Name,
     Receiver => $args{Receiver} || "UniLang",
     # Date => "",
     Contents => $args{Contents},
     Data => $args{Data} || {},
    );
  $fh->open(">>$filename") or warn "Cannot open file\n";
  print $fh $message->Generate;
  $fh->close();
  $self->MessageCounter($self->MessageCounter + 1);
}

1;
