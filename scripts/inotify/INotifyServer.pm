package INotifyServer;

use PerlLib::SwissArmyKnife;
use UniLang::Util::Message;

use Event;
use Linux::Inotify2;
use Tk;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / MyINotify Name MessageDir IncomingDir AgentIncomingFiles /

  ];

sub init {
  my ($self,%args) = @_;
  $self->Name($args{Name} || "0");
  $self->AgentIncomingFiles({});
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
  $self->IncomingDir(ConcatDir($self->MessageDir,"server",$self->Name,"incoming"));
  MkDirIfNotExists
    (
     Directory => $self->IncomingDir,
    );
  $self->MyINotify->watch
    (
     $self->IncomingDir,
     IN_ALL_EVENTS,
     sub {
       my $e = shift;
       # print Dumper($e);
       my $name = $e->fullname;
       if ($e->IN_MODIFY) {
	 my $raw = read_file($name);
	 my $message = UniLang::Util::Message->new
	   (
	    Raw => $raw,
	   );
	 $self->ProcessMessage
	   (
	    Message => $message,
	   );
	 system "rm ".shell_quote($name);
       }
       if (1) {
	 print "$name was modified\n" if $e->IN_MODIFY;
	 print "$name was accessed\n" if $e->IN_ACCESS;
	 print "$name is no longer mounted\n" if $e->IN_UNMOUNT;
	 print "$name is gone\n" if $e->IN_IGNORED;
	 print "events for $name have been lost\n" if $e->IN_Q_OVERFLOW;
	 # cancel this watcher: remove no further events
	 # $e->w->cancel;
       }
     },
    );
}

sub StartListening {
  my ($self,%args) = @_;
  1 while $self->MyINotify->poll;
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

sub ProcessMessage {
  my ($self,%args) = @_;
  print Dumper($args{Message});
}

1;
