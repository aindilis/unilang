package UniLang2::Util::TestHarness;

use UniLang2::Util::TempAgent;
use UniLang2::Util::TestHarness::Util;

use Expect;

# use UniLang2::Agent::Agent;
# use UniLang2::Util::Message;

use Data::Dumper;
# use Data::Dump qw(dump);

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / Host Port MyExpect Debug PID /

  ];

sub init {
  my ($self,%args) = @_;
  $self->Host($args{Host} || "localhost");
  $self->Port($args{Port} || "9010");
  $self->Debug($args{Debug});
  $self->MyExpect(Expect->new);
  $self->MyExpect->raw_pty(1);
  $self->MyExpect->log_stdout($self->Debug);
}

sub StartTemporaryUniLangInstance {
  my ($self,%args) = @_;
  # going to initiate a unilang2 instance, look at ushell to see how it
  # does it

  # figure out how to have it allocate a new available port
  # have it start on a testing port for clarity
  my $command = "/var/lib/myfrdcsa/codebases/internal/unilang/start -s -u ".$self->Host." ".$self->Port." -c";
  $self->MyExpect->spawn($command, @parameters)
    or die "Cannot spawn $command: $!\n";
  print "Waiting for UniLang2 to initialize...\n";
  # use expect to start it and wait for it to be ready
  $self->MyExpect->expect(300, [qr/\[Server (.+) accepting clients\]/, sub {print "UniLang2 is Initialized.\n"}]);
  $self->MyExpect->clear_accum();
  # fork off and die!
  my $pid;
  defined($pid = fork()) or die "Cannot fork()!\n";
  if (! $pid) {
    $self->MyExpect->expect(30000, [qr/UniLang2 has been told to quit now/s, sub { print "Dying\n"; exit(0); } ]);
  } else {
    $self->PID($pid);
  }
}

sub StopTemporaryUniLangInstance {
  my ($self,%args) = @_;
  my $tempagent = UniLang2::Util::TempAgent->new
    (
     Host => $self->Host,
     Port => $self->Port,
    );
  print Dumper($tempagent->Send
	       (
		Contents => "UniLang, echo UniLang2 has been told to quit now",
	       ));
  # if that doesn't work, kill it by killing this process
}

1;
