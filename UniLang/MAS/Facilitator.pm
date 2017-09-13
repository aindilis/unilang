package UniLang::MAS::Facilitator;

use MyFRDCSA qw (ConcatDir Dir);

use Data::Dumper;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / AgentRegistry PIDs /

  ];

sub init {
  my ($self,%args) = @_;
  $self->Attribute($args{Attribute} || "");
  if ($args{AgentRegistry}) {
    $self->AgentRegistry
      ($args{AgentRegistry});
  } else {
    my $f = $args{AgentRegistryFile} || $UNIVERSAL::systemdir."/data/agent-registry.pl";
    if (-f $f) {
      my $c = `cat "$f"`;
      my $e = eval $c;
      $self->AgentRegistry($e);
    }
  }
  $self->PIDs({});
}

sub StartUp {
  my ($self,%args) = @_;
  my $agents = $args{Agents};
  foreach my $agent (@$agents) {
    $self->StartAgent
      (Agent => $agent);
  }
}

sub StartAgent {
  my ($self,%args) = @_;
  my $agent = $args{Agent};
  if (exists $self->AgentRegistry->{$agent}) {
    if (0) {
      system $self->AgentRegistry->{$agent}. " &";
    } else {
      # better to do it with a fork I think
      my $pid = fork();
      if ($pid) {
	# we're the parent
	$self->PIDs->{$agent} = $pid;
      } else {
	# we're the child
	exec $self->AgentRegistry->{$agent};
      }
    }
  }
}

sub StopAgent {
  my ($self,%args) = @_;
  $UNIVERSAL::unilang->Deregister(Agent => $args{Agent});
  # wait a second and kill it
  system "kill -9 ".$self->PIDs->{$args{Agent}};
}

1;

