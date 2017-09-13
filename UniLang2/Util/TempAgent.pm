package UniLang2::Util::TempAgent;

use UniLang2::Agent::Agent;
use UniLang2::Util::Message;

use Data::Dumper;
# use Data::Dump qw(dump);

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / MyAgent Name Data Debug Host Port /

  ];

sub init {
  my ($self,%args) = @_;
  my $name;
  if ($args{Name}) {
    $name = $args{Name};
  } elsif ($args{RandName}) {
    $name = $args{RandName}."-".rand();
  } else {
    $name = "temp-agent-".rand();
  }
  $self->Name($name);
  $self->Debug($args{Debug});
  $self->MyAgent
    (UniLang2::Agent::Agent->new
     (Name => $self->Name,
      ReceiveHandler =>
      $args{ReceiveHandler} ||
      sub {$self->Receive(@_)}));
  # print dump($self->MyAgent->ReceiveHandler);
  $self->MyAgent->DoNotDaemonize(1);
  $self->Host($args{Host} || (defined $conf->{-u}->{'<host>'} ? $conf->{-u}->{'<host>'} : "localhost"));
  $self->Port($args{Port} || (defined $conf->{-u}->{'<port>'} ? $conf->{-u}->{'<port>'} : "9000"));
  $self->MyAgent->Register
    (
     Host => $self->Host,
     Port => $self->Port,
     Properties => $args{Properties},
    );
}

sub Receive {
  my ($self,%args) = @_;
  print Dumper($args{Message});
  # $self->MyAgent->UnListen;
}

sub Send {
  my ($self,%args) = @_;
  if ($self->Debug) {
    print Dumper(\%args);
  } else {
    $self->MyAgent->SendContents
      (%args);
    # $self->MyAgent->Listen;
  }
}

sub RPC {
  my ($self,%args) = @_;
  my $m = $self->MyAgent->QueryAgent
    (
     Receiver => $args{Receiver},
     Data => {
	      _RPC_Sub => $args{_RPC_Sub},
	      _RPC_Args => $args{_RPC_Args},
	     },
    );
  return (@{$m->Data->{_RPC_Results}});
}

sub DESTROY {
  my ($self,%args) = @_;
}

1;
