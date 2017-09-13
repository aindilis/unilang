package UniLang2;

use BOSS::Config;
use MyFRDCSA;
use UniLang2::MAS::MAS;

use Data::Dumper;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / Config MyMAS /

  ];

sub init {
  my ($self,%args) = @_;
  $specification = "
	-u [<host> <port>]					Run UniLang2 on these ports

	--add-service [<host> <type> <port> <password>]		Add a remote UniLang2 server web service
	--remove-service [<host> <type> <port>]			Remove a remote UniLang2 server web service

	--list-active-services					List remote running UniLang2 server web services
	--list-all-services					List all known remote UniLang2 server web services

	--list-active-agents					List all known active agents
	--list-all-agents					List all known active and vivifiable agents

	-l <logfile>						Log file
";

  $UNIVERSAL::systemdir = ConcatDir(Dir("internal codebases"),"unilang");
  $self->Config
    (BOSS::Config->new
     (Spec => $specification,
      ConfFile => ""));
  my $conf = $self->Config->CLIConfig;
  if (exists $conf->{'-u'}) {
    $self->MyMAS
      (UniLang2::MAS::MAS->new
       (
	Host => defined $conf->{-u}->{'<host>'} ? $conf->{-u}->{'<host>'} : "localhost",
	Port => defined $conf->{-u}->{'<port>'} ? $conf->{-u}->{'<port>'} : "9000",
       ));
  } else {
    $self->MyMAS
      (UniLang2::MAS::MAS->new
       (
	Host => "localhost",
	Port => "9000",
       ));
  }
}

sub Execute {
  my ($self,%args) = @_;
  # my $conf = $self->Config->CLIConfig;
  $self->MyMAS->Start;
}

1;
