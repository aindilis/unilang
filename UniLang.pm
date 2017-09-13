package UniLang;

use BOSS::Config;
use MyFRDCSA;
use UniLang::MAS::MAS;

use Data::Dumper;
use Time::HiRes qw( time );

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / Config MyMAS StartTime /

  ];

sub init {
  my ($self,%args) = @_;
  $specification = "
	-u [<host> <port>]	Run UniLang on these ports
	-W [<delay>]		Exit as soon as possible (with optional delay)

	--add-service [<host> <type> <port> <password>]		Add a remote UniLang server web service
	--remove-service [<host> <type> <port>]			Remove a remote UniLang server web service

	--list-active-services					List remote running UniLang server web services
	--list-all-services					List all known remote UniLang server web services

	--list-active-agents					List all known active agents
	--list-all-agents					List all known active and vivifiable agents

	-l <logfile>		Log file

	-t			Test to see if we can start the server at all
";

  $UNIVERSAL::systemdir = ConcatDir(Dir("internal codebases"),"unilang");
  $self->Config
    (BOSS::Config->new
     (Spec => $specification,
      ConfFile => ""));
  my $conf = $self->Config->CLIConfig;
  if (exists $conf->{'-W'}) {
    $self->StartTime(time());
  }
  if (exists $conf->{'-u'}) {
    $self->MyMAS
      (UniLang::MAS::MAS->new
       (
	Host => defined $conf->{-u}->{'<host>'} ? $conf->{-u}->{'<host>'} : "localhost",
	Port => defined $conf->{-u}->{'<port>'} ? $conf->{-u}->{'<port>'} : "9000",
	Conf => $conf,
       ));
  } else {
    $self->MyMAS
      (UniLang::MAS::MAS->new
       (
	Host => "localhost",
	Port => "9000",
	Conf => $conf,
       ));
  }
}

sub Execute {
  my ($self,%args) = @_;
  # my $conf = $self->Config->CLIConfig;
  $self->MyMAS->Start;
}

1;
