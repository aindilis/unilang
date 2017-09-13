package UniLang::Plugins::WebServices::Server;

use UniLang::Util::System;
use UniLang::Util::TempAgent;
use Sayer;

use Data::Dumper;

use base qw(UniLang::Plugins::WebService);

use Class::MethodMaker
  new_with_init => 'new',
  get_set       =>
  [

   qw / MySayer MyTempAgent Host Port /

  ];

sub init {
  my ($self,%args) = @_;
  UniLang::Plugins::WebService->init(%args);
  $self->Name
    ((ref $self) || "UniLang-WebService-Server");
  my $hostname = `hostname -f`;
  chomp $hostname;
  $self->Host
    ($args{Host} || $hostname);
  $self->Port
    ($args{Port} || GetNewUnusedPort());
}

sub Start {
  my ($self,%args) = @_;
  # my $ref = ref $self;
  # print Dumper({Ref => $ref});
  # $self->StartServer(%args);
  UniLang::Plugins::WebService ->Start(%args);
}

sub StartServer {
  my ($self,%args) = @_;
}

# look into caching things with Sayer

# now, if we receive a message, be sure to check whether we already
# have that server in our list of servers

1;
