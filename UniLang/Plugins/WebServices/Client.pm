package UniLang::Plugins::WebServices::Client;

use base qw(UniLang::Plugins::WebService);

use Data::Dumper;

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => 
  [

   qw / RemoteHost RemotePort /

  ];

sub init {
  my ($self,%args) = @_;
  UniLang::Plugins::WebService->init(%args);
  $self->Name
    ((ref $self) || "UniLang-WebService-Client");
  $self->RemoteHost
    ($args{RemoteHost} || "localhost");
  $self->RemotePort
    ($args{RemotePort} || 9001);
}

sub Start {
  my ($self,%args) = @_;
  # my $ref = ref $self;
  # print Dumper({Ref => $ref});
  $self->StartClient(%args);
  UniLang::Plugins::WebService->Start(%args);
}

sub StartClient {
  my ($self,%args) = @_;

}

# sub ConnectToRemoteUniLang {
#   my ($self,%args) = @_;
#   # use the appropriate service
# }

1;
