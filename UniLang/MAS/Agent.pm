package UniLang::MAS::Agent;

use UniLang::Util::Message;

use Data::Dumper;
use IO::Socket;
use IO::Select;
use IO::Handle;
use Net::hostent;
use Net::Telnet;

use vars qw/ $VERSION /;
$VERSION = '1.00';
use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw / Name Client / ];

sub init {
  my ($self, %args) = (shift,@_);
  # add  the read set  and the  client information  when the  agent is
  # created
  $self->Name($args{Name});
  $self->Client($args{Client});
}

sub GrowTwig {
  my ($self, %args) = (shift,@_);
  # prints in XML form, i.e.
  my $agent = XML::Twig::Elt->new('agent');
  my $name = XML::Twig::Elt->new('name');
  $name->set_text($self->Name);
  $name->paste('last_child', $agent);

  my $client = XML::Twig::Elt->new('client');
  $client->set_text(Dumper($self->Client));
  $client->paste('last_child', $agent);
  return $agent;
}

#sub DESTROY {
#  my ($self, %args) = (shift,@_);
#}
#
#sub Register {
#  my ($self, %args) = (shift,@_);
#  $self->Connect(%args);
#  $self->Startup;
#}
#
#sub Deregister {
#  my ($self, %args) = (shift,@_);
#  $self->Shutdown;
#  $self->Disconnect;
#}
#
#sub Startup {
#  my ($self, %args) = (shift,@_);
# just create a message to send to unilang with your name
#}
#
#sub Shutdown {
#  # just send a message to unilang with saying you are quiting
#}
#
#sub Connect {
#  my ($self, %args) = (shift,@_);
#  $self->Client(Net::Telnet->new(%args));
#  $self->Client->open or die
#    "Cannot open connection to server";
#  my $handle = $self->Client;
#  $self->ReadSet->add($handle);
#}
#
#sub Disconnect {
#  my ($self, %args) = (shift,@_);
#  $self->Client->close;
#}
#
#sub SendUser {
#  my ($self, %args) = (shift,@_);
#  print $args{Message}->Contents;
#}
#
#sub Listen {
#  my ($self,%args) = (shift,@_);
#  while (1) {
#    my ($buf, @handles);
#    my $timeout = 0;
#    do {
#      @handles = $self->ReadSet->can_read($timeout);
#    } while (!@handles);
#    foreach my $handle (@handles) {
#      my $message;
#      if ($handle eq \*STDIN) {
#	my $contents = <>;
#	chomp $contents;
#	my $date = `date`;
#	chomp $date;
#	$message = UniLang::Util::Message->new(Sender => $self->Name,
#					       Receiver => "UniLang",
#					       Date => $date,
#					       Contents => $contents);
#	$self->Send(Handle => $self->Client,
#		    Message => $message);
#      } elsif ($handle eq $self->Client) {
#	my $contents = $self->Receive(Handle => $self->Client);
#	if ($contents) {
#	  my $message = UniLang::Util::Message->new(Raw => $contents);
#	  $self->SendUser(Message => $message);
#	}
#      }
#    }
#  }
#}
#
#sub Print {
#  my ($self,%args) = (shift,@_);
#  print $self->Name . "\n";
#}
#
#sub Send {
#  my ($self,%args) = (shift,@_);
#  my $message = $args{Message};
#  my $handle = $args{Handle};
#  print $handle $message->Generate;
#}
#
#sub Receive {
#  my ($self,%args) = (shift,@_);
#  my $handle = $args{Handle};
#  my $line;
#  my $contents;
#  $line = $handle->getline();
#  if ($line) {
#    $contents .= $line;
#    my $closure = $line;
#    $closure =~ s/^</<\//;
#    do {
#      $line = $handle->getline();
#      $contents .= $line;
#    } while ($line ne $closure);
#    return $contents;
#  }
#  return;
#}

1;
