package UniLang::MAS::Client;
use Net::hostent;

use strict;
use Carp;
use vars qw($VERSION);

$VERSION = '1.00';

use Class::MethodMaker new_with_init => 'new',
  get_set => [ qw / Name Handle Agent / ];

sub init {
  my ($self, %args) = (shift,@_);
  my $i = 0;

  #   foreach my $key (keys %args) {
  #     ++$i;
  #     print "$i) <$key><$args{$key}>\n";
  #   }

  $self->Name($args{Name});
  $self->Handle($args{Handle});

  # now must handshake and authenticate
  #$self->HandShake;
  #$self->Authenticate;
}

sub HandShake {
  my ($self, %args) = (shift, @_);
  my $handle = $self->Handle;
  $handle->autoflush(1);
  print $handle "Welcome to UniLang server: $0.\n";
  my $hostinfo = gethostbyaddr($handle->peeraddr);
  printf "[Connect from %s]\n", $hostinfo->name || $handle->peerhost;
  print $handle "UniLang: Version $VERSION\n";
}

sub Authenticate {
  my ($self, %args) = (shift, @_);
}

1;
