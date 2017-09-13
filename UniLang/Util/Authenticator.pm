package UniLang::Util::Authenticator;

use Data::Dumper;

use vars qw/ $VERSION /;
$VERSION = '0.1';

use Class::MethodMaker
  new_with_init => 'new',
  get_set       => [ qw / Debug Type Socket MyAuthenticated ClientHash PassHash CredList Callback  / ];

sub init {
  my ($s, %a) = @_;
  $s->Debug(1);
  $s->Type($a{Type});
  $s->Socket($a{Socket});
  $s->Callback($a{Callback} || \&DefaultCallback);
  $s->PassHash($a{PassHash} || {});
  $s->CredList($a{CredList} || ["default",""]);
}

sub Authenticate {
  my ($s, %a) = @_;
  if ($s->Type eq "server") {
    return $s->ServerSideAuth;
  } elsif ($s->Type eq "client") {
    return $s->ClientSideAuth;
  }
}

sub Authenticated {
  my ($s, %a) = @_;
  if (! defined $s->MyAuthenticated) {
    $s->MyAuthenticated($s->Authenticate);
  }
  return $s->MyAuthenticated;
}

sub ServerSideAuth {
  my ($s,%a) = @_;
  print "Server: Authenticating the new agent\n" if $s->Debug;
  my $sock = $s->Socket;
  print $sock Quote("Agent:");
  my $agent = Unquote($sock->getline);
  if ($agent) {
    print $sock Quote("Passphrase:");
    my $passphrase = Unquote($sock->getline);
    if ($passphrase) {
      # now comes the test
      if (DefaultCallback
 	  (Self => $s,
 	   Agent => $agent,
 	   Passphrase => $passphrase)) {
	print $sock "Authentication successful\n";
	print "Server: Authentication successful\n" if $s->Debug;
	return 1;
      } else {
	# this is invalid, we must dispose of this connection
	print $sock "Authentication failed\n";
      }
    }
  }
  print "Server: Authentication failed\n" if $s->Debug;
  return 0;
}

sub ClientSideAuth {
  my ($s,%a) = @_;
  my $sock = $s->Socket;
  my $line1 = Unquote($sock->getline);
  if ($line1 =~ /^Agent:$/) {
    # supply the passphrase
    print $sock Quote($s->CredList->[0]);
    my $line2 = Unquote($sock->getline);
    if ($line2 =~ /^Passphrase:$/) {
      print $sock Quote($s->CredList->[1]);
      my $line3 = Unquote($sock->getline);
      if ($line3 =~ /^Authentication successful$/) {
	print "Client: Authentication successful\n" if $s->Debug;
	return 1;
      }
    }
  }
  print "Client: Authentication failed\n" if $s->Debug;
  return 0;
}

sub Quote {
  (shift)."\n";
}

sub Unquote {
  my $it = shift;
  $it =~ s/\n$//;
  $it;
}

sub DefaultCallback {
  my (%a) = @_;
  return exists $a{Self}->PassHash->{$a{Agent}}->{$a{Passphrase}};
}

1;
