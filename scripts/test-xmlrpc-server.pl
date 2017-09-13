#!/usr/bin/perl -w

use Data::Dumper;
use RPC::XML::Server;

my $host = shift;
my $port = shift;

if (! (defined $host and defined $port)) {
  print "Usage: server <host> <port>\n";
}

my $rpc = RPC::XML::Server->new
  (
   no_default => 1,
   host => $host,
   port => $port,
  );

$rpc->add_method
  ({
    name => "Org.FRDCSA.UniLang.Plugins.WebService.simple",
    version => "1.0",
    signature => [
		  'array',
		  'array array',
		 ],
    code => sub {
      print Dumper(\@_);
      return 1;
    },
    help => "This is the WebService version of the SendContents
      method, and it just sends it and I believe returns a 1 if it got
      off okay...",
   });

$rpc->add_default_methods;

# print Dumper($rpc);

RPCXMLServerLoop
  (
   TimeOut => 0.01,
  );

sub RPCXMLServerLoop {
  my (%args) = @_;
  if ($rpc->{__daemon}) {
    my ($conn, $req, $resp, $reqxml, $return, $respxml, $exit_now,
	$timeout);

    # Localize and set the signal handler as an exit route
    my @exit_signals;

    if (exists $args{signal} and $args{signal} ne 'NONE') {
      @exit_signals =
	(ref $args{signal}) ? @{$args{signal}} : $args{signal};
    } else {
      push @exit_signals, 'INT';
    }

    local @SIG{@exit_signals} = (sub { $exit_now++ }) x @exit_signals;

    $rpc->started('set');
    $exit_now = 0;
    $timeout  = $rpc->{__daemon}->timeout(0.1);
    while (!$exit_now) {
      $conn = $rpc->{__daemon}->accept;
      last if $exit_now;
      next unless $conn;

      $conn->timeout($rpc->timeout);
      $rpc->process_request($conn);
      $conn->close;
      undef $conn;		# Free up any lingering resources
    }

    $rpc->{__daemon}->timeout($timeout) if defined $timeout;
  }
}
