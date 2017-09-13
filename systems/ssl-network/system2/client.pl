#!/usr/bin/perl -w

# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl t/nonblock.t'

use Net::SSLeay;
use Socket;
use IO::Socket::SSL;
use IO::Select;
use Errno qw(EAGAIN EINPROGRESS );
use strict;

use vars qw( $SSL_SERVER_ADDR );
do "t/ssl_settings.req" || do "ssl_settings.req";

$|=1;

my ($SSL_SERVER_PORT) = 9000; # unpack_sockaddr_in( $server->sockname );

my %extra_options = $Net::SSLeay::VERSION>=1.16 ?
  (
   SSL_key_file => "certs/server-key.enc",
   SSL_passwd_cb => sub { return "bluebell" },
   #	    SSL_verify_callback => \&verify_sub
  ) : (
       SSL_key_file => "certs/server-key.pem"
      );

my $client = IO::Socket::INET->new( "$SSL_SERVER_ADDR:$SSL_SERVER_PORT" )
  || print "not ";
ok( "client tcp connect" );

unless ( IO::Socket::SSL->start_SSL( $client,
				     SSL_version => 'TLSv1',
				     SSL_cipher_list => 'HIGH',
				     %extra_options
				   )) {
  #DEBUG( $SSL_ERROR );
  print "not ";
}
ok( "sslify client" );

UNIVERSAL::isa( $client,'IO::Socket::SSL' ) || print "not ";
ok( 'client reblessed as IO::Socket::SSL' );

print $client "hannibal\n";

exit;

sub ok { print "ok #$_[0]\n"; }
