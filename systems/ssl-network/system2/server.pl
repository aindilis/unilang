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

my $server = IO::Socket::INET->new(
    LocalAddr => $SSL_SERVER_ADDR,
    LocalPort => 9000,
    Listen => 2,
    ReuseAddr => 1,
);

print "not ok\n", exit if !$server;
ok("Server Initialization");

print "not " if (!defined fileno($server));
ok("Server Fileno Check");

my ($SSL_SERVER_PORT) = unpack_sockaddr_in( $server->sockname );

my $csock = $server->accept || print "not ";
ok( "tcp accept" );

my %extra_options = $Net::SSLeay::VERSION>=1.16 ?
    (
	SSL_key_file => "certs/client-key.enc", 
	SSL_passwd_cb => sub { return "opossum" }
    ) : (
	SSL_key_file => "certs/client-key.pem"
    );

IO::Socket::SSL->start_SSL( $csock,
    SSL_server => 1,
    SSL_verify_mode => 0x00,
    SSL_ca_file => "certs/test-ca.pem",
    SSL_use_cert => 1,
    SSL_cert_file => "certs/client-cert.pem",
    SSL_version => 'TLSv1',
    SSL_cipher_list => 'HIGH',
    %extra_options
) || print "not ";
#DEBUG( $IO::Socket::SSL::ERROR );
ok( 'sslify server' );

UNIVERSAL::isa( $csock,'IO::Socket::SSL' ) || print "not ";
ok( 'server reblessed as IO::Socket::SSL' );

my $l = <$csock>;
#DEBUG($l);
print "not " if $l ne "hannibal\n";
ok( "received client message" );


wait;



sub ok { print "ok #$_[0]\n"; }

