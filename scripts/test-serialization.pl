#!/usr/bin/perl -w

use RPC::Lite::Serializer::XML;
# # SOAP::Serializer
# # SOAP::StructSerializer
# # XMLRPC::Serializer;

use Data::Dumper;

my $serialize = RPC::Lite::Serializer::XML->new( );
my $res = [$serialize->Serialize({"This is a test" => 1})];

print Dumper({
	      Res => $res,
	     });

