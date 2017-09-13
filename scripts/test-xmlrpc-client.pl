#!/usr/bin/perl -w

# connect to it and send it a test message...

use XMLRPC::Lite;

use Data::Dumper;

my $serverstring;
if (1) {
  $serverstring = "http://ai.frdcsa.org:10000";
} else {
  $serverstring = "http://127.0.0.1:10000";
}

my $result = [
	      XMLRPC::Lite
	      -> proxy($serverstring)
	      -> call(
		      # 'Org.FRDCSA.UniLang.Plugins.WebService.SendContents',
		      'Org.FRDCSA.UniLang.Plugins.WebService.echo',
		      [Test => "hello there"],
		     )
	      ->result,
	     ];

print Dumper($result);
