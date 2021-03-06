#!/usr/bin/perl -w

# connect to it and send it a test message...

use XMLRPC::Lite;

use Data::Dumper;

use UniLang::Util::Message;

my $serverstring;
if (1) {
  $serverstring = "http://ai.frdcsa.org:10000";
} elsif (0) {
  $serverstring = "http://localhost:10000";
} else {
  $serverstring = "http://127.0.0.1:10000";
}

# my $contents = '<message>
#   <id>1</id>
#   <sender>Alexa-Skill-FLP</sender>
#   <receiver>Agent1</receiver>
#   <date>Fri Feb  3 02:14:29 CST 2017</date>
#   <contents></contents>
#   <data>$VAR1 = {};</data>
# </message>';

my $contents = '<message>
  <id>1</id>
  <sender>Alexa-Skill-FLP</sender>
  <receiver>Agent1</receiver>
  <date>Fri Feb  3 02:14:29 CST 2017</date>
  <contents></contents>
  <data>$VAR1 = {_DoNotLog => 1, Eval => [[\'_prolog_list\',[\'_prolog_list\',\\*{\'::?Response\'}],[\'alexaSkillFLPQuery\',\'When is my next appointment with Jennifer?\',\\*{\'::?Response\'}]]]};\n	</data>
</message>';


# my $message = UniLang::Util::Message->new(Raw => $bcontents);

my $result = [
	      XMLRPC::Lite
	      -> proxy($serverstring)
	      -> call(
		      'Org.FRDCSA.UniLang.Plugins.WebService.QueryAgent',
		      [$contents],
		     )
	      -> result
	     ];

print Dumper($result);
