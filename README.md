# unilang
Trusty old interprocess communication system which aspires to eventually be a MAS

http://frdcsa.org/frdcsa/internal/unilang

<system>
  <title>UniLang</title>
  <slogan>
    No sooner said, done
  </slogan>
  <short-description>
    Stream of consciousness multiagent system
  </short-description>
  <medium-description>
    UniLang coordinates the agents and allows them to send messages to
    each   other.   It   performs  multistrategy,   context  sensitive
    classification  and routing of  messages.  It  is very  useful for
    recording  arbitrary thoughts,  since it  will route  them  to the
    appropriate locations  and services, performing  translation where
    necessary and  possible, interacting with  to disambiguate serious
    cases.
  </medium-description>
  <long-description>
    <p>
      UniLang is a system for  providing the services you would expect
      from  a  Multi-Agent System  (MAS),  and  more.   It allows  IPC
      between  different Agents connecting  through sockets,  and also
      has packages which client Agents can use to do most of the work,
      based on Event.pm.
    </p>
    <p>    
      From a users perspective,  the UniLang-Client Agent allows us to
      communicate with a wide variety  of agents through a single text
      based client,  from inside the provided  Emacs major-mode called
      ushell.
    </p>
    <p>
      Ushell now incorporates  a bidirectional interface between Emacs
      and UniLang, allowing us  to greatly enhance integration between
      Emacs  and all other  systems.  For  instance, the  long awaited
      Emacs Agenda  system was  completed with just  one function  - a
      call to PSE from within Emacs through UniLang.
    </p>
    <p>
      Automatic  classification of commands  is working  (see corpus).
      Therefore,  one can  simply  write what  they  are thinking  and
      UniLang will dispatch this to the proper agent.
    </p>
    <p>
      Here is  an example of automatically  classified commands.  Note
      that  the classifier, which  usually works  well, is  not giving
      accurate classifications for some as simple but unknown reason.

<pre>
The classifications are not correct, but, whatever.
	[icodebase-capability-request, capability-radar, capability-unilang, solution-to-extant-problem, goal]

I think we should have some kind of autoformatting for UniLang.
	[capability-manager, unilang-specific-message, observation, shopping-list-item, complex-statement]

Great - corpus is working well, if a bit slow.
	[shopping-list-item, complex-statement, capability-manager, unilang-specific-message, observation]

Now all we have to do is fix that data and worry about complexity/efficiency issues with Corpus doing autoclassification.  We also have to add more elegant classification than bayes, as well as talking amongst agents to support better categorization and command execution from our notes.
	[shopping-list-item, complex-statement]

I wonder how much memory this takes up.
	[icodebase-capability-request, goal, solution-to-extant-problem, capability-radar, capability-unilang]

Maybe there's a way to save classifications models so the whole thing need not be reloaded.
	[complex-statement, icodebase-capability-request, capability-radar, capability-unilang, solution-to-extant-problem]

UniLang agents should, defined through Audience, have a nominal state that allows UniLang while dynamically starting them upon messages received for them (so they don't all have to start at once), that indicates they are ready for processing.
	[goal, icodebase-capability-request, capability-radar, solution-to-extant-problem, capability-unilang]

Should define a general purpose classification scheme - so that the system can get really specific about what it just classified.
	[capability-manager, shopping-list-item, complex-statement, observation, icodebase-capability-request]

Complex-statement for instance can be checked for using get_sentences
	[observation, shopping-list-item, complex-statement, capability-manager, unilang-specific-message]

Debugging UniLang is rather difficult and we don't want to fall prey to the same problem as with GNU Hurd.
	[capability-manager, goal, observation, shopping-list-item, complex-statement]

Now we have enough demos to definitely get people working on our projects.  Mike is right that I should learn persuasion.
	[shopping-list-item, complex-statement, unilang-specific-message, capability-manager, observation]

We have to hurry, though.
	[unilang-specific-message, observation, complex-statement, shopping-list-item, icodebase-capability-request]

You know, should set up regression testing soon.
	[shopping-list-item, complex-statement, capability-manager, unilang-specific-message]
</pre>
    </p>
    <p>
      Here is a random  example of typical UniLang-Client traffic from
      the log file.  Note that this is only traffic from the user, not
      the other agents.  I have  annotated them with very rough sample
      classifications. Often,  one entry  will initiate or  continue a
      great amount of activity.

      <ul>
	<li>(PSE  - add  to BOSS  todo) Replace  all the  uses  of use
	Package qw  ( F1  F2 ... )  and simply export  these functions
	instead.</li>
	<li>(PSE - add to BOSS todo) Document all programs soon, since
	I am already forgetting what they do.</li>
	<li>(Bugzilla -  file bug on RADAR)  Status: Available through
	apt-get, yet sudo  apt-get install libnet-google-perl says its
	already the latest.</li>
	<li>(PSE  - add  to RADAR  and Predator  todos)  Compute error
	probabilities   for   every   phase  of   the   RADAR-Predator
	system.</li>
	<li>(BOSS  - add  to  Machiavelli's requirements)  Machiavelli
	should also try to estimate  what other people are working on,
	so that we can be sure not to redouble efforts. </li>
	<li>(BOSS  -  add to  RADAR's  requirements) Incorporate  that
	special stuff for  tracking upstream that I read  in Wed Aug 4
	21:23:53 EDT 2004 debian weekly news.</li>
	<li>(MyFRDCSA  -  search  capabilities)  What  are  our  video
	editing options?</li>
	<li>(BOSS - add to Predator's requirements) automatic analysis
	of whether a package belongs to free/non-free, etc.</li>
	<li>(Machiavelli  - assert related  group) http://acmsoft.org/
	looks to be in a similar position to us.</li>
	<li>(Audience   -  add   to   Eric's  queue)   send  to   Eric
	http://www.nitle.org/semantic_search.php</li>
	<li>(BOSS - add to FRDCSA-website requirements) Make a website
	which poses questions, and  allows people to answer them.  For
	instance,  What is  a good  system that  acts like  a personal
	wayback machine?</li>
      </ul>
    </p>
    <p>
      Currently, the corpus system still is mainly responsible for the
      classification of  these logs.  Additionally, we  are working on
      adding  translation capabilities  between the  various languages
      encountered, hence  the name, UniLang.  We will  utilize a great
      many   tools  for  this   from  the   knowledge  representation,
      linguistic  ontological  engineering,  and  machine  translation
      fields.  Examples forthcoming.

      Humorous unforeseen interaction.

<pre>
  Lord, don't let me fall, don't let me FALL!
<message>
  <id>1</id>
  <sender>UniLang</sender>
  <receiver>UniLang-Client</receiver>
  <date>Mon Oct  2 22:41:05 CDT 2006</date>
  <contents>No one here by that name.
</contents>
</message>
</pre>
    </p>
  </long-description>
</system>
