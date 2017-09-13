(Okay here's the deal
 (Say a local unilang instance receives a message for "Enju,
 process", with it's data.  However, Enju isn't available for the
  machine.  CLARIFY THIS (because it is but can't run, so how to
			  deal with that).
  
  
  So, the machine says, okay, who has Enju?  And it looks up in a
  table which remote machines have a working copy of enju.  Then
  it tries to figure out which ones it should try to contact, in
  which order, to see if they have it.  These calculations should
  include run time of remote system, utilization, latency, etc.
  So it determines which machines.  In our test case, this is
  simple, just return ("frdcsa.org", "XMLRPC", 8558).  So there arises
  the confusion of which UniLang MAS Server is connected to which
  XMLRPC agent, since we've allowed for multiple unilang servers
  per machine...

  Is it time to implement agent classes?  It may seem so...

  Okay, once the machine has this server information, it checks
  to see whether it already has a connection open to it.  It
  checks if there is a web-service agent running that has those
  specs, and if so, it tries it.  If there is a problem, it
  trouble shoots the problem.  For instance, if the ws-client
  agent is not running, if it is but can't connect, if it can
  connect but the remote machine doesn't have an agent by that
  name, if the remote agent doesn't respond, and so forth.

  If it is a queryagent call, it tries to get a reply from the
  machine, it returns that.
  )

 (now, how does the remote ws-server agent handle all of this
  (it get's a message, addressed to Enju, and just routes it
  along, and if it's a query agent, it route back any response.)
  
  (now, I suppose we need to translate sender and receiver
  information, because say you send a message to Enju, with the
  Sender of Formalize.  Well, when the remote UniLang routes that
  message to Enju, Enju will reply to Formalize, which will in
  turn respond to the local machine.  So we need to figure out
  how to deal with that.)


  )
 
 )

(How might a knowledge based approach to all of this work?  Is it
 worth implementing FreeKBS reasoning before going ahead and
 implementing UniLang Web Services?  Can we succeed with
 reasoning?  Is it so easy?  Will it tire us out and prevent us
 from ever returning to the motivating problems?

 (Here is how a knowledge based UniLang might work)

 (UniLang has to route it's communications to FreeKBS to do
  reasoning.  So for this, we need to Fix FreeKBS to use the Data
  segment more productively.  We should also begin a knowledge
  base about the semantics of all UniLang Messages.
  )

 (first of all, there should be many different agents that can do
  the same thing.  so for instance, there might be 3 Enju agent
  instances running at the same time.  They would have unique
  names, but they would all be ("isa" ?AGENT "Enju UniLang
 Agent").  That way, when a message was addressed to an agent
  s.t.  any of those would be addressed.  There could be different
  types of addressing, e.g. all matching, first matching, best
  matching, etc.  And procedures for figuring out how to fail
  over.  Also, sometimes, in an emergency, you would send to all
  and return the first response, or maybe even a Vote procedure,
  etc.
  
  Seems like BDI could play a large role in all of this....

  )
 
 (All I can say is this seems pretty complicated, so what we may
  just want to do is save all of this to UniLang version 2.  On
  the other hand, delaying these improvements might lead us down
  a dumb development path.  However, I am not terribly interested
  in these improvements just yet.  Maybe we can compromise and
  implement a TOY approach that only solves the basic route and
  leaves everything else open for future improvements, such as
  the sender types.  No, this will break all the existing code.
  We'll have to figure something else out for now.
  )
 )
