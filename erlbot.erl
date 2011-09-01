-module( erlbot ).
-export( [ start/0 ] ).

start() ->
	{ ok, Config } = file:consult( "config.erl" ),
	
	{ host, Host } = proplists:lookup( host, Config ),
	{ nick, Nick } = proplists:lookup( nick, Config ),
	{ owner, Owner } = proplists:lookup( owner, Config ),
	
	{ ok, Pid } = bot:start_link( Host, Nick, Owner ),
	Pid. 