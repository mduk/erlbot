-module( erlbot ).
-export( [ start/0, start/1 ] ).

start() ->
	case file:consult( "config.erl" ) of
		{ ok, Config } -> start( Config );
		Err            -> io:format( "Can't read config. ~p~n", [ Err ] )
	end.

start( Config ) ->
	{ host, Host } = proplists:lookup( host, Config ),
	{ nick, Nick } = proplists:lookup( nick, Config ),
	{ owner, Owner } = proplists:lookup( owner, Config ),
	
	bot:start_link( Host, Owner, Nick ).