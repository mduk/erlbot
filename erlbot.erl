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
	
	{ ok, Bot } = bot:start_link( Host, Owner, Nick ),
	
	case proplists:lookup( channels, Config ) of
		none     -> nevermind;
		{ _, Channels } ->
			 spawn( fun() ->
			 timer:sleep( 3000 ),
			 	lists:foreach( fun( Channel ) ->
			 		bot:join( Bot, Channel )
			 	end, Channels )
			 end )
	end,
	
	Bot.