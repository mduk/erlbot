-module( irc ).

-export( [
	parse/1,
	parse_header/1,
	join/1,
	part/1,
	part/2,
	quit/0,
	quit/1,
	say/2,
	valid_channel_name/1
] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse( [ $: | Line ]  ) ->
	parse( Line );
parse( Line ) ->
	{ parse_header( Line ), parse_body( Line ) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse_header/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_header( Line ) ->
	Header = case string:chr( Line, $: ) of
		0 -> Line;
		BodyStart -> string:substr( Line, 1, BodyStart - 1 )
	end,
	[ From, Type, To | Args ] = string:tokens( Header, " " ),
	{ parse_origin( From ), Type, To, parse_args( Args ) }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse_origin/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_origin( From ) ->
	string:tokens( From, "!@" ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse_args/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_args( [] ) -> [];
parse_args( [ Arg | Args ] ) ->
	Parsed = case string:str( Arg, "=" ) of
		0 -> Arg;
		_ -> list_to_tuple( string:tokens( Arg, "=" ) )
	end,
	[ Parsed | parse_args( Args ) ].
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parse_body/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
parse_body( Line ) ->
	case string:chr( Line, $: ) of
		0         -> no_body;
		BodyStart -> string:substr( Line, BodyStart + 1 )
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% join/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
join( Channel ) ->
	case ?MODULE:valid_channel_name( Channel ) of
		false -> { error, "Invalid Channel Name" };
		_     -> lists:concat( [ "JOIN ", Channel, "\r\n" ] )
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% part/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
part( Channel ) ->
	case ?MODULE:valid_channel_name( Channel ) of
		false -> { error, "Invalid Channel Name" };
		_     -> lists:concat( [ "PART ", Channel, "\r\n" ] )
	end.

part( Channel, Message ) ->
	case ?MODULE:valid_channel_name( Channel ) of
		false -> { error, "Invalid Channel Name" };
		_     -> lists:concat( [ "PART ", Channel, " :", Message, "\r\n" ] )
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% quit/0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
quit() ->
	quit( "Bye!" ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% quit/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
quit( Message ) ->
	lists:concat( [ "QUIT :", Message, "\r\n" ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% say/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
say( Channel, Message ) ->
	lists:concat( [ "PRIVMSG ", Channel, " :", Message, "\r\n" ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% valid_channel_name/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
valid_channel_name( Name ) ->
	case string:str( Name, "#" ) of
		1 -> Name;
		_ -> false
	end.