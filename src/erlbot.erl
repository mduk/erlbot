-module( erlbot ).

-export( [ start/0 ] ).

-behaviour( application ).
-export( [ start/2, stop/1 ] ).

start() ->
	application:start( erlbot ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% application callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%====================================================================
%% start/2
%%====================================================================
%% Start without arguments - read config file
%%--------------------------------------------------------------------
start( Type, [] ) ->
	{ ok, Config } = file:consult( "config.erl" ),
	start( Type, Config );
%%--------------------------------------------------------------------
%% Start with arguments
%%--------------------------------------------------------------------
start( _Type, Config ) ->
	{ host, Host } = proplists:lookup( host, Config ),
	{ nick, Nick } = proplists:lookup( nick, Config ),
	{ owner, Owner } = proplists:lookup( owner, Config ),
	
	{ ok, Bot } = bot:start_link( Host, Owner, Nick ),
	
	case proplists:lookup( channels, Config ) of
		none            -> nevermind;
		{ _, Channels } -> spawn( fun() -> start_channels( Bot, Channels ) end )
	end,
	
	{ ok, Bot }.

%%====================================================================
%% stop/2
%%====================================================================
stop( _State ) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%====================================================================
%% start_channels/2
%%====================================================================
%% Join default channels
%%--------------------------------------------------------------------
start_channels( Bot, Channels ) ->
	timer:sleep( 3000 ),
	lists:foreach( fun( Channel ) ->
		bot:join( Bot, Channel )
	end, Channels ).
