-module( erlbot ).

-export( [ start/0 ] ).

-behaviour( application ).
-export( [ start/2, stop/1 ] ).

start() ->
	application:start( sasl ),
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
start( _Type, Conf ) ->
	R = erlbot_sup:start_link(),
	Bots = proplists:lookup_all( bot, Conf ),
	start_bots( Bots ),
	R.

%%====================================================================
%% stop/1
%%====================================================================
stop( _State ) ->
	ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%====================================================================
%% start_bots/1
%%====================================================================
start_bots( [ { bot, Alias, Config } | Bots ]  ) ->
	{ host, Host } = proplists:lookup( host, Config ),
	{ nick, Nick } = proplists:lookup( nick, Config ),
	{ owner, Owner } = proplists:lookup( owner, Config ),
	
	{ ok, Bot } = erlbot_sup:start_worker( Alias, erlbot_bot, start_link, [ Host, Owner, Nick ] ),
	
	case proplists:lookup( channels, Config ) of
		none            -> nevermind;
		{ _, Channels } -> spawn( fun() -> start_channels( Bot, Channels ) end )
	end,
	
	start_bots( Bots );
start_bots( [] ) ->
	ok.

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
		erlbot_bot:join( Bot, Channel )
	end, Channels ).
