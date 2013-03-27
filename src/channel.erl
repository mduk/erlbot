-module( channel ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ 
	start_link/3,
	join/1,
	part/1,
	part/2,
	say/2,
	handle/2,
	register_plugin/2,
	start_plugin/2
] ).

-record( state, {
	bot,
	server,
	channel,
	plugins,
	status
} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%====================================================================
%% start_link/3
%%====================================================================
start_link( Bot, Server, Channel ) ->
	gen_server:start_link( ?MODULE, [ Bot, Server, Channel ], [] ).

%%====================================================================
%% join/1
%%====================================================================
join( Channel ) ->
	gen_server:cast( Channel, join ).

%%====================================================================
%% part/1
%%====================================================================
part( Channel ) ->
	gen_server:cast( Channel, part ).
	
%%====================================================================
%% part/2
%%====================================================================
part( Channel, Message ) ->
	gen_server:cast( Channel, { part, Message } ).

%%====================================================================
%% say/2
%%====================================================================
say( Channel, Message ) ->
	gen_server:cast( Channel, { say, Message } ).

%%====================================================================
%% handle/2
%%====================================================================
handle( Channel, Packet ) ->
	gen_server:cast( Channel, Packet ).

%%====================================================================
%% register_plugin/2
%%====================================================================
register_plugin( Channel, Plugin ) when is_pid( Plugin ) ->
	gen_server:cast( Channel, { register_plugin, Plugin } ).

%%====================================================================
%% start_plugin/2
%%====================================================================
start_plugin( Channel, Plugin ) ->
	gen_server:cast( Channel, { start_plugin, Plugin } ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init( [ Bot, Server, Channel] ) ->
	State = #state{
		bot = Bot,
		server = Server,
		channel = Channel,
		plugins = [],
		status = parted
	},
	{ ok, State }.

%%====================================================================
%% handle_call/2
%%====================================================================
handle_call( _Request, _From, State ) ->
	{ reply, ok, State }.

%%====================================================================
%% handle_cast/2
%%====================================================================
%% Join the channel
%%--------------------------------------------------------------------
handle_cast( join, State ) ->
	State#state.server ! { send, irc:join( State#state.channel ) },
	{ noreply, State#state{ status = joined } };
%%--------------------------------------------------------------------
%% Part the channel
%%--------------------------------------------------------------------
handle_cast( part, State ) ->
	State#state.server ! { send, irc:part( State#state.channel ) },
	{ noreply, State#state{ status = parted } };
%%--------------------------------------------------------------------
%% Part the channel with a message
%%--------------------------------------------------------------------
handle_cast( { part, Message }, State ) ->
	State#state.server ! { send, irc:part( State#state.channel, Message ) },
	{ noreply, State#state{ status = parted } };
%%--------------------------------------------------------------------
%% Say something to the channel
%%--------------------------------------------------------------------
handle_cast( { say, Message }, State ) ->
	State#state.server ! { send, irc:privmsg( State#state.channel, Message ) },
	{ noreply, State };
%%--------------------------------------------------------------------
%% Register a Plugin
%%--------------------------------------------------------------------
handle_cast( { register_plugin, Plugin }, State ) ->
	NewState = State#state{ plugins = [ Plugin | State#state.plugins ] },
	{ noreply, NewState };
%%--------------------------------------------------------------------
%% Start a Plugin
%%--------------------------------------------------------------------
handle_cast( { start_plugin, Plugin }, State ) ->
	{ ok, PluginPid } = Plugin:start_link(),
	NewState = State#state{ plugins = [ PluginPid | State#state.plugins ] },
	{ noreply, NewState };
%%--------------------------------------------------------------------
%% All PRIVMSGs to the channel
%%--------------------------------------------------------------------
handle_cast( _Packet = { { [ Nick | _ ], "PRIVMSG", _, _ }, Message }, State ) ->
	io:format( "~s> (~s) ~s~n", [ State#state.channel, Nick, Message ] ),
	lists:foreach( fun( Plugin ) ->
		gen_server:cast( Plugin, { self(), privmsg, Nick, Message } )
	end, State#state.plugins ),
	{ noreply, State };
%%--------------------------------------------------------------------
%% Catch all casts
%%--------------------------------------------------------------------
handle_cast( _Msg, State ) ->
	{ noreply, State }.

%%====================================================================
%% handle_info/2
%%====================================================================
handle_info( _Info, State ) ->
	{ noreply, State }.

%%====================================================================
%% terminate/2
%%====================================================================
terminate( _Reason, _State ) -> 
	ok.

%%====================================================================
%% code_change/3
%%====================================================================
code_change( _OldVsn, State, _Extra ) -> 
	{ ok, State }.
