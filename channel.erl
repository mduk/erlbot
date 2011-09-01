-module( channel ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ 
	start_link/3,
	join/1,
	part/1,
	part/2,
	say/2
] ).

-record( state, {
	bot,
	server,
	channel
} ).

start_link( Bot, Server, Channel ) ->
	gen_server:start_link( ?MODULE, [ Bot, Server, Channel ], [] ).

join( Channel ) ->
	gen_server:cast( Channel, join ).

part( Channel ) ->
	gen_server:cast( Channel, part ).
	
part( Channel, Message ) ->
	gen_server:cast( Channel, { part, Message } ).

say( Channel, Message ) ->
	gen_server:cast( Channel, { say, Message } ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init( [ Bot, Server, Channel] ) ->
	State = #state{
		bot = Bot,
		server = Server,
		channel = Channel
	},
	{ ok, State }.

handle_call( _Request, _From, State ) ->
	{ reply, ok, State }.

%%====================================================================
%% handle_cast/2
%%====================================================================
handle_cast( join, State ) ->
	State#state.server ! { send, irc:join( State#state.channel ) },
	{ noreply, State };
%%--------------------------------------------------------------------
handle_cast( part, State ) ->
	State#state.server ! { send, irc:part( State#state.channel ) },
	{ noreply, State };
%%--------------------------------------------------------------------
handle_cast( { part, Message }, State ) ->
	State#state.server ! { send, irc:part( State#state.channel, Message ) },
	{ noreply, State };
%%--------------------------------------------------------------------
handle_cast( { say, Message }, State ) ->
	State#state.server ! { send, irc:say( State#state.channel, Message ) },
	{ noreply, State };
%%--------------------------------------------------------------------
handle_cast( _Msg, State ) ->
	{ noreply, State }.

handle_info( _Info, State ) ->
	{ noreply, State }.

terminate( _Reason, _State ) -> 
	ok.

code_change(_OldVsn, State, _Extra) -> 
	{ ok, State }.
  
