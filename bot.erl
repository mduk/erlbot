-module( bot ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ 
	start_link/3,
	join/2,
	part/2,
	part/3,
	say/3,
	register_plugin/3
] ).

-record( state, {
	pid,
	nick,
	owner,
	server_pid,
	channels
} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% start_link/3
%%==============================================================================
start_link( Host, Owner, Nick ) ->
	gen_server:start_link( ?MODULE, [ Host, Owner, Nick ], [] ).

%%==============================================================================
%% join/2
%%==============================================================================
join( Bot, Channel ) ->
	gen_server:cast( Bot, { join, Channel } ).

%%==============================================================================
%% part/2
%%==============================================================================
part( Bot, Channel ) ->
	gen_server:cast( Bot, { part, Channel, "" } ).

%%==============================================================================
%% part/3
%%==============================================================================
part( Bot, Channel, Message ) ->
	gen_server:cast( Bot, { part, Channel, Message } ).

%%==============================================================================
%% say/3
%%==============================================================================
say( Bot, Recipient, Message ) ->
	gen_server:cast( Bot, { privmsg, Recipient, Message } ).

%%==============================================================================
%% register_plugin/3
%%==============================================================================
register_plugin( Bot, Channel, Plugin ) ->
	gen_server:cast( Bot, { register_plugin, Channel, Plugin } ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% init/1
%%==============================================================================
init( [ Host, Owner, Nick ] ) ->
	Server = server:start_link( self(), Host, Nick ),
	State = #state{
		owner = Owner,
		nick = Nick,
		pid = self(),
		server_pid = Server,
		channels = []
	},
	{ ok, State }.

%%==============================================================================
%% handle_call/3
%%==============================================================================
handle_call( _Request, _From, State ) ->
	{ reply, ok, State }.

%%==============================================================================
%% handle_cast/2
%%==============================================================================
%% Join a channel
%%------------------------------------------------------------------------------
handle_cast( { join, Channel }, State ) ->
	NewState = case proplists:lookup( Channel, State#state.channels ) of
		none -> 
			{ ok, Pid } = channel:start_link( self(), State#state.server_pid, Channel ),
			channel:join( Pid ),
			State#state{ channels = [ { Channel, Pid } | State#state.channels ] };
		
		{ _, Pid } -> 
			channel:join( Pid ),
			State
	end,
	{ noreply, NewState };
%%------------------------------------------------------------------------------
%% Part a channel
%%------------------------------------------------------------------------------
handle_cast( { part, Channel, Message }, State ) ->
	case proplists:lookup( Channel, State#state.channels ) of
		none -> ok;
		
		{ _, Pid } -> 
			channel:part( Pid, Message ),
			State
	end,
	{ noreply, State };
%%------------------------------------------------------------------------------
%% Send a PRIVMSG to a recipient
%%------------------------------------------------------------------------------
handle_cast( { privmsg, Recipient, Message }, State ) ->
	State#state.server_pid ! { send, irc:privmsg( Recipient, Message ) },
	{ noreply, State };
%%------------------------------------------------------------------------------
%% Register a plugin on a channel
%%------------------------------------------------------------------------------
handle_cast( { register_plugin, Channel, Plugin }, State ) ->
	case proplists:lookup( Channel, State#state.channels ) of
		{ _, Pid } -> channel:register_plugin( Pid, Plugin )
	end,
	{ noreply, State };
%%------------------------------------------------------------------------------
%% Received PRIVMSG from Owner to Bot
%%------------------------------------------------------------------------------
handle_cast( { irc, Packet = { { [ Nick | _ ], "PRIVMSG", To, _ }, [ $- | _ ] }, _ }, State ) 
when Nick == State#state.owner, To == State#state.nick ->
	echo( Packet ),
	process_flag( trap_exit, true ),
	spawn_link( fun() -> handle_command( State, Packet ) end ),
	{ noreply, State };
%%------------------------------------------------------------------------------
%% IRC PRIVMSGs
%%------------------------------------------------------------------------------
handle_cast( { irc, Packet = { { _, "PRIVMSG", To = "#" ++ _, _ }, _ }, _Line }, State ) ->
	case proplists:lookup( To, State#state.channels ) of
		{ _, Pid } -> gen_server:cast( Pid, Packet )
	end,
	{ noreply, State };
%%------------------------------------------------------------------------------
%% Catch all IRC packets
%%------------------------------------------------------------------------------
handle_cast( { irc, Packet, _Line }, State ) ->
	echo( Packet ),
	{ noreply, State };
%%------------------------------------------------------------------------------
%% Got a line from the server
%%------------------------------------------------------------------------------
handle_cast( { incoming_line, Line }, State ) ->
	process_flag( trap_exit, true ),
	spawn_link( fun() -> gen_server:cast( State#state.pid, { irc, irc:parse( Line ), Line } ) end ),
	{ noreply, State };
%%------------------------------------------------------------------------------
%% A linked process exited normally
%%------------------------------------------------------------------------------
handle_cast( { 'EXIT', _, normal }, State ) ->
	{ noreply, State };
%%------------------------------------------------------------------------------
%% A linked process exited for some reason
%%------------------------------------------------------------------------------
handle_cast( { 'EXIT', Pid, Reason }, State ) ->
	echo( { "~p crashed because ~p~n", [ Pid, Reason ] } ),
	{ noreply, State };
%%------------------------------------------------------------------------------
%% Server Disconnected
%%------------------------------------------------------------------------------
handle_cast( { _, disconnected }, State ) ->
	echo( "Server Disconnected~n" ),
	{ stop, server_disconnected, State };

%%------------------------------------------------------------------------------
%% Catch all casts
%%------------------------------------------------------------------------------
handle_cast( Message, State ) ->
	echo( { [ "Unknown Cast: ~p~n" ], [ Message ] } ),
	{ noreply, State }.

%%==============================================================================
%% handle_info/2
%%==============================================================================
handle_info( _Info, State ) ->
	{ noreply, State }.

%%==============================================================================
%% terminate/2
%%==============================================================================
terminate( _Reason, _State ) -> 
	ok.

%%==============================================================================
%% code_changed/3
%%==============================================================================
code_change( _OldVsn, State, _Extra ) -> 
	{ ok, State }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%==============================================================================
%% handle_command/2
%%==============================================================================
handle_command( State, { { [ Nick | _ ], _, _, _ }, "-" ++ Body } ) ->
	case string:tokens( Body, " " ) of
	
		%% Echo
		[ "echo" | Tail ] ->
			State#state.server_pid ! { send, irc:privmsg( Nick, string:join( Tail, " " ) ) };
		
		%% Join a channel
		[ "channel", "join", Channel ] -> 
			bot:join( State#state.pid, Channel );
			
		%% Part a channel
		[ "channel", "part", Channel ] -> 
			bot:part( State#state.pid, Channel );
		
		%% List channels
		[ "channel", "list" ] -> 
			gen_server:cast( State#state.pid, { privmsg, Nick, "Channels: " } ),
			lists:foreach( fun( { Channel, _ } ) ->
				gen_server:cast( State#state.pid, { privmsg, Nick, lists:concat( [ "  ", Channel ] ) } )
			end, State#state.channels );
		
		[ "plugin", "register", Channel, Plugin ] ->
			case proplists:lookup( Channel, State#state.channels ) of
				{ _, ChannelPid } ->
					
					PluginName = list_to_atom( Plugin ),
					{ ok, PluginPid } = PluginName:start_link(),
					channel:register_plugin( ChannelPid, PluginPid ),
					
					bot:say( State#state.pid, Nick, io_lib:format( 
						"Plugin ~p (~s) registered on channel ~p (~s).", 
						[ PluginPid, Plugin, ChannelPid, Channel ]
					) )
			end;
			
		
		%% Inject Raw IRC
		[ "raw" | Tail ] ->
			Irc = lists:concat( [ string:join( Tail, " " ), "\r\n" ] ),
			State#state.server_pid ! { send, Irc };
		
		%% Anything else
		Unknown ->
			io:format( ">>> Unknown Command: ~s~n", [ string:join( Unknown, " " ) ] )
	end.

%%==============================================================================
%% echo/1
%%==============================================================================
%% IRC Packet
%%------------------------------------------------------------------------------
echo( { { [ Nick | _ ], Type, To, Args }, Body } ) ->
	Message = case Type of
		"001"     -> { "Welcome: ~s~n",                    [ Body ]                      };
		"002"     -> { "Host: ~s~n",                       [ Body ]                      };
		"003"     -> { "History: ~s~n",                    [ Body ]                      };
		"004"     -> { "???: ~s~n",                        [ string:join( Args, ", " ) ] };
		"005"     -> { "Options: Boring!~n",               []                            };
		"042"     -> { "Unique ID: ~s~n",                  [ Args ]                      };
		"252"     -> { "Operators Online: ~s~n",           [ Args ]                      };
		"251"     -> { "Info: ~s~n",                       [ Body ]                      };
		"254"     -> { "Active Channels: ~s~n",            [ Args ]                      };
		"255"     -> { "Info: ~s~n",                       [ Body ]                      };
		"265"     -> { "Local Load: ~s~n",                 [ Body ]                      };
		"266"     -> { "Global Load: ~s~n",                [ Body ]                      };
		"372"     -> { "MOTD: ~s~n",                       [ Body ]                      };
		"375"     -> { "MOTD: ---Message of the Day---~n", []                            };
		"376"     -> { "MOTD: ---Message of the Day---~n", []                            };
		"396"     -> { "Host Mask: ~s~n",                  [ Args ]                      };
		"433"     -> { "Error: Nickname in use~n",         []                            };
		"451"     -> { "Notice: ~s~n",                     [ Body ]                      };
		"NOTICE"  -> { "Notice: ~s~n",                     [ Body ]                      };
		"MODE"    -> { "Mode: ~s~n",                       [ string:join( Args, ", " ) ] };
		"PRIVMSG" -> { "Privmsg: (~s -> ~s) ~s~n",         [ Nick, To, Body ]            };
		_        -> none
	end,
	case Message of
		{ Format, Params } -> echo( { lists:concat( [ "IRC: ", Format ] ), Params } );
		_                  -> no_message
	end;
%%------------------------------------------------------------------------------
%% Nothing
%%------------------------------------------------------------------------------
echo( nothing ) ->
	ok;
%%------------------------------------------------------------------------------
%% Format and Arguments
%%------------------------------------------------------------------------------
echo( { Format, Args } ) ->
	io:format( lists:concat( [ "Bot> ", Format ] ), Args );
%%------------------------------------------------------------------------------
%% Just Format
%%------------------------------------------------------------------------------
echo( Format ) ->
	io:format( lists:concat( [ "Bot> ", Format ] ) ).