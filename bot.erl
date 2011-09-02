-module( bot ).
-compile( export_all ).

-record( state, {
	owner,
	nick,
	pid,
	server_pid
} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start( Host, Owner, Nick ) ->
	spawn( ?MODULE, init, [ Host, Owner, Nick ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start_link/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link( Host, Owner, Nick ) ->
	spawn_link( ?MODULE, init, [ Host, Owner, Nick ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% init/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init( Host, Owner, Nick ) ->
	Server = server:start_link( self(), Host, Nick ),
	
	State = #state{
		owner = Owner,
		nick = Nick,
		pid = self(),
		server_pid = Server
	},
	
	?MODULE:loop( State ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% loop/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop( State ) ->
	MyOwner = State#state.owner,
	MyNick = State#state.nick,
	receive
	
		%% Send a PRIVMSG
		{ privmsg, Recipient, Message } ->
			State#state.server_pid ! { send, irc:privmsg( Recipient, Message ) },
			?MODULE:loop( State );
		
		%% Bot Commands
		%%
		%% -join Channel
		%% -part Channel
		%% -quit
		%% -raw Input...
		{ irc, Packet = { { [ MyOwner | _ ], "PRIVMSG", MyNick, _ }, [ $- | _ ] }, _ } ->
			spawn( fun() -> handle_command( State, Packet ) end ),
			?MODULE:loop( State );
		
		%% IRC Parse Error
		{ irc, error, Line } ->
			io:format( "Parse Error: ~s~n", [ Line ] ),
			?MODULE:loop( State );
	
		%% IRC Packet Catch-all
		{ irc, Packet, _Raw } ->
			echo( Packet ),
			?MODULE:loop( State );
		
		%% Parse a line from the server
		{ incoming_line, Line } ->
			
			process_flag( trap_exit, true ),
			spawn_link( fun() -> 
				State#state.pid ! { irc, irc:parse( Line ), Line }
			end ),
			
			?MODULE:loop( State );
		
		%% A linked process exited normally
		{ 'EXIT', _, normal } ->
			?MODULE:loop( State );
		
		%% A linked process exited for some reason
		{ 'EXIT', Pid, Reason } ->
			io:format( "~p crashed because ~p~n", [ Pid, Reason ] ),
			?MODULE:loop( State );
		
		%% Server Disconnected
		{ _, disconnected } 
			-> io:format( "Server Disconnected~n" );
	
		%% Quit
		quit 
			-> State#state.server_pid ! { send, irc:quit() };
		
		%% Anything else
		Unknown -> 
			io:format( "Bot got: ~p~n", [ Unknown ] ),
			?MODULE:loop( State )
		
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%====================================================================
%% handle_command/1
%%====================================================================
handle_command( State, { { [ Nick | _ ], _, _, _ }, "-" ++ Body } ) ->
	[ Command | Args ] = string:tokens( Body, " " ),
	io:format( ">>> Command: ~p, Args: ~p~n", [ Command, Args ] ),
	case Command of
	
		%% Echo
		"echo" ->
			State#state.server_pid ! { send, irc:privmsg( Nick, string:join( Args, " " ) ) };
		
		%% Join a channel
		"join" ->
			[ Channel | _ ] = Args,
			case irc:join( Channel ) of
				{ error, Reason } -> io:format( "Join Error: ~p~n", [ Reason ] );
				Irc               -> State#state.server_pid ! { send, Irc }
			end;
			
		%% Part a channel
		"part" ->
			[ Channel | _ ] = Args,
			case irc:part( Channel ) of
				{ error, Reason } -> io:format( "Part Error: ~p~n", [ Reason ] );
				Irc               -> State#state.server_pid ! { send, Irc }
			end;
		
		%% Quit the server
		"quit" ->
			Message = lists:concat( [ string:join( Args, " " ), "\r\n" ] ),
			State#state.server_pid ! { self(), send, irc:quit( Message ) };
		
		%% Inject Raw IRC
		"raw" ->
			Command = lists:concat( [ string:join( Args, " " ), "\r\n" ] ),
			State#state.server_pid ! { send, Command };
		
		%% Anything else
		_ -> io:format( ">>> Unknown Command: ~p Args: ~p~n", [ Command, Args ] )
	end.

%%====================================================================
%% echo/1
%%====================================================================
%% IRC Packet
%%--------------------------------------------------------------------
echo( { { [ Nick | _ ], Type, To, Args }, Body } ) ->
	Message = case Type of
		"001"     -> { [ "Welcome: ~s~n" ],                    [ Body ]                      };
		"002"     -> { [ "Host: ~s~n" ],                       [ Body ]                      };
		"003"     -> { [ "History: ~s~n" ],                    [ Body ]                      };
		"004"     -> { [ "???: ~s~n" ],                        [ string:join( Args, ", " ) ] };
		"005"     -> { [ "Options: Boring!~n" ],               []                            };
		"042"     -> { [ "Unique ID: ~s~n" ],                  [ Args ]                      };
		"252"     -> { [ "Operators Online: ~s~n" ],           [ Args ]                      };
		"251"     -> { [ "Info: ~s~n" ],                       [ Body ]                      };
		"254"     -> { [ "Active Channels: ~s~n" ],            [ Args ]                      };
		"255"     -> { [ "Info: ~s~n" ],                       [ Body ]                      };
		"265"     -> { [ "Local Load: ~s~n" ],                 [ Body ]                      };
		"266"     -> { [ "Global Load: ~s~n" ],                [ Body ]                      };
		"372"     -> { [ "MOTD: ~s~n" ],                       [ Body ]                      };
		"375"     -> { [ "MOTD: ---Message of the Day---~n" ], []                            };
		"376"     -> { [ "MOTD: ---Message of the Day---~n" ], []                            };
		"396"     -> { [ "Host Mask: ~s~n" ],                  [ Args ]                      };
		"433"     -> { [ "Error: Nickname in use~n" ],         []                            };
		"451"     -> { [ "Notice: ~s~n" ],                     [ Body ]                      };
		"NOTICE"  -> { [ "Notice: ~s~n" ],                     [ Body ]                      };
		"MODE"    -> { [ "Mode: ~s~n" ],                       [ string:join( Args, ", " ) ] };
		"PRIVMSG" -> { [ "Privmsg: (~s -> ~s) ~s~n" ],         [ Nick, To, Body ]            };
		_        -> none
	end,
	case Message of
		{ Format, Params } -> echo( { [ "IRC: " | Format ], Params } );
		_                  -> no_message
	end;
%%--------------------------------------------------------------------
%% Nothing
%%--------------------------------------------------------------------
echo( nothing ) ->
	ok;
%%--------------------------------------------------------------------
%% Format and Arguments
%%--------------------------------------------------------------------
echo( { Format, Args } ) ->
	io:format( lists:concat( [ "Bot> " | Format ] ), Args ).