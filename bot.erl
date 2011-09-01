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
	
		{ irc, { { _, "NOTICE", _, _ }, Body }, _ } ->
			io:format( lists:concat( [ "Notice: ", Body, "~n" ] ) ),
			?MODULE:loop( State );
	
		{ irc, { { _, "MODE", _, Args }, _ }, _ } ->
			io:format( lists:concat( [ "Mode: ", string:join( Args, ", " ), "~n" ] ) ),
			?MODULE:loop( State );
	
		{ irc, { { _, "001", _, _ }, Body }, _ } ->
			io:format( lists:concat( [ "Welcome: ", Body, "~n" ] ) ),
			?MODULE:loop( State );
	
		{ irc, { { _, "002", _, _ }, Body }, _ } ->
			io:format( lists:concat( [ "Host: ", Body, "~n" ] ) ),
			?MODULE:loop( State );
	
		{ irc, { { _, "003", _, _ }, Body }, _ } ->
			io:format( lists:concat( [ "History: ", Body, "~n" ] ) ),
			?MODULE:loop( State );
	
		{ irc, { { _, "004", _, Args }, _ }, _ } ->
			io:format( lists:concat( [ "Uhh...: ", string:join( Args, ", " ), "~n" ] ) ),
			?MODULE:loop( State );
	
		{ irc, { { _, "005", _, _ }, _ }, _ } ->
			io:format( "Options: Boring!~n" ),
			?MODULE:loop( State );
	
		{ irc, { { _, "042", _, Args }, _ }, _ } ->
			[ Id ] = Args,
			io:format( "Unique ID: ~p~n", [ Id ] ),
			?MODULE:loop( State );
	
		{ irc, { { _, "252", _, Args }, _ }, _ } ->
			[ Ops ] = Args,
			io:format( "Operators Online: ~s~n", [ Ops ] ),
			?MODULE:loop( State );
	
		{ irc, { { _, "251", _, _ }, Body }, _ } ->
			io:format( "Info: ~s~n", [ Body ] ),
			?MODULE:loop( State );
	
		{ irc, { { _, "254", _, Args }, _ }, _ } ->
			[ Channels ] = Args,
			io:format( "Active Channels: ~s~n", [ Channels ] ),
			?MODULE:loop( State );
	
		{ irc, { { _, "255", _, _ }, Body }, _ } ->
			io:format( "Info: ~s~n", [ Body ] ),
			?MODULE:loop( State );
	
		{ irc, { { _, "265", _, _ }, Body }, _ } ->
			io:format( "Local Load: ~s~n", [ Body ] ),
			?MODULE:loop( State );
	
		{ irc, { { _, "266", _, _ }, Body }, _ } ->
			io:format( "Global Load: ~s~n", [ Body ] ),
			?MODULE:loop( State );
	
		{ irc, { { _, "372", _, _ }, Body }, _ } ->
			io:format( lists:concat( [ "MOTD: ", Body, "~n" ] ) ),
			?MODULE:loop( State );
		
		{ irc, { { _, "375", _, _ }, _ }, _ } -> ?MODULE:loop( State );
		{ irc, { { _, "376", _, _ }, _ }, _ } -> ?MODULE:loop( State );
	
		{ irc, { { _, "396", _, Args }, _ }, _ } ->
			[ HostMask ] = Args,
			io:format( "Host Mask: ~s~n", [ HostMask ] ),
			?MODULE:loop( State );
	
		{ irc, { { _, "433", _, _ }, _ }, _ } ->
			io:format( "!!! Nickname in use. Quitting.~n" ),
			self() ! quit;
	
		{ irc, { { _, "451", _, _ }, Body }, _ } ->
			io:format( lists:concat( [ "Notice: ", Body, "~n" ] ) ),
			?MODULE:loop( State );
	
		%% Bot Commands
		%%
		%% -join Channel
		%% -part Channel
		%% -quit
		%% -raw Input...
		{ irc, { { [ MyOwner | _ ], "PRIVMSG", MyNick, _ }, [ $- | Body ] }, _ } ->
			
			spawn( fun() ->
				[ Command | Args ] = string:tokens( Body, " " ),
				io:format( ">>> Command: ~p, Args: ~p~n", [ Command, Args ] ),
				case Command of
				
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
				end
			end ),
			
			?MODULE:loop( State );
		
		%% IRC Parse Error
		{ irc, error, Line } ->
			io:format( "Parse Error: ~s~n", [ Line ] ),
			?MODULE:loop( State );
	
		%% IRC Packet Catch-all
		{ irc, Packet, _Raw } ->
			io:format( "Got: ~p~n", [ Packet ] ),
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
	