-module( server ).
-compile( export_all ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start( Pid, { Host, Port }, Nick ) -> 
	spawn( ?MODULE, init, [ Pid, Host, Port, Nick ] );
start( Pid, Host, Nick ) -> 
	spawn( ?MODULE, init, [ Pid, Host, 6667, Nick ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% start_link/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link( Pid, { Host, Port }, Nick ) ->
	spawn_link( ?MODULE, init, [ Pid, Host, Port, Nick ] );
start_link( Pid, Host, Nick ) ->
	spawn_link( ?MODULE, init, [ Pid, Host, 6667, Nick ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% init/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init( Bot, Host, Port, Nick ) ->
	{ ok, Sock } = gen_tcp:connect( Host, Port, [
		binary,
		{ packet, 0 },
		{ nodelay, true },
		{ keepalive, true },
		{ active, true },
		{ reuseaddr, true }
	] ),
	
	spawn( fun() ->
		timer:sleep( 1500 ),
	
		gen_tcp:send( Sock, lists:concat( [ "NICK ", Nick, "\r\n" ] ) ),
		gen_tcp:send( Sock, lists:concat( [ "USER ", Nick, " localhost localdomain : ", Nick, "\r\n" ] ) ),
		Bot ! { self, login_done } 
	end ),
	
	?MODULE:loop( Bot, Sock ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% loop/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
loop( Bot, Sock ) ->
	receive

		%% Ping
		{ tcp, _Sock, <<$P, $I, $N, $G, $ , Rest/binary>> } ->
			gen_tcp:send( Sock, lists:concat( [ 
				"PONG ", 
				binary_to_list( Rest ) 
			] ) ),
			?MODULE:loop( Bot, Sock );
		
		%% Socket data in
		{ tcp, _Sock, Bin } ->
			Data = binary_to_list( Bin ),
			case process_incoming( Bot, Data ) of
				[]       -> ok;
				LeftOver -> io:format( "<server> Left Over: ~s~n", [ LeftOver ] )
			end,
			?MODULE:loop( Bot, Sock );
		
		%% Socket closed
		{ tcp_closed, _Port } ->
			Bot ! { self(), disconnected },
			?MODULE:loop( Bot, Sock );
		
		%% Send packet to server
		{ send, Packet } ->
			gen_tcp:send( Sock, Packet ),
			?MODULE:loop( Bot, Sock );
		
		%% Close socket
		quit ->
			gen_tcp:close( Sock );
		
		%% Anything else
		Unknown ->
			io:format( "Server Got: ~p~n", [ Unknown ] ),
			?MODULE:loop( Bot, Sock )
			
	end.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% process_incoming/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
process_incoming( Bot, Data ) ->
	Position = string:str( Data, "\r\n" ),
	case Position of
		0 -> [];
		_ ->
			Line = string:substr( Data, 1, Position - 1 ),
			Remainder = string:substr( Data, Position + 2, string:len( Data ) - ( Position - 1 ) ),
			
			gen_server:cast( Bot, { incoming_line, Line } ),
			
			?MODULE:process_incoming( Bot, Remainder )
	end.