-module( youtube ).

-behaviour( gen_server ).
-export( [ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3 ] ).

-export( [ start_link/0 ] ).

-record( state, {
	lookups
} ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%====================================================================
%% start_link/2
%%====================================================================
start_link() ->
	gen_server:start_link( ?MODULE, [], [] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%====================================================================
%% init/2
%%====================================================================
init( [] ) ->
	State = #state{
		lookups = 0
	},
	inets:start(),
	{ ok, State }.

%%====================================================================
%% handle_call/2
%%====================================================================
handle_call( _Request, _From, State ) ->
	{ reply, ok, State }.

%%====================================================================
%% handle_cast/2
%%====================================================================
%% Command
%%--------------------------------------------------------------------
handle_cast( { Channel, privmsg, _, "-" ++ Body }, State ) ->
	NewState = case string:tokens( Body, " " ) of

		[ "youtube", "lookup", VideoId ] -> 
			spawn( fun() -> lookup( Channel, VideoId ) end ),
			State#state{ lookups = State#state.lookups + 1 };
		
		_ -> State
		
	end,
	{ noreply, NewState };
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
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% private functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%====================================================================
%% lookup/2
%%====================================================================
lookup( Channel, VideoId ) ->
	Url = "http://gdata.youtube.com/feeds/api/videos/" ++ VideoId,
	{ ok, { { _, 200, _ }, _, XmlString } } = httpc:request( Url ),
	{ Xml, _ } = xmerl_scan:string( XmlString ),
	
	[ { xmlText, _, _, _, Title, _ } ] = 
		xmerl_xpath:string( "//title/text()", Xml ),
	
	[ { xmlText, _, _, _, Uploader, _ } ] = 
		xmerl_xpath:string( "//author/name/text()", Xml ),
	
	[ { xmlAttribute, _, _, _, _, _, _, _, Duration, _ } ] = 
		xmerl_xpath:string( "//yt:duration/@seconds", Xml, [] ),
	
	[ { xmlAttribute, _, _, _, _, _, _, _, FavouriteCount, _ } ] = 
		xmerl_xpath:string( "//yt:statistics/@favoriteCount", Xml, [] ),
	
	[ { xmlAttribute, _, _, _, _, _, _, _, ViewCount, _ } ] = 
		xmerl_xpath:string( "//yt:statistics/@viewCount", Xml, [] ),
	
	channel:say( Channel, io_lib:format(
		"~s (~s) ~s Seconds. Favourites: ~s Views: ~s",
		[ Title, Uploader, Duration, FavouriteCount, ViewCount ]
	) ).