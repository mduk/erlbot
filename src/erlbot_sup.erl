-module( erlbot_sup ).

-behaviour( supervisor ).
-export( [ init/1 ] ).

-export( [ start_link/0, start_worker/4, start_worker/3, start_supervisor/4 ] ).

start_link() ->
	supervisor:start_link( { local, ?MODULE }, ?MODULE, [] ).

start_worker( M, F, A ) ->
	start_worker( M, M, F, A ).

start_worker( Id, M, F, A ) ->
	Mfa = { M, F, A },
	Child = { Id,  Mfa, permanent, 5000, worker, [ M ] },
	supervisor:start_child( ?MODULE, Child ).

start_supervisor( Id, M, F, A ) ->
	Mfa = { M, F, A },
	Child = { Id,  Mfa, permanent, 5000, supervisor, [ M ] },
	supervisor:start_child( ?MODULE, Child ).

init( [] ) ->
	{ ok, { { one_for_one, 5, 10 }, [] } }.