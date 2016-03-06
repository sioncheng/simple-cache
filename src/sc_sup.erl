-module(sc_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Value, LatestTime) ->
	supervisor:start_child(?SERVER, [Value, LatestTime]).

init([]) ->
	Element = {sc_element, {sc_element, start_link, []},
		temporary, brutal_kill, worker, [sc_element]},
		%% the child specification is
		%% {ID, StartFunc, Restart, Shutdown, Type, Modules}.
		%% there is a blog for it
		%% https://pdincau.wordpress.com/2010/01/28/supervisors-in-erlang-otp/
	Children = [Element],
	RestartStrategy = {simple_one_for_one, 0, 1},
	{ok, {RestartStrategy, Children}}.