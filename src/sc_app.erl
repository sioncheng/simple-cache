-module(sc_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
	io:format("start sc_app, sc_store:init"),
	sc_store:init(),
	io:format("start sc_app, sc_store:init ok"),
	case sc_sup:start_link() of 
		{ok, Pid} -> 
			io:format("start ok ~p ~n", [Pid]),
			{ok, Pid};
		Other -> 
			io:format("start error ~p ~n", [Other]),
			{error, Other}
	end.

stop(_State) ->
	ok.

