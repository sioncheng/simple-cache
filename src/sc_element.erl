-module(sc_element).

-behaviour(gen_server).

%%% --------------
%%% api
%%% --------------
-export([start_link/2,
	create/2,
	create/1,
	fetch/1,
	delete/1,
	replace/2
	]).

%%% --------------
%%% gen_server callback
%%% --------------
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
	]).


-define(SERVER, ?MODULE).
-define(DEFAULT_LEASE_TIME, (60 * 60 * 24)).

-record(state, {value, lease_time, start_time}).

%%% =====================
%%% api functions
%%% =====================
start_link(Value, LeaseTime) ->
	gen_server:start_link(?MODULE, [Value, LeaseTime], []).

create(Value, LeaseTime) ->
	sc_sup:start_child(Value, LeaseTime).

create(Value) ->
	create(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
	gen_server:call(Pid, fetch).

delete(Pid) ->
	gen_server:cast(Pid, delete).

replace(Pid, Value) ->
	gen_server:cast(Pid, {replace, Value}).



%%% =====================
%%% callback functions
%%% =====================
init([Value, LeaseTime]) ->
	CurrentTime = current_time(),
	State = #state{value=Value, lease_time=LeaseTime, start_time = CurrentTime},
	LeftTime = left_time(CurrentTime, LeaseTime),
	{ok, State, LeftTime}.

handle_call(fetch, _From, State) ->
	#state{value = Value, lease_time = LeaseTime, start_time = StartTime} = State,
	LeftTime = left_time(StartTime, LeaseTime),
	{reply, {ok, Value}, State, LeftTime}.

handle_cast({replace, Value}, State)->
	#state{lease_time = LeaseTime, start_time = StartTime} = State,
	LeftTime = left_time(StartTime, LeaseTime),
	NewState = State#state{value = Value},
	{noreply, NewState, LeftTime};
handle_cast(delete, State) ->
	{stop, normal, State}.

handle_info(timeout, State) ->
	{stop, normal, State}.

terminate(_Reason, _State) ->
	sc_store:delete(self()),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%% =====================
%%% internal functions
%%% =====================
left_time(_StartTime, infinity) ->
	infinity;
left_time(StartTime, LeaseTime) ->
	CurrentTime = current_time(),
	TimeElapsed = CurrentTime - StartTime,
	case LeaseTime - TimeElapsed of
		Time when Time < 0 -> 0;
		Time -> Time * 1000
	end.

current_time() ->
	Now = calendar:local_time(),
	calendar:datetime_to_gregorian_seconds(Now).