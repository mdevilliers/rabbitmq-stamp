-module (rabbit_stamp_monitor).
-behaviour (gen_server).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export ([start_monitor/0, start_monitoring/0]).

% api
start_monitor() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_monitoring() ->
	{ok,Pid} = rabbit_stamp_worker:start_link(),
	case is_local_pid(Pid) of
		false ->
			monitor(process,Pid),
			{ok,Pid};
		true  ->
			{ok,Pid}
	end.

% gen_server
init([]) ->
	%rabbit_log:info("rabbit_stamp_monitor : starting ~p~n", [self()]),
	start_monitoring(),
	{ok, []}.

handle_call(_, _ , State) ->
    {reply, ok, State}.	

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorReference, process, _SomePid, _Reason}, State) ->
	%rabbit_log:info("rabbit_stamp_monitor : Pid: ~p DOWN. Reason: ~p~n", [SomePid, Reason]),
	%rabbit_log:info("rabbit_stamp_monitor : Attempting re-start on this node~n"),
	start_monitoring(),
	{noreply, State};
handle_info(_Info, State) ->
	%rabbit_log:info("rabbit_stamp_monitor : Info: ~p~n", [_Info]),
    {noreply, State}.

terminate(_, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

% helpers
is_local_pid(Pid) ->
	node() =:= node(Pid).
