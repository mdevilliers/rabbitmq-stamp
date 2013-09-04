-module(rabbit_stamp_worker).
-behaviour(gen_server).

-export([start_link/0, next/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    rabbit_log:info("rabbit_stamp_worker : starting...~n"),
    case gen_server:start_link({global, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} -> 
            rabbit_log:info("rabbit_stamp_worker : started locally on ~p~n", [node()]),
            {ok, Pid};
        {error, {already_started, Pid}} -> 
            rabbit_log:info("rabbit_stamp_worker : already started on ~p node ~p ~n", [Pid, node(Pid)]), 
            {ok, Pid};
        Else -> Else
    end.

next(<<ExchangeName/binary>>) ->
    ExchangeName0 = list_to_atom( binary_to_list( ExchangeName ) ),
    gen_server:call( find_worker() , {next, ExchangeName0});
next(ExchangeName) when is_atom(ExchangeName) ->
    gen_server:call( find_worker() , {next, ExchangeName}).

init([]) ->
    {ok, []}.

handle_call({next, ExchangeName}, _From , State) ->

    case proplists:get_value(ExchangeName, State, unknown) of
        unknown -> 
            CurrentCount = get_timestamp();
        {LocalCount} ->
            CurrentCount = LocalCount
    end,

	NextCount = CurrentCount + 1,

	NewState0 = proplists:delete(ExchangeName, State),

    {reply, {ok,NextCount}, [{ExchangeName, {NextCount}}| NewState0]}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

% helpers
get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.

find_worker() ->
    global:whereis_name(rabbit_stamp_worker).