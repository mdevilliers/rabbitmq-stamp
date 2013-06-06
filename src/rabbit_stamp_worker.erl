-module(rabbit_stamp_worker).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

handle_call({_, ExchangeName}, _From , State) ->

	Key = list_to_atom( binary_to_list( ExchangeName ) ),
	CurrentCount = proplists:get_value(Key, State, 0),
	NextCount = CurrentCount + 1,
	NewState0 = proplists:delete(Key, State),

    {reply, {ok,NextCount}, [{Key, NextCount}| NewState0]}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.
