-module(rabbit_stamp_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([setup_schema/0]).
-export([upsert_counter/2,find_counter/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("../include/rabbit_stamp.hrl").

%-rabbit_boot_step({rabbit_stamp_mnesia,
%  [{description, "rabbit stamp exchange type: mnesia"},
%    {mfa, {?MODULE, setup_schema, []}},
%    {requires, database},
%    {enables, external_infrastructure}]}).

start_link() ->
    rabbit_log:info("rabbit_stamp_worker : starting...~n"),
    case gen_server:start_link({global, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} -> 
            {ok, Pid};
        {error, {already_started, Pid}} -> 
            rabbit_log:info("rabbit_stamp_worker : Already started on ~p node ~p ~n", [Pid, node(Pid)]), 
            {ok, Pid};
        Else -> Else
    end.

init([]) ->
    {ok, []}.

handle_call({_, ExchangeName}, _From , State) ->

	Key = list_to_atom( binary_to_list( ExchangeName ) ),

    case proplists:get_value(Key, State, unknown) of
        unknown -> 
            % i don't have it locally
            %case find_counter(Key) of
            %    [] ->
            %        % i don't have it in mnesia 
            %        % so create it
            %        upsert_counter(Key, ?COUNT_OFFSET),
                    CurrentOffset = ?COUNT_OFFSET,
                    CurrentCount = 0 ;
            %    {_,Key,CurrentCount} ->
            %        % i have it in mnesia
            %        % update the offset to be safe as this might be a restart
            %        CurrentOffset = CurrentCount + ?COUNT_OFFSET,
            %        upsert_counter(Key, CurrentOffset)
            %end;
        {LocalCount, LocalOffset} ->
            %i have it locally
            CurrentCount = LocalCount,
            CurrentOffset = LocalOffset
    end,

	NextCount = CurrentCount + 1,

    case NextCount =:= CurrentOffset of
        true ->
            CurrentOffset0 = CurrentCount + ?COUNT_OFFSET;
           % upsert_counter(Key, CurrentOffset0);
        false ->
            CurrentOffset0 = CurrentOffset
    end,

	NewState0 = proplists:delete(Key, State),

    {reply, {ok,NextCount}, [{Key, {NextCount, CurrentOffset0}}| NewState0]}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

% mnesia set up
setup_schema() ->
  case mnesia:create_table(rabbit_stamp_state_offset,
          [{attributes, record_info(fields, rabbit_stamp_state_offset)},
           {type, set},
           {disc_copies, [node()]}]) of 
      {atomic, ok} -> ok;
      {aborted, {already_exists, rabbit_stamp_state_offset}} -> ok
  end. 

upsert_counter(Name, Count) ->
    F = fun() ->
         case mnesia:wread({rabbit_stamp_state_offset, Name}) of
            [P] -> 
                mnesia:write(P#rabbit_stamp_state_offset{high=Count});
            []  -> 
                mnesia:write(#rabbit_stamp_state_offset{exchangeName=Name, high=Count})
        end
    end, 
    mnesia:activity(transaction,F).

find_counter(Name) ->
    F = fun() ->
        mnesia:read({rabbit_stamp_state_offset, Name})
    end,
    case mnesia:transaction(F) of
        {atomic,[Row]} -> Row ;
        {atomic,[]} -> []
    end.

