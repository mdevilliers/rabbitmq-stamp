
-module(rabbitmq_stamp_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("../../include/rabbit_stamp.hrl").

can_handle_call_increment_key_test()->

    State  = iterate_state(<<"demo-exchange">>, [], 3),
    {CurrentValue, _} = proplists:get_value('demo-exchange', State),

    ?assert(CurrentValue =:= 3),
ok.

can_handle_call_different_keys_test()->

    State  = iterate_state(<<"exchange1">>, [], 2),
    State0  = iterate_state(<<"exchange2">>, State, 1),

    {CurrentValue1, _} = proplists:get_value('exchange1', State0),
    ?assert(CurrentValue1 =:= 2),

    {CurrentValue2, _} = proplists:get_value('exchange2', State0),
    ?assert(CurrentValue2 =:= 1),

ok.

will_pick_up_previous_value()->

    rabbit_stamp_worker:upsert_counter('offset-test-exchange', 5),   

    State  = iterate_state(<<"offset-test-exchange">>, [], 1),

    {CurrentValue1, _} = proplists:get_value('offset-test-exchange', State),

    ?assert(CurrentValue1 =:= 6),

ok.

will_move_over_to_next_offset_test() ->
    
    rabbit_stamp_worker:upsert_counter('offset-test-exchange', 1),

    Iterations = ?COUNT_OFFSET + 10,
    State  = iterate_state(<<"offset-test-exchange">>, [], Iterations),  

    {CurrentValue, Offset} = proplists:get_value('offset-test-exchange', State),

    ?assert(CurrentValue =:= (Iterations + 1)),
    ?assert(Offset =:= ?COUNT_OFFSET * 2),
ok.

can_create_exchange_of_stamp_type_test()->

    Channel = get_connected_channel(),
    XChangeName = <<"demo-exchange">>,
    
    ExchangeDeclare = #'exchange.declare'{exchange = XChangeName , type = <<"x-stamp">>},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),
   
	ok.

% private
get_connected_channel()->
	{ok, Connection} = amqp_connection:start(#amqp_params_direct{}),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Channel.

iterate_state(_, InitialState, 0) ->
    InitialState;
iterate_state(Name, InitialState, Count) ->
    { _, _, NewState } = rabbit_stamp_worker:handle_call( { nothing, Name} , nothing, InitialState),
    NewCount = Count - 1,
    iterate_state(Name,NewState, NewCount).

log(Message,Value) ->
    ?debugFmt("~p: ~p~n",[Message,Value]).