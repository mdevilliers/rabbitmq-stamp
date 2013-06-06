
-module(rabbitmq_stamp_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

can_handle_call_increment_key_test()->

    { _, _, State0 } = rabbit_stamp_worker:handle_call( { nothing, <<"demo-exchange">>} , nothing, []),
    { _, _, State1 } = rabbit_stamp_worker:handle_call( { nothing, <<"demo-exchange">>} , nothing, State0),
    { _, _, State2 } = rabbit_stamp_worker:handle_call( { nothing, <<"demo-exchange">>} , nothing, State1),
    ?assert(proplists:get_value('demo-exchange', State2) =:= 3),
ok.

can_handle_call_different_keys_test()->

    { _, _, State0 } = rabbit_stamp_worker:handle_call( { nothing, <<"exchange1">>} , nothing, []),
    { _, _, State1 } = rabbit_stamp_worker:handle_call( { nothing, <<"exchange2">>} , nothing, State0),
    { _, _, State2 } = rabbit_stamp_worker:handle_call( { nothing, <<"exchange1">>} , nothing, State1),

    ?assert(proplists:get_value('exchange1', State2) =:= 2),
    ?assert(proplists:get_value('exchange2', State2) =:= 1),
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

log(Message,Value) ->
    ?debugFmt("~p: ~p~n",[Message,Value]).
