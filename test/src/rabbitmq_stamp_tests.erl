
-module(rabbitmq_stamp_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

can_expect_increasing_identifiers_test() ->
    {ok, Result1} = rabbit_stamp_worker:next( <<"AAA">>),
    {ok, Result2} = rabbit_stamp_worker:next( <<"AAA">>),
    ?assert(Result2 > Result1),
    ?assert(Result2 - Result1 =:= 1),
    ok.

can_expect_different_identifiers_for_more_than_one_exchange_test() ->
    {ok, Result1} = rabbit_stamp_worker:next( <<"BBB">>),
    {ok, Result2} = rabbit_stamp_worker:next( <<"CCC">>),
    ?assert(Result2 > Result1),
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