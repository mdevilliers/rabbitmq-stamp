
-module(rabbitmq_stamp_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

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