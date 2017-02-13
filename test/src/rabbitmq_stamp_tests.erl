
-module(rabbitmq_stamp_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

can_send_a_message_to_stamp_exchange_test() ->

    Channel = get_connected_channel(),
    SendingXChangeName = <<"demo-exchange">>,
    RecievingXChangeName = <<"mydestinationexchange">>,

    % declare exchanges
    SendingExchangeDeclare = #'exchange.declare'{exchange = SendingXChangeName , type = <<"x-stamp">>},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, SendingExchangeDeclare),

    RecievingExchangeDeclare = #'exchange.declare'{exchange = RecievingXChangeName , type = <<"fanout">> },
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, RecievingExchangeDeclare),

    % listening queue
    #'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{}),
    Binding = #'queue.bind'{  queue       = Queue,
                              exchange    = RecievingXChangeName,
                              routing_key = <<"#">>},

    #'queue.bind_ok'{} = amqp_channel:call(Channel, Binding),
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue,
                                                     no_ack = true}, self()),

    Payload = <<"foobar">>,
    Key = <<"my_routing_key">>,
    StampHeader = {<<"forward_exchange">>, longstr, RecievingXChangeName},

    Props = #'P_basic'{headers = [StampHeader]},
    Publish = #'basic.publish'{exchange = SendingXChangeName, routing_key = Key},

    Message = #amqp_msg{props = Props, payload = Payload},

    ok = amqp_channel:cast(Channel, Publish, Message),
    
    receive
         #'basic.consume_ok'{} -> ok
    end,

    loop(RecievingXChangeName),
    ok.

loop( DestExchange) ->

    receive
        {#'basic.deliver'{},  #amqp_msg{props = Props}} ->
            Headers = Props#'P_basic'.headers,
            log(" [x] Header ~p~n", Headers),
            {<<"stamp">>,long,_} = lists:nth(1,Headers),
            {<<"forward_exchange">>,longstr, DestExchange} = lists:nth(2,Headers),
            ?assert(length(Headers) =:= 2)
    after 
        1000 ->  ?assert( false )
    end.

can_create_exchange_of_stamp_type_test()->

    Channel = get_connected_channel(),
    XChangeName = <<"demo-exchange">>,
    
    ExchangeDeclare = #'exchange.declare'{exchange = XChangeName , type = <<"x-stamp">>},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, ExchangeDeclare),

	ok.

% private
get_connected_channel()->
    {ok, Connection} = amqp_connection:start(#amqp_params_direct{	
    }),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Channel.

log(Message,Value) ->
    ?debugFmt("~p: ~p~n",[Message,Value]).
