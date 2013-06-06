-module(rabbit_stamp_exchange).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([description/0, serialise_events/0, route/2]).
-export([validate/1, create/2, delete/3, add_binding/3,
         remove_bindings/3, assert_args_equivalence/2,policy_changed/2,validate_binding/2 ]).

-rabbit_boot_step({?MODULE,
  [{description, "stamp exchange type: registry"},
    {mfa, {rabbit_registry, register, 
    	[exchange, <<"x-stamp">>, ?MODULE]}},
    {requires, rabbit_registry},
    {enables, kernel_ready}]}).

-behaviour(rabbit_exchange_type).

description() ->
  [{name, <<"x-stamp">>},
   {description, <<"RabbitMQ Exchange to provide globally unique incrementing identifiers for messages sent to it before forwarding to a destination exchange.">>}].

serialise_events() -> false.

route(#exchange{name = XName}, Delivery) ->	
	%rabbit_log:info("stamp:message routed: ~p!~n", [XName]),
	
	BasicMessage = (Delivery#delivery.message),
  	Content = (BasicMessage#basic_message.content),
  	Headers = rabbit_basic:extract_headers(Content),
  	[RoutingKey|_] = BasicMessage#basic_message.routing_keys,

  	ToExchange = extract_header(Headers, <<"forward_exchange">>, XName),
  	
    Content1 = rabbit_basic:map_headers(fun(H)  -> 
    	{ _R,N } = gen_server:call( global:whereis_name(rabbit_stamp_worker), {next, XName#resource.name}),
		  lists:append( [{<<"stamp">>, long, N}], H)
    end, Content), 

  	{_Ok, Msg} = rabbit_basic:message({resource,<<"/">>, exchange, ToExchange}, RoutingKey, Content1),
	
    NewDelivery = build_delivery(Delivery, Msg),

   	%send message
	rabbit_basic:publish(NewDelivery),
	[].

% helpers
extract_header(Headers, Key, Default) ->
   case lists:keyfind(Key, 1, Headers) of
        false ->
            Default;
        {_,_,Header} ->
           Header
    end.

build_delivery(Delivery, Message) ->
    Mandatory = Delivery#delivery.mandatory,
    MsgSeqNo = Delivery#delivery.msg_seq_no,
    NewDelivery = rabbit_basic:delivery(Mandatory, Message, MsgSeqNo),
    NewDelivery.

% default callbacks - not implemented
validate(_X) -> ok.
create(_Tx, _X) -> ok.
delete(_Tx, _X, _Bs) -> ok.
add_binding(_Tx, _X, _B) -> ok.
remove_bindings(_Tx, _X, _Bs) -> ok.
assert_args_equivalence(X, Args) ->
  rabbit_exchange:assert_args_equivalence(X, Args).
policy_changed(_A,_B) -> ok.
validate_binding(_A,_B) -> ok.