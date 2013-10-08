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
  rabbit_stamp_worker:next( XName#resource.name, XName#resource.virtual_host , Delivery),
	[].

% default callbacks
validate(_X) -> ok.
create(_Tx, _X) -> ok.
delete(_Tx, _X, _Bs) -> ok.
add_binding(_Tx, _X, _B) -> ok.
remove_bindings(_Tx, _X, _Bs) -> ok.
assert_args_equivalence(X, Args) ->
  rabbit_exchange:assert_args_equivalence(X, Args).
policy_changed(_A,_B) -> ok.
validate_binding(_A,_B) -> ok.