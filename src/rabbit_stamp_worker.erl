-module(rabbit_stamp_worker).
-behaviour(gen_server).

-export([start_link/0, next/3]).
-export([get_next_number/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").

start_link() ->
    %rabbit_log:info("rabbit_stamp_worker : starting...~n"),
    case gen_server:start_link({global, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} -> 
            %rabbit_log:info("rabbit_stamp_worker : started locally on ~p~n", [node()]),
            {ok, Pid};
        {error, {already_started, Pid}} -> 
            %rabbit_log:info("rabbit_stamp_worker : already started on ~p node ~p ~n", [Pid, node(Pid)]), 
            {ok, Pid};
        Else -> Else
    end.

next(<<ExchangeName/binary>>, VirtualHost, Message) ->
    ExchangeName0 = list_to_atom( binary_to_list( ExchangeName ) ),
    gen_server:cast( find_worker() , {next, ExchangeName0, VirtualHost, Message});
next(ExchangeName, VirtualHost, Message) when is_atom(ExchangeName) ->
    gen_server:cast( find_worker() , {next, ExchangeName, VirtualHost, Message}).

init([]) ->
    {ok, []}.

handle_call(_, _ , State) ->
      {reply, ok, State}.

handle_cast({next, ExchangeName, VirtualHost, Message}, State) ->

    {ok, NextCount, NewState} = get_next_number(ExchangeName, State),
    
    BasicMessage = Message#delivery.message,
    Content = BasicMessage#basic_message.content,
    Headers = rabbit_basic:extract_headers(Content),
    [RoutingKey|_] = BasicMessage#basic_message.routing_keys,

    ToExchange = extract_header(Headers, <<"forward_exchange">>, ExchangeName),

    Content1 = rabbit_basic:map_headers(fun(H)  ->   
        lists:append( [{<<"stamp">>, long, NextCount}], H)
    end, Content), 

    {ok, Msg} = rabbit_basic:message({resource, VirtualHost, exchange, ToExchange}, RoutingKey, Content1),

    NewDelivery = build_delivery(Message, Msg),
    rabbit_basic:publish(NewDelivery),
    {noreply, NewState}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _State) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

% helpers
get_next_number(ExchangeName, State) ->
    case proplists:get_value(ExchangeName, State, unknown) of
        unknown -> 
            CurrentCount = get_timestamp();
        {LocalCount} ->
            CurrentCount = LocalCount
    end,

    NextCount = CurrentCount + 1,
    NewState0 = proplists:delete(ExchangeName, State),
    {ok, NextCount, [{ExchangeName, {NextCount}}| NewState0]}.

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
    DoConfirm = Delivery#delivery.confirm,
    rabbit_basic:delivery(Mandatory, DoConfirm, Message, MsgSeqNo).

get_timestamp() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.

find_worker() ->
    global:whereis_name(rabbit_stamp_worker).