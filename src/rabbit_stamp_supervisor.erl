-module(rabbit_stamp_supervisor).
-behaviour(supervisor).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).

init([]) ->

    GlobalWorker = {rabbit_stamp_worker, 
          {rabbit_stamp_worker, start_link, []}, 
          permanent , 1000, worker, []},

    Monitor = {rabbit_stamp_monitor, 
          {rabbit_stamp_monitor, start_monitor, []}, 
          permanent , 1000, worker, []},

    {ok, {{one_for_one, 3, 10},[GlobalWorker, Monitor]}}.
