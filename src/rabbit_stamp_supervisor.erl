-module(rabbit_stamp_supervisor).
-behaviour(supervisor).

-include_lib("amqp_client/include/amqp_client.hrl").

-export([start_link/0, init/1]).

-rabbit_boot_step({?MODULE,
  [{description, "stamp"},
 		{mfa,{ rabbit_sup, start_child, [?MODULE]}},
                    {requires,    kernel_ready},
                    {enables,     core_initialized }]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Arg = []).

init([]) ->
    {ok, {{one_for_one, 3, 10},
          [{rabbit_stamp_worker,
            {rabbit_stamp_worker, start_link, []},
            permanent,
            10000,
            worker,
            [rabbit_stamp_worker]}
          ]}}.
