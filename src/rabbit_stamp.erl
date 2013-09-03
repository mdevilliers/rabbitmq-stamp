-module(rabbit_stamp).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, []) ->
    rabbit_stamp_supervisor:start_link().

stop(_State) ->
    ok.
