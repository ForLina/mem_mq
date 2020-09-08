%%%-------------------------------------------------------------------
%% @doc mem_mq public API
%% @end
%%%-------------------------------------------------------------------

-module(mem_mq_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mem_mq_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
