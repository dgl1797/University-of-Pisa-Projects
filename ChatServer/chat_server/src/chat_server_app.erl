%%%-------------------------------------------------------------------
%% @doc chat_server public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[Chat Server] -> starting supervisor~n"),
    chat_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
