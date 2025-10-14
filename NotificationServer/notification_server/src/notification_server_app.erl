%%%-------------------------------------------------------------------
%% @doc notification_server public API
%% @end
%%%-------------------------------------------------------------------

-module(notification_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("[Notification Server] -> starting supervisor~n"),
    notification_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
