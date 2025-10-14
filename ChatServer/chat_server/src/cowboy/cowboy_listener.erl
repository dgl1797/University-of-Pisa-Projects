-module(cowboy_listener).
-behaviour(gen_server).

%% API
-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-import(chat_registry, [start_chat_registry/0]).

start_link() ->
  io:format("[Cowboy] -> starting new server~n"),
  start_chat_registry(),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

routes() ->
  {ok, ChatEndpoint} = application:get_env(chat_endpoint),
  Routes = [
    {ChatEndpoint, socket_listener, []}
  ],
  {Routes}.

init(_) ->
  {ok, Port} = application:get_env(port),
  {Routes} = routes(),

  Dispatcher = cowboy_router:compile([{'_', Routes}]),

  {ok, Pid} = cowboy:start_clear(cowboy_routes, [{port, Port}], #{env => #{dispatch => Dispatcher}}),
  io:format("[Cowboy] -> new cowboy listener initialized with pid ~p at port ~p~n", [Pid, Port]),
  {ok, []}.

handle_call(Req, _, State) ->
  {reply, Req, State}.

handle_cast(_, State) ->
  {noreply, State}.