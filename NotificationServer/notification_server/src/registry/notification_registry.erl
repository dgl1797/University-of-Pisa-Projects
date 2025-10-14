-module(notification_registry).

-export([start_notification_registry/0]).
-import(mysql_handler, [start_mysql/0]).

start_notification_registry()->
  Pid = spawn(fun() -> registry_loop(#{}) end),
  io:format("[Notification Registry] -> Starting register at pid ~p~n",[Pid]),
  DBPid = spawn(fun() -> start_mysql() end),
  io:format("[Notification Registry] -> Starting mysql handler at pid ~p~n",[DBPid]),
  register(mysql_connection, DBPid),
  register(notification_registry, Pid).

send_online(_Key, Value, Who) ->
  Value ! {online, Who}.
send_offline(_Key, Value, Who) ->
  Value ! {offline, Who}.

registry_loop(Mappings) ->
  receive 
    {register, Username, Pid} ->
      io:format("[Notification Registry] -> adding user ~p with pid ~p~n",[Username, Pid]),
      case maps:get(Username, Mappings, undefined) of 
        undefined -> maps:foreach(fun(Key, Value) -> send_online(Key,Value,Username) end, Mappings);
        _ -> ok
      end,
      NewMappings = maps:put(Username, Pid, Mappings),
      registry_loop(NewMappings);
    {increase, Username, Sender, ChatID} ->
      DBPid = whereis(mysql_connection),
      DBPid ! {save_notification, Username, Sender, ChatID},
      case maps:get(Username, Mappings, undefined) of 
        Pid when Pid =/= undefined-> 
          io:format("[Notification Registry] -> Pushing new notification to ~p by ~p~n",[Username, Sender]),
          Pid ! {increase, Sender};
        undefined ->
          ok
      end,
      registry_loop(Mappings);
    {delete_chat, Who, Sender} ->
      case maps:get(Who, Mappings, undefined) of
        Pid when Pid =/= undefined ->
          io:format("[Notification Registry] -> Forwarding chat deletion message to ~p~n",[Who]),
          Pid ! {delete_chat, Sender}
      end,
      registry_loop(Mappings);
    {unregister, Username} ->
      io:format("[Notification Registry] -> user logged out, notifying active users~n"),
      NewMappings = maps:remove(Username, Mappings),
      maps:foreach(fun(Key, Value) -> send_offline(Key,Value,Username) end, Mappings),
      registry_loop(NewMappings)
    end.