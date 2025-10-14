-module(chat_registry).

-export([start_chat_registry/0]).
-import(mysql_handler, [start_db/0]).

start_chat_registry()->
  Pid = spawn(fun() -> registry_loop(#{}) end),
  io:format("[Chat Registry] -> Starting register at pid ~p~n",[Pid]),
  DBPid = spawn(fun() -> start_db() end),
  io:format("[Chat Registry] -> Starting Database Connection at pid ~p~n",[DBPid]),
  register(database_connection, DBPid),
  register(chat_registry, Pid).

handle_mysql(DestinationPid, Destination, Content, Sender, ChatID, Instant, Caller) ->
  DBPid = whereis(database_connection),
  DBPid ! {save_message, Content, Sender, ChatID, Instant, self()},
  receive
    {ok, _Message} ->
      if 
        DestinationPid =/= undefined -> 
          io:format("[Message Handler] -> forwarding message to ~p~n",[DestinationPid]),
          DestinationPid ! {forwarded_message, Content};
        true ->
          io:format("[Message Handler] -> user not in chat~n"),
          {ok, NotificationNode} = application:get_env(notification_node),
          ConnectedNodes = nodes(),
          if 
            ConnectedNodes == [] -> net_kernel:connect_node(NotificationNode);
            true -> ok
          end,
          {notification_registry, NotificationNode} ! {increase, Destination, Sender, ChatID}
      end;
    {sql_error, Reason} ->
      Caller ! {sql_error, Reason} 
  end.

registry_loop(Mappings) ->
  receive 
    {register, Username, Pid} ->
      io:format("[Chat Registry] -> adding user ~p with pid ~p~n",[Username, Pid]),
      NewMappings = maps:put(Username, Pid, Mappings),
      registry_loop(NewMappings);
    {forward, Destination, Content, ChatID, Instant, Sender, Caller} ->
      DestinationPid = maps:get(Destination, Mappings, undefined),
      spawn(fun() -> handle_mysql(
        DestinationPid, 
        Destination, 
        Content, 
        Sender, 
        ChatID, 
        Instant, 
        Caller
      ) end),
      registry_loop(Mappings);
    {unregister, Username} ->
      io:format("[Chat Registry] -> removing user ~p from registry~n", [Username]),
      NewMappings = maps:remove(Username, Mappings),
      registry_loop(NewMappings)
    end.