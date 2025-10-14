-module(mysql_handler).

-export([start_mysql/0]).

run_loop(Conn) ->
  receive
    {save_notification, User, Sender, ChatID} ->
      io:format("[Notification MySQL] -> Saving tuple (~p, ~p, ~p)~n",[User,Sender,ChatID]),
      Statement = "INSERT INTO notification(user, sender, chatID) VALUES (?, ?, ?)",
      case mysql:prepare(Conn, Statement) of
        {ok, StatementID} ->
          mysql:execute(Conn, StatementID, [User, Sender, ChatID]);
        {error, Reason} ->
          io:format("[Notification MySQL] -> failed to save: ~p~n",[Reason]),
          ok
      end
  end,
  run_loop(Conn).

start_mysql() ->
  {ok, DBConfig} = application:get_env(db_config),
  {ok, Conn} = mysql:start_link(DBConfig),
  run_loop(Conn).
