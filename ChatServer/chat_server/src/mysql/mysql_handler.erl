-module(mysql_handler).

% APIs
-export([start_db/0]).

run_loop(Conn) ->
  receive
    {save_message, Content, Sender, ChatID, CreationTime, Caller} ->
      io:format("[Chat MySQL] -> Saving tuple (~p, ~p, ~p, ~p)~n",[Content,Sender,ChatID,CreationTime]),
      Statement = "INSERT INTO message(content, sender, chatID, creationTime) VALUES (?, ?, ?, FROM_UNIXTIME(?*0.001))",
      case mysql:prepare(Conn, Statement) of
        {ok, StatementID} ->
          case mysql:execute(Conn,StatementID,[Content, Sender, ChatID, CreationTime]) of
            {error, Reason} ->
              io:format("[Chat MySQL] -> Failed to save: ~p~n", [Reason]),
              Caller ! {sql_error, CreationTime};
            Result ->
              io:format("[Chat MySQL] -> ~p~n",[Result]),
              Caller ! {ok, <<"saved">>}
          end;
        {error, Reason} -> 
          io:format("[Chat MySQL] -> Failed to save: ~p~n",[Reason]),
          Caller ! {sql_error, CreationTime}
      end
  end,
  run_loop(Conn).

start_db() ->
  {ok, DBConfig} = application:get_env(db_config),
  {ok, Conn} = mysql:start_link(DBConfig),
  run_loop(Conn).
