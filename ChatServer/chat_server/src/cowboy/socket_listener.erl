-module(socket_listener).

%%API
-export([init/2, websocket_handle/2, websocket_info/2, terminate/3, websocket_init/1]).

% called when cowboy receives a request
init(Req, _State)->
  % handing the websocket to cowboy_websocket module passing it the request using infinite idle timeout option
  #{username:=CurrentUsername} = cowboy_req:match_qs([{username, nonempty}], Req),
  RegisterPid = whereis(chat_registry),
  InitialState = #{username => CurrentUsername, register_pid => RegisterPid},
  {cowboy_websocket, Req, InitialState, #{idle_timeout => infinity}}.

% stores the Username to Pid mapping in the registry
websocket_init(State)->
  #{username := CurrentUsername, register_pid := RegisterPid} = State,
  RegisterPid ! {register, CurrentUsername, self()},
  {ok, State}.

% override of the cowboy_websocket websocket_handle/2 method
websocket_handle(_Frame={text, Message}, State) ->
  DecodedMessage = jsone:try_decode(Message),
  Reply = case DecodedMessage of
    {ok, MessageMap, _} ->
      io:format("[Chat WS:~p] -> Received frame: ~p~n",[self(), MessageMap]),

      % body unpack
      Destination = maps:get(<<"username">>, MessageMap, undefined),
      ChatID = maps:get(<<"chatID">>, MessageMap, undefined),
      Instant = maps:get(<<"timestampMillis">>, MessageMap, undefined),
      Content = maps:get(<<"message">>, MessageMap, ""),

      % limit case handling
      if 
        Destination == undefined orelse ChatID == undefined orelse Instant == undefined -> 
          JsonResponse = jsone:encode(#{<<"type">> => <<"error">>, <<"messageID">> => Instant}),
          {reply, {text, JsonResponse}, State};
        true -> 
          % correct format
          #{username := Sender, register_pid := RegisterPid} = State,
          RegisterPid ! {forward, Destination, Content, ChatID, Instant, Sender, self()},
          {ok, State}
      end;
    _ -> 
      JsonResponse = jsone:encode(#{<<"type">> => <<"error">>, <<"reason">> => <<"invalid payload">>}),
      {reply, {text, JsonResponse}, State}
  end,
  Reply.

% called when cowboy receives an Erlang message  
% (=> from another Erlang process).
websocket_info(Info, State) ->
  case Info of
    {forwarded_message, ReceivedMessage} ->
      io:format("[Chat WS:~p] -> Received forwarded message: ~p~n",[self(), ReceivedMessage]),
      Json = jsone:encode(#{<<"type">> => <<"message">>, <<"content">> => ReceivedMessage}),
      {reply, {text, Json}, State};
    {sql_error, CreationTime} ->
      io:format("[Chat WS:~p] -> Received error from SQL~n",[self()]),
      Json = jsone:encode(#{<<"type">> => <<"error">>, <<"messageID">> => CreationTime}),
      {reply, {text, Json}, State};
    _ ->
      {ok, State}
  end.

% called when connection terminate
terminate(Reason, _Req, State) ->
  io:format("[Chat WS:~p] -> Closed websocket connection, Reason: ~p ~n", [self(), Reason]),
  #{username := Username, register_pid := RegisterPid} = State,
  RegisterPid ! {unregister, Username},
  {ok, State}.