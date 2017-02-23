-module(malebot).

-export([start/0]).

-import(bsearch,[search_word/2,get_word_list/0]).


-include("token.hrl").
-define(BASE_URL, "https://api.telegram.org/bot" ++ ?TOKEN).
-define(GET_COMMAND_URL, ?BASE_URL ++ "/getUpdates?offset=").
-define(SET_COMMAND_URL, ?BASE_URL ++ "/sendMessage").

start() ->
  io:format("---Start bot---~n"),
  inets:start(),
  ssl:start(),

  Words = get_word_list(),

  command_handler(?GET_COMMAND_URL, 0, Words).


command_handler(Url, UpdateId, Words) ->
  Response = parse_response(get_command(Url ++ integer_to_list(UpdateId + 1))),
  {JsonObj} = jiffy:decode(Response),
  Result = proplists:get_value(<<"result">>, JsonObj, []),
  Message = case Result of
    [ {[{<<"update_id">>, NewUpdateId}, {<<"message">>, {_Message}} |_]} |_] ->
      _Message;
    [ {[{<<"update_id">>, NewUpdateId}, {<<"edited_message">>, {_Message}} |_]} |_] ->
      _Message;
    [ {[{<<"update_id">>, NewUpdateId} |_]} |_] ->
      notxt;
    [] ->
      NewUpdateId = UpdateId,
      notxt
  end,
  case parse_message(Message) of
    {command, ChatID, _, Msg_str} ->
      run_command(ChatID, Msg_str);
    {text, ChatID, MsgID, Msg_str} ->
      check_badword(ChatID, MsgID, Msg_str, Words);
      notxt -> ok
  end,
  timer:sleep(1000),
  command_handler(Url, NewUpdateId, Words).

send_message(ChatID, Text) ->
  set_command(?SET_COMMAND_URL, "chat_id=" ++ integer_to_list(ChatID) ++ "&text=" ++ Text).

send_message(ChatID, MsgID, Text) ->
  set_command(?SET_COMMAND_URL, "chat_id=" ++ integer_to_list(ChatID) ++ "&text=" ++ Text ++ "&reply_to_message_id=" ++ integer_to_list(MsgID)).

get_command(Url) ->
  request(get, {Url, []}).

set_command(Url, Data) ->
  Response = request(post, {Url, [], "application/x-www-form-urlencoded", Data}),
  {ok, {{"HTTP/1.1",ReturnCode, State}, _, _}} = Response,
  io:format("~w / ~p~n", [ReturnCode, State]).

request(Method, Body) ->
  httpc:request(Method, Body, [{ssl,[{verify,0}]}], []).

parse_response({ok, { _, _, Body}}) -> Body.

parse_message(notxt) -> notxt;

parse_message(Message) ->
  {Chat} = proplists:get_value(<<"chat">>, Message),
  ChatID = proplists:get_value(<<"id">>, Chat),
  Command = proplists:get_value(<<"text">>, Message),
  MsgID = proplists:get_value(<<"message_id">>, Message),
  case Command of
    undefined -> notxt;
    _ -> Msg_str = binary_to_list(Command),
      case Msg_str of
        [47|_] -> {command, ChatID, MsgID, Msg_str};
        _  -> {text, ChatID, MsgID, Msg_str}
      end
  end.


terminate() ->
  ssl:stop(),
  inets:stop().

run_command(ChatID, "/help") ->
  send_message(ChatID, "Help text");

run_command(_, _ ) -> ok.

check_badword(ChatID, MsgID, Message, Words) ->
  N1 = os:timestamp(),
  Msg_lower = string:to_lower(Message),
  Msg_clean = re:replace(Msg_lower, "[^A-Za-z \n]", "", [global, {return, list}]),
  List = string:tokens(Msg_clean, " \n"),

  case check_badword_rec(List, Words) of
    {bad, Word} ->
      send_message(ChatID, MsgID, "Modera il linguaggio! '"++ Word++"' non si dice!");
    nobad -> nobad
  end,
  N2 = os:timestamp(),
  Time = timer:now_diff(N2,N1)/1000000,
  io:format("check message in ~w~n", [Time]).


check_badword_rec([H|[]], Words) ->
  case search_word(H,Words) of
    true ->	{bad, H};
    false -> nobad
  end;

check_badword_rec([H1|T], Words) ->
  case check_badword_rec([H1], Words) of
    nobad -> [H2|_] = T,
      case search_word(H1++H2,Words) of
        true ->	{bad, H1 ++ " " ++ H2};
        false -> check_badword_rec(T, Words)
      end;
    A -> A
  end;

check_badword_rec([], _) -> nobad.
