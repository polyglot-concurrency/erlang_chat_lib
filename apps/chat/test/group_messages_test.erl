%%%-------------------------------------------------------------------
%%% @author Albert Cruz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 0X. Jul 2018 XX:XX
%%%------------------------------------------------------------------
-module(group_messages_test).
-include_lib("eunit/include/eunit.hrl").

-author("Albert Cruz").

group_messages_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(SetupData) ->
             [
              group_creation(SetupData)
             , group_creation_twice(SetupData)
             , send_message_to_group(SetupData)

             ]
     end}.

start() ->
    {ok, Pid} = db:start(),

    db:add_user("jhon", "aaaa"),
    db:add_user("ana", "bbbb"),
    db:add_user("mike", "cccc"),
    db:add_user("will", "dddd"),

    db:add_group("group2", "ana"),
    db:add_user_to_group("group2", "mike"),
    db:add_user_to_group("group2", "jhon"),
    db:add_user_to_group("group2", "will"),

    chat_server:start(),
    chat_client:start(node()),

    Pid.

stop(_) ->
    chat_server:stop(),
    chat_client:stop(),
    db:stop().

group_creation(_) ->
    Res = db:add_group("group1", "jhon"),
    [?_assertEqual(ok, Res)].

group_creation_twice(_) ->
    Res = db:add_group("group1", "jhon"),
    [?_assertEqual({error, group_exist}, Res)].

send_message_to_group(_) ->
    {_, Pana} = chat_client:login("ana", "bbbb"),

    {_, Pmike} = chat_client:login("mike", "cccc"),
    {_, Pjhon} = chat_client:login("jhon", "aaaa"),

    chat_client:send_msg_to_group(Pana, "group2", "hello mike"),
    chat_client:send_msg_to_group(Pana, "group2", "hello again"),
    chat_client:send_msg_to_group(Pjhon, "group2", "hello"),
    chat_client:send_msg(Pjhon, "will", "what's up?"),

    {_, Pwill} = chat_client:login("will", "dddd"),

    {ok, Msgs} = chat_client:get_msgs(Pmike),
    {ok, MsgsPjhon} = chat_client:get_msgs(Pjhon),
    {ok, MsgsPana} = chat_client:get_msgs(Pana),
    {ok, MsgsPwill} = chat_client:get_msgs(Pwill),

    [
     ?_assertEqual([
                    {"group2:ana", "hello mike"}
                   , {"group2:ana", "hello again"}
                   , {"group2:jhon", "hello"}
                   ], Msgs)
    ,?_assertEqual([
                    {"group2:ana", "hello mike"}
                   , {"group2:ana", "hello again"}
                   ], MsgsPjhon)
    ,?_assertEqual([
                    {"group2:jhon", "hello"}
                   ], MsgsPana)
    ,?_assertEqual([
                    {"group2:ana", "hello mike"}
                   , {"group2:ana", "hello again"}
                   , {"group2:jhon", "hello"}
                   , {"jhon", "what's up?"}
                   ], MsgsPwill)

    ].
