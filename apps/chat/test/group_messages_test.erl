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
    db:add_group("group2", "ana"),
    db:add_user_to_group("group2", "mike"),
    db:add_user_to_group("group2", "jhon"),
    Pid.

stop(_) ->
    db:stop().

group_creation(_) ->
    Res = db:add_group("group1", "jhon"),
    [?_assertEqual(ok, Res)].

group_creation_twice(_) ->
    Res = db:add_group("group1", "jhon"),
    [?_assertEqual({error, group_exist}, Res)].

send_message_to_group(_) ->
    {_, Pana} = domain:login("ana", "bbbb"),
    {_, Pmike} = domain:login("mike", "cccc"),
    {_, Pjhon} = domain:login("jhon", "aaaa"),

    domain:send_msg_to_group(Pana, "group2", "hello mike"),
    domain:send_msg_to_group(Pana, "group2", "hello again"),
    domain:send_msg_to_group(Pjhon, "group2", "hello"),

    {ok, Msgs} = domain:get_msgs(Pmike),
    {ok, MsgsPjhon} = domain:get_msgs(Pjhon),
    {ok, MsgsPana} = domain:get_msgs(Pana),

    [?_assertEqual([
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
    ].

