%%%-------------------------------------------------------------------
%%% @author Albert Cruz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 0X. Jul 2018 XX:XX
%%%------------------------------------------------------------------
-module(login_and_simple_messages_test).
-include_lib("eunit/include/eunit.hrl").

-author("Albert Cruz").

login_and_simple_messages_test_() ->
    {setup,
     fun start/0,
     fun stop/1,
     fun(SetupData) ->
             [
              login_logout_user(SetupData)
             , login_user_twice(SetupData)
             , user_name_to_user_pid(SetupData)
             , send_message_to_user(SetupData)

             ]
     end}.

start() ->
    {ok, Pid} = db:start(),
    db:add_user("jhon", "aaaa"),
    db:add_user("ana", "bbbb"),
    db:add_user("mike", "cccc"),
    Pid.

stop(_) ->
    db:stop().

login_logout_user(_) ->
    {Res, Pj} = domain:login("jhon", "aaaa"),
    R2 = domain:logout(Pj),
    {Res2, _} = domain:login("jhon", "aaaa"),
    [?_assertEqual(ok, Res),?_assertEqual(ok, R2),?_assertEqual(ok, Res2) ].

login_user_twice(_) ->
    Res = domain:login("jhon", "aaaa"),
    [?_assertEqual({error, user_already_loged}, Res)].

user_name_to_user_pid(_) ->
    {ok, P} = db:user_name_to_user_pid("jhon"),
    Res = db:user_pid_to_user_name(P),
    [?_assertEqual({ok, "jhon"}, Res)].

send_message_to_user(_) ->
    {_, Pana} = domain:login("ana", "bbbb"),
    {_, Pmike} = domain:login("mike", "cccc"),
    domain:send_msg(Pana, "mike", "hello mike"),
    domain:send_msg(Pana, "mike", "hello again"),
    {ok, Msgs} = domain:get_msgs(Pmike),
    [?_assertEqual([
                    {"ana", "hello mike"}
                   , {"ana", "hello again"}
                   ], Msgs)].

