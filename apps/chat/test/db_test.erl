%%%-------------------------------------------------------------------
%%% @author Albert Cruz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 0X. Jul 2018 XX:XX
%%%------------------------------------------------------------------
-module(db_test).
-include_lib("eunit/include/eunit.hrl").

-author("Albert Cruz").

double_register_test_() ->
    {setup,
     fun start/0, % setup function
     fun stop/1, % cleanup function
     fun(SetupData) ->
             [
              user_do_not_exists(SetupData)
             ,register_one_user(SetupData)
             ,wrong_password(SetupData)
             ,geting_user(SetupData)
             ,register_one_user_twice(SetupData)
             ]
     end}. % instantiators

start() ->
    {ok, Pid} = db:start(),
    Pid.

stop(_) ->
    db:stop().

user_do_not_exists(_) ->
    Res = db:is_user("jhon", "pass"),
    [?_assertEqual({error, user_do_not_exists}, Res)].

register_one_user(_) ->
    Res = db:add_user("jhon", "pass"),
    [?_assertEqual(ok, Res)].

wrong_password(_) ->
    Res = db:is_user("jhon", "pass2"),
    [?_assertEqual({error, wrong_password}, Res)].

geting_user(_) ->
    Res = db:is_user("jhon", "pass"),
    [?_assertEqual(ok, Res)].

register_one_user_twice(_) ->
    Res = db:add_user("jhon", "pass2"),
    [?_assertEqual({error, user_exist}, Res)].
