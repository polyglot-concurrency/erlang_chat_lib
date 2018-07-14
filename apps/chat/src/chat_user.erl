%%%-------------------------------------------------------------------
%%% @author Albert Cruz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 0X. Jul 2018 XX:XX
%%%------------------------------------------------------------------
-module(chat_user).
-author("Albert Cruz").

%% API
-export([]).

-compile(export_all).

start() ->
    spawn(chat_user, init, [[]]).

init(Args) ->
    State = Args,
    loop(State).

loop(State) ->
    receive
        {msg_from_user, Msg, UserName} ->
            NewState = [{UserName, Msg} | State],
            loop(NewState);

        {get_messages, Sender} ->
            Sender ! State,
            loop([]);

        stop ->
            terminate()
    end.

terminate() ->
    ok.
