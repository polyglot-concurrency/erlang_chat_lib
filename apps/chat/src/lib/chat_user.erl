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
-export([start/1, init/1, terminate/0]).

start(Node) ->
    spawn(Node, ?MODULE, init, [[]]).

init(Args) ->
    State = Args,
    loop(State).

loop(State) ->
    receive
        {msg_from_user, Msg, UserName} ->
            NewState = [{UserName, Msg} | State],
            loop(NewState);

        {get_messages, Sender} ->
            Sender ! lists:reverse(State),
            loop([]);

        stop ->
            terminate()
    end.

terminate() ->
    ok.
