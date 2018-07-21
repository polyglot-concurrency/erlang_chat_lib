%%%-------------------------------------------------------------------
%%% @author Albert Cruz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 0X. Jul 2018 XX:XX
%%%------------------------------------------------------------------
-module(chat_server).
-author("Albert Cruz").

%%%===================================================================
%%% API
%%%===================================================================

-export([start/0, init/0, stop/0]).

start() ->
    register(?MODULE, spawn(?MODULE, init, [])).

init() ->
    loop().

stop()->
    ?MODULE ! stop.

loop() ->
    receive
        {login, Name, Password, SenderNode, Sender} ->
            R = db:is_user(Name, Password),
            Sender ! case R of
                         ok ->
                             UL = db:is_user_loged(Name),
                             case UL of
                                 true -> {error, user_already_loged};
                                 false ->
                                     NUser = chat_user:start(SenderNode),
                                     Add_loged_user_response = db:add_loged_user(Name, NUser),
                                     case Add_loged_user_response of
                                         ok -> {ok, NUser};
                                         _ -> Add_loged_user_response
                                     end
                             end;
                         _ -> R
                     end,
            loop();

        {create_user, Name, Password, Sender} ->
            Sender ! db:add_user(Name, Password),
            loop();

        {logout, LogedUserPid, Sender} ->
            Sender ! db:remove_loged_user(LogedUserPid),
            loop();

        {get_all_users_names, Sender} ->
            Sender ! db:get_all_users_names(),
            loop();

        {create_group, LogedUserPid, GroupName, Sender} ->
            R = db:is_user_pid_loged(LogedUserPid),
            Sender ! case R of
                         ok -> {ok, UserName}=db:user_pid_to_user_name(LogedUserPid),
                               db:add_group(GroupName, UserName);
                         E -> E
                     end,
            loop();

        {add_user_to_group, LogedUserPid, GroupName, Sender} ->
            R = db:is_user_pid_loged(LogedUserPid),
            Sender ! case R of
                         ok -> {ok, UserName}=db:user_pid_to_user_name(LogedUserPid),
                               db:add_user_to_group(GroupName, UserName);
                         E -> E
                     end,
            loop();

        {send_msg, LogedUserPid, To, Msg} ->
            db:send_msg(LogedUserPid, To, Msg),
            loop();

        {get_msgs, LogedUserPid, Sender} ->
            R = db:is_user_pid_loged(LogedUserPid),
            Sender ! case R of
                         ok -> db:get_msgs(LogedUserPid);
                         E -> E
                     end,
            loop();

        {send_msg_to_group, LogedUserPid, GroupName, Msg} ->
            R = db:is_user_pid_loged(LogedUserPid),
            case R of
                ok -> db:send_msg_to_group(LogedUserPid, GroupName, Msg);
                E -> E
            end,
            loop();

        stop ->
            terminate()
    end.

terminate() ->
    ok.
