%%%-------------------------------------------------------------------
%%% @author Albert Cruz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 0X. Jul 2018 XX:XX
%%%------------------------------------------------------------------
-module(domain).
-author("Albert Cruz").

%% API
-export([]).

-compile(export_all).

-spec create_user(string(), string()) -> ok | {error, _}.
create_user(Name, Password) ->
    db:add_user(Name, Password).

-spec login(string(), string()) -> {ok, pid()} | {error, _}.
login(Name, Password) ->
    R = db:is_user(Name, Password),
    case R of
        ok ->
            UL = db:is_user_loged(Name),
            case UL of
                true -> {error, user_already_loged};
                false ->
                    NUser = chat_user:start(),
                    Add_loged_user_response = db:add_loged_user(Name, NUser),
                    case Add_loged_user_response of
                        ok -> {ok, NUser};
                        _ -> Add_loged_user_response
                    end
            end;
        _ -> R
    end.

-spec create_group(pid(), string()) -> ok | {error, _}.
create_group(LogedUserPid, GroupName) ->
    R = db:is_user_pid_loged(LogedUserPid),
    case R of
        ok -> {ok, UserName}=db:user_pid_to_user_name(LogedUserPid),
              db:add_group(GroupName, UserName);
        E -> E
    end.

-spec add_user_to_group(pid(), string()) -> ok | {error, _}.
add_user_to_group(LogedUserPid, GroupName) ->
    R = db:is_user_pid_loged(LogedUserPid),
    case R of
        ok -> {ok, UserName}=db:user_pid_to_user_name(LogedUserPid),
              db:add_user_to_group(GroupName, UserName);
        E -> E
    end.

-spec send_msg(pid(), string(), string()) -> ok | {error, _}.
send_msg(LogedUserPid, To, Msg) ->
    db:send_msg(LogedUserPid, To, Msg).

get_msgs(LogedUserPid) ->
    R = db:is_user_pid_loged(LogedUserPid),
    case R of
        ok -> db:get_msgs(LogedUserPid);
        E -> E
    end.

-spec send_msg_to_group(pid(), string(), string()) -> ok | {error, _}.
send_msg_to_group(LogedUserPid, GroupName, Msg) ->
    R = db:is_user_pid_loged(LogedUserPid),
    case R of
        ok -> db:send_msg_to_group(LogedUserPid, GroupName, Msg);
        E -> E
    end.

