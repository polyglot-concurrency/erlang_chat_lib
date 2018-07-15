%%%-------------------------------------------------------------------
%%% @author Albert Cruz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 0X. Jul 2018 XX:XX
%%%-------------------------------------------------------------------
-module(db).
-behaviour(gen_server).

-author("Albert Cruz").

-record(models,
        {users = #{}, current_loged_users = #{},
         groups = #{}, messages_not_delivered = []}
       ).


%% API
%% -export([start/0, stop/0]).

-compile(export_all).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #models{}}.

stop() ->
    gen_server:cast(?MODULE, stop).

-spec add_user(string(), string()) -> ok | {error, _}.
add_user(Name, Pass) ->
    gen_server:call(?MODULE, {{add_user, Name, Pass}, self()}).

add_group(GroupName, UserName) ->
    gen_server:call(?MODULE, {{add_group, GroupName, UserName}, self()}).

add_user_to_group(GroupName, UserName) ->
    gen_server:call(?MODULE, {{add_user_to_group, GroupName, UserName}, self()}).

add_loged_user(Name, UserPid) ->
    R = gen_server:call(?MODULE, {{add_loged_user, Name, UserPid}, self()}),
    gen_server:cast(?MODULE, {notify_loged_user, Name}),
    R.

get_loged_users(LogedUserPid) ->
    gen_server:call(?MODULE, {{get_loged_users, LogedUserPid}, self()}).

pget_loged_users() ->
    gen_server:call(?MODULE, {pget_loged_users, self()}).

-spec is_user(string(), string()) -> ok | {error, _}.
is_user(Name, Password) ->
    gen_server:call(?MODULE, {{is_user, Name, Password}, self()}).

is_user_loged(Name) ->
    {ok, All} = db:pget_loged_users(),
    lists:member(Name, All).

is_user_loged(Name, Models) ->
    All =pget_loged_users_(Models),
    lists:member(Name, All).

is_user_pid_loged(UserPid) ->
    gen_server:call(?MODULE, {{is_user_pid_loged, UserPid}, self()}).

user_name_to_user_pid(Name) ->
    gen_server:call(?MODULE, {{user_name_to_user_pid, Name}, self()}).

user_pid_to_user_name(LogedUserPid) ->
    gen_server:call(?MODULE, {{user_pid_to_user_name, LogedUserPid}, self()}).

get_msgs(LogedUserPid) ->
    gen_server:call(?MODULE, {{get_msgs, LogedUserPid}, self()}).

-spec send_msg(pid(), string(), string()) -> ok | {error, _}.
send_msg(LogedUserPid, To, Msg) ->
    gen_server:cast(?MODULE, {send_msg, LogedUserPid, To, Msg}).

send_msg_to_group(LogedUserPid, GroupName, Msg) ->
    gen_server:cast(?MODULE, {send_msg_to_group, LogedUserPid, GroupName, Msg}).

handle_call({{add_user, Name, Pass}, Pid}, _From, Models) ->
    T = Models#models.users,
    case maps:is_key(Name, T) of
        true -> {reply, {error, user_exist}, Models};
        false -> {reply, ok, Models#models{users = T#{Name => Pass}}}
    end;

handle_call({{add_group, GroupName, UserName}, Pid}, _From, Models) ->
    T = Models#models.groups,
    case maps:is_key(GroupName, T) of
        true -> {reply, {error, group_exist}, Models};
        false -> {reply, ok, Models#models{groups = T#{GroupName => [UserName]}}}
    end;

handle_call({{add_user_to_group, GroupName, UserName}, Pid}, _From, Models) ->
    T = Models#models.groups,
    case maps:is_key(GroupName, T) of
        true ->
            {reply, ok, Models#models{groups = T#{GroupName => [UserName | maps:get(GroupName, T)]}}};
        false ->
            {reply, {error, group_do_not_exist}, Models}

    end;

handle_call({{add_loged_user, Name, UserPid}, Pid}, _From, Models) ->
    T = Models#models.current_loged_users,
    case maps:is_key(Name, T) of
        true -> {reply, {error, user_already_loged}, Models};
        false -> {reply, ok, Models#models{current_loged_users = T#{Name => UserPid}}}
    end;

handle_call({pget_loged_users, Pid}, _From, Models) ->
    {reply, {ok, pget_loged_users_(Models)}, Models};

handle_call({{get_loged_users, LogedUserPid}, Pid}, _From, Models) ->
    T = Models#models.current_loged_users,
    E = maps:to_list(T),
    R = lists:keyfind(LogedUserPid, 2, E),
    case R of
        false -> {reply, {error, user_is_not_loged}, Models};
        _ ->
            AllUsers = lists:filter(fun({Name, UserPid}) -> LogedUserPid =/= UserPid end, E),
            {reply, {ok, lists:map(fun({Name, _}) -> Name end, AllUsers)}, Models}
    end;

handle_call({{is_user, Name, Password}, Pid}, _From, Models) ->
    T = Models#models.users,
    case maps:is_key(Name, T) of
        true ->
            X = maps:get(Name, T),
            case X =:= Password of
                true -> {reply, ok, Models};
                false -> {reply, {error, wrong_password}, Models}
            end;
        false -> {reply, {error, user_do_not_exists}, Models}
    end;

handle_call({{user_pid_to_user_name, LogedUserPid}, Pid}, _From, Models) ->
    {reply, user_pid_to_user_name_(LogedUserPid, Models), Models};

handle_call({{get_msgs, LogedUserPid}, Pid}, _From, Models) ->
    LogedUserPid ! {get_messages, self()},
    {reply, {ok, receive
                     Response -> Response
                 end}, Models};

handle_call({{is_user_pid_loged, UserPid}, Pid}, _From, Models) ->
    R = user_pid_to_user_name_(UserPid, Models),
    case R of
        {ok, _} -> {reply, ok, Models};
        E -> {reply, E, Models}
    end;

handle_call({{user_name_to_user_pid, Name}, Pid}, _From, Models) ->
    {reply, user_name_to_user_pid_(Name, Models), Models}.

%% Utility functions

user_pid_to_user_name_(LogedUserPid, Models) ->
    T = Models#models.current_loged_users,
    E = maps:to_list(T),
    R = lists:keyfind(LogedUserPid, 2, E),
    case R of
        false -> {error, user_is_not_loged};
        {Name, _} -> {ok, Name}
    end.

user_name_to_user_pid_(Name, Models) ->
    T = Models#models.current_loged_users,
    Res = maps:get(Name, T, default),
    case Res of
        default ->
            {error, user_is_not_loged};
        Key -> {ok, Key}
    end.

pget_loged_users_(Models)->
    T = Models#models.current_loged_users,
    E = maps:to_list(T),
    lists:map(fun({Name, _}) -> Name end, E).

handle_cast({send_msg, UserPid, ToName, Msg}, Models) ->
    %% TODO: proper validation
    {ok, To} = user_name_to_user_pid_(ToName, Models),
    {ok, UserName} = user_pid_to_user_name_(UserPid, Models),
    To ! {msg_from_user, Msg, UserName},
    {noreply, Models};

handle_cast({send_msg_to_group, UserPid, GroupName, Msg}, Models) ->
    T = Models#models.groups,
    case maps:is_key(GroupName, T) of
        true ->

            UsersNamesInGroup = maps:get(GroupName, T),

            {ActiveUsers, InActiveUsers} = lists:partition(fun(N)-> is_user_loged(N, Models) end, UsersNamesInGroup),

            LogedUsersPids = lists:map(fun(UName) ->
                                               {ok, To} = user_name_to_user_pid_(UName, Models),
                                               To end, ActiveUsers),

            {ok, UserName} = user_pid_to_user_name_(UserPid, Models),
            LogedUsersPidsClean = lists:filter(fun (To) -> To =/= UserPid end, LogedUsersPids),
            lists:foreach(fun(To) ->
                                  To ! {msg_from_user, Msg, GroupName ++ ":" ++ UserName}
                          end, LogedUsersPidsClean),

            T1 = Models#models.messages_not_delivered,
            NewMessages = lists:map(fun(ToName)-> {ToName, GroupName ++ ":" ++ UserName, Msg}  end, InActiveUsers),
            {noreply, Models#models{messages_not_delivered = T1 ++ NewMessages}};
        false -> {noreply, Models}
    end;

handle_cast({notify_loged_user, Name}, Models) ->
    T = Models#models.messages_not_delivered,
    {Messages, RestMessages} = lists:partition(
                                 fun({To, _, _})-> To =:= Name end, T),
    {ok, To} = user_name_to_user_pid_(Name, Models),

    lists:foreach(fun({_, From, Msg}) ->
                          To ! {msg_from_user, Msg, From}
                  end,
                  Messages),

    {noreply, Models#models{messages_not_delivered = RestMessages}};

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

terminate(_Reason, _LoopData) ->
    ok.
