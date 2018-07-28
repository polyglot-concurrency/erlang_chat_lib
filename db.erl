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
        {users = #{}::map(), current_loged_users = #{}::map(),
         groups = #{}::map(), messages_not_delivered = []::list({string(), string(), string()})}
       ).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-export([start/0, stop/0, send_msg/3, send_msg_to_group/3, add_user/2,
         add_group/2, add_user_to_group/2, get_loged_users/1, add_loged_user/2,
         user_pid_to_user_name/1, user_name_to_user_pid/1, get_msgs/1, is_user/2,
         is_user_loged/1, is_user_pid_loged/1, remove_loged_user/1, get_all_users_names/0]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, #models{}, []).

init(_Args) ->
    {ok, _Args}.

stop() ->
    gen_server:cast(?MODULE, stop).

-spec add_user(string(), string()) -> ok | {error, _}.
add_user(Name, Pass) ->
    gen_server:call(?MODULE, {{add_user, Name, Pass}, self()}).

-spec add_group(string(), string()) -> ok | {error, _}.
add_group(GroupName, UserName) ->
    gen_server:call(?MODULE, {{add_group, GroupName, UserName}, self()}).

-spec add_user_to_group(string(), string()) -> ok | {error, _}.
add_user_to_group(GroupName, UserName) ->
    gen_server:call(?MODULE, {{add_user_to_group, GroupName, UserName}, self()}).

-spec add_loged_user(string(), pid()) -> ok | {error, _}.
add_loged_user(Name, UserPid) ->
    R = gen_server:call(?MODULE, {{add_loged_user, Name, UserPid}, self()}),
    gen_server:cast(?MODULE, {notify_loged_user, Name}),
    R.

-spec remove_loged_user(pid()) -> ok | {error, _}.
remove_loged_user(LogedUserPid) ->
    gen_server:call(?MODULE, {{remove_loged_user, LogedUserPid}, self()}).

-spec get_loged_users(pid()) -> {ok, list()} | {error, _}.
get_loged_users(LogedUserPid) ->
    gen_server:call(?MODULE, {{get_loged_users, LogedUserPid}, self()}).

-spec is_user(string(), string()) -> ok | {error, _}.
is_user(Name, Password) ->
    gen_server:call(?MODULE, {{is_user, Name, Password}, self()}).

-spec is_user_loged(string()) -> boolean().
is_user_loged(Name) ->
    {ok, All} = pget_loged_users(),
    lists:member(Name, All).

-spec is_user_pid_loged(pid()) -> ok | {error, _}.
is_user_pid_loged(UserPid) ->
    gen_server:call(?MODULE, {{is_user_pid_loged, UserPid}, self()}).

-spec user_name_to_user_pid(string()) -> {ok, pid()} | {error, _}.
user_name_to_user_pid(Name) ->
    gen_server:call(?MODULE, {{user_name_to_user_pid, Name}, self()}).

-spec user_pid_to_user_name(pid()) -> {ok, string()} | {error, _}.
user_pid_to_user_name(LogedUserPid) ->
    gen_server:call(?MODULE, {{user_pid_to_user_name, LogedUserPid}, self()}).

-spec get_msgs(pid()) -> list() | {error, _}.
get_msgs(LogedUserPid) ->
    gen_server:call(?MODULE, {{get_msgs, LogedUserPid}, self()}).

-spec send_msg(pid(), string(), string()) -> ok | {error, _}.
send_msg(LogedUserPid, To, Msg) ->
    gen_server:cast(?MODULE, {send_msg, LogedUserPid, To, Msg}).

-spec send_msg_to_group(pid(), string(), string()) -> ok | {error, _}.
send_msg_to_group(LogedUserPid, GroupName, Msg) ->
    gen_server:cast(?MODULE, {send_msg_to_group, LogedUserPid, GroupName, Msg}).

%% Private functions
-spec pget_loged_users() -> {ok, list()} | {error, _}.
pget_loged_users() ->
    gen_server:call(?MODULE, {pget_loged_users, self()}).

-spec get_all_users_names() -> list().
get_all_users_names() ->
    gen_server:call(?MODULE, {get_all_users_names, self()}).

-spec is_user_loged(string(), #models{}) -> boolean().
is_user_loged(Name, Models) ->
    All =pget_loged_users_(Models),
    lists:member(Name, All).

handle_call({{add_user, Name, Pass}, _Pid}, _From, Models) ->
    add_user_logic(Models, Name, Pass);

handle_call({{add_group, GroupName, UserName}, _Pid}, _From, Models) ->
    add_group_logic(Models, GroupName, UserName);

handle_call({{add_user_to_group, GroupName, UserName}, _Pid}, _From, Models) ->
    add_user_to_group_logic(Models, GroupName, UserName);

handle_call({{add_loged_user, Name, UserPid}, _Pid}, _From, Models) ->
    add_loged_user_logic(Models, Name, UserPid);

handle_call({{remove_loged_user, LogedUserPid}, _Pid}, _From, Models) ->
    remove_loged_user_logic(LogedUserPid, Models);

handle_call({pget_loged_users, _Pid}, _From, Models) ->
    {reply, {ok, pget_loged_users_(Models)}, Models};

handle_call({get_all_users_names, _Pid}, _From, Models) ->
    get_all_users_names_logic(Models);

handle_call({{get_loged_users, LogedUserPid}, _Pid}, _From, Models) ->
    get_loged_users_logic(Models, LogedUserPid);

handle_call({{is_user, Name, Password}, _Pid}, _From, Models) ->
    is_user_logic(Models, Name, Password);

handle_call({{user_pid_to_user_name, LogedUserPid}, _Pid}, _From, Models) ->
    {reply, user_pid_to_user_name_(LogedUserPid, Models), Models};

handle_call({{get_msgs, LogedUserPid}, _Pid}, _From, Models) ->
    LogedUserPid ! {get_messages, self()},
    {reply, {ok, receive Response -> Response end}, Models};

handle_call({{is_user_pid_loged, UserPid}, _Pid}, _From, Models) ->
    is_user_pid_loged_logic(UserPid, Models);

handle_call({{user_name_to_user_pid, Name}, _Pid}, _From, Models) ->
    {reply, user_name_to_user_pid_(Name, Models), Models}.

is_user_pid_loged_logic(UserPid, Models) ->
    UserNameResponse = user_pid_to_user_name_(UserPid, Models),
    case UserNameResponse of
        {ok, _} -> {reply, ok, Models};
        {error, user_is_not_loged} -> {reply, {error, user_is_not_loged}, Models}
    end.

is_user_logic(Models, Name, Password) ->
    Users = Models#models.users,
    case maps:is_key(Name, Users) of
        true ->
            UserPass = maps:get(Name, Users),
            case UserPass =:= Password of
                true -> {reply, ok, Models};
                false -> {reply, {error, wrong_password}, Models}
            end;
        false -> {reply, {error, user_do_not_exists}, Models}
    end.

get_loged_users_logic(Models, LogedUserPid) ->
    LogedUsers = Models#models.current_loged_users,
    LogedUsersAsList = maps:to_list(LogedUsers),
    UserResponse = lists:keyfind(LogedUserPid, 2, LogedUsersAsList),
    case UserResponse of
        false -> {reply, {error, user_is_not_loged}, Models};
        _ ->
            AllUsers = lists:filter(fun({_Name, UserPid}) -> LogedUserPid =/= UserPid end, LogedUsersAsList),
            {reply, {ok, lists:map(fun({Name, _}) -> Name end, AllUsers)}, Models}
    end.

get_all_users_names_logic(Models) ->
    Users = Models#models.users,
    UsersList = maps:to_list(Users),
    {reply, lists:map(fun({Name, _}) -> Name end, UsersList), Models}.

remove_loged_user_logic(LogedUserPid, Models) ->
    UserNameResponse = user_pid_to_user_name_(LogedUserPid, Models),
    case UserNameResponse of
        {ok, UserName} -> LogedUsers = Models#models.current_loged_users,
            {reply, ok, Models#models{current_loged_users = maps:remove(UserName, LogedUsers)}};
        E -> {reply, E, Models}
    end.

add_loged_user_logic(Models, Name, UserPid) ->
    LogedUsers = Models#models.current_loged_users,
    case maps:is_key(Name, LogedUsers) of
        true -> {reply, {error, user_already_loged}, Models};
        false -> {reply, ok, Models#models{current_loged_users = LogedUsers#{Name => UserPid}}}
    end.

add_user_to_group_logic(Models, GroupName, UserName) ->
    Groups = Models#models.groups,
    case maps:is_key(GroupName, Groups) of
        true ->
            {reply, ok, Models#models{groups = Groups#{GroupName => [UserName | maps:get(GroupName, Groups)]}}};
        false ->
            {reply, {error, group_do_not_exist}, Models}
    end.

add_group_logic(Models, GroupName, UserName) ->
    Groups = Models#models.groups,
    case maps:is_key(GroupName, Groups) of
        true -> {reply, {error, group_exist}, Models};
        false -> {reply, ok, Models#models{groups = Groups#{GroupName => [UserName]}}}
    end.

add_user_logic(Models, Name, Pass) ->
    Users = Models#models.users,
    case maps:is_key(Name, Users) of
        true -> {reply, {error, user_exist}, Models};
        false -> {reply, ok, Models#models{users = Users#{Name => Pass}}}
    end.

%% Utility functions

user_pid_to_user_name_(LogedUserPid, Models) ->
    LogedUsers = Models#models.current_loged_users,
    LogedUsersAsList = maps:to_list(LogedUsers),
    NameResponse = lists:keyfind(LogedUserPid, 2, LogedUsersAsList),
    case NameResponse of
        false -> {error, user_is_not_loged};
        {Name, _} -> {ok, Name}
    end.

user_name_to_user_pid_(Name, Models) ->
    LogedUsers = Models#models.current_loged_users,
    PidResult = maps:get(Name, LogedUsers, default),
    case PidResult of
        default -> {error, user_is_not_loged};
        Pid -> {ok, Pid}
    end.

pget_loged_users_(Models)->
    LogedUsers = Models#models.current_loged_users,
    LogedUsersAsList = maps:to_list(LogedUsers),
    lists:map(fun({Name, _}) -> Name end, LogedUsersAsList).

handle_cast({send_msg, UserPid, ToName, Msg}, Models) ->
    send_msg_logic(ToName, Models, UserPid, Msg);

handle_cast({send_msg_to_group, UserPid, GroupName, Msg}, Models) ->
    send_msg_to_group_logic(Models, GroupName, UserPid, Msg);

handle_cast({notify_loged_user, Name}, Models) ->
    notify_loged_user_logic(Models, Name);

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

notify_loged_user_logic(Models, Name) ->
    MessagesNoDelivered = lists:reverse(Models#models.messages_not_delivered),
    {Messages, RestMessages} = lists:partition(
        fun({To, _, _}) -> To =:= Name end, MessagesNoDelivered),
    {ok, To} = user_name_to_user_pid_(Name, Models),
    lists:foreach(fun({_, From, Msg}) ->
        To ! {msg_from_user, Msg, From}
                  end,
        Messages),
    {noreply, Models#models{messages_not_delivered = RestMessages}}.

send_msg_to_group_logic(Models, GroupName, UserPid, Msg) ->
    Groups = Models#models.groups,
    case maps:is_key(GroupName, Groups) of
        true ->
            UsersNamesInGroup = maps:get(GroupName, Groups),
            {ActiveUsers, InActiveUsers} = lists:partition(fun(N) -> is_user_loged(N, Models) end, UsersNamesInGroup),
            LogedUsersPids = lists:map(fun(UName) ->
                {ok, To} = user_name_to_user_pid_(UName, Models),
                To end, ActiveUsers),
            {ok, UserName} = user_pid_to_user_name_(UserPid, Models),
            LogedUsersPidsClean = lists:filter(fun(To) -> To =/= UserPid end, LogedUsersPids),
            lists:foreach(fun(To) ->
                To ! {msg_from_user, Msg, GroupName ++ ":" ++ UserName}
                          end, LogedUsersPidsClean),
            MessagesNoDelivered = Models#models.messages_not_delivered,
            NewMessages = lists:map(fun(ToName) -> {ToName, GroupName ++ ":" ++ UserName, Msg} end, InActiveUsers),
            {noreply, Models#models{messages_not_delivered = NewMessages ++ MessagesNoDelivered}};
        false -> {noreply, Models}
    end.

send_msg_logic(ToName, Models, UserPid, Msg) ->
    UserPidResponse = user_name_to_user_pid_(ToName, Models),
    {ok, SenderName} = user_pid_to_user_name_(UserPid, Models),
    case UserPidResponse of
        {ok, To} ->
            To ! {msg_from_user, Msg, SenderName},
            {noreply, Models};

        {error, user_is_not_loged} ->
            {noreply, Models#models{messages_not_delivered = [{ToName, SenderName, Msg} | Models#models.messages_not_delivered]}}
    end.

terminate(_Reason, _LoopData) ->
    ok.
