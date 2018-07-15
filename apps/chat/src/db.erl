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
        {users = #{}, current_loged_users = #{}, groups, messages_not_delivered}).


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


add_loged_user(Name, UserPid) ->
    gen_server:call(?MODULE, {{add_loged_user, Name, UserPid}, self()}).

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

user_name_to_user_pid(Name) ->
    gen_server:call(?MODULE, {{user_name_to_user_pid, Name}, self()}).

user_pid_to_user_name(LogedUserPid) ->
    gen_server:call(?MODULE, {{user_pid_to_user_name, LogedUserPid}, self()}).

get_msgs(LogedUserPid) ->
    gen_server:call(?MODULE, {{get_msgs, LogedUserPid}, self()}).

-spec send_msg(pid(), string(), string()) -> ok | {error, _}.
send_msg(User, To, Msg) ->
    gen_server:cast(?MODULE, {send_msg, User, To, Msg}).

handle_call({{add_user, Name, Pass}, Pid}, _From, Models) ->
    T = Models#models.users,
    case maps:is_key(Name, T) of
        true -> {reply, {error, user_exists}, Models};
        false -> {reply, ok, Models#models{users = T#{Name => Pass}}}
    end;

handle_call({{add_loged_user, Name, UserPid}, Pid}, _From, Models) ->
    T = Models#models.current_loged_users,
    case maps:is_key(Name, T) of
        true -> {reply, {error, user_already_loged}, Models};
        false -> {reply, ok, Models#models{current_loged_users = T#{Name => UserPid}}}
    end;

handle_call({pget_loged_users, Pid}, _From, Models) ->
    T = Models#models.current_loged_users,
    E = maps:to_list(T),
    {reply, {ok, lists:map(fun({Name, _}) -> Name end, E)}, Models};

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

handle_call({{user_name_to_user_pid, Name}, Pid}, _From, Models) ->
    {reply, user_name_to_user_pid_(Name, Models), Models}.

%% Utility functions

user_pid_to_user_name_(LogedUserPid, Models)->
    T = Models#models.current_loged_users,
    E = maps:to_list(T),
    R = lists:keyfind(LogedUserPid, 2, E),
    case R of
        false ->  {error, user_is_not_loged};
        {Name, _} ->  {ok, Name}
    end.

user_name_to_user_pid_(Name, Models)->
    T = Models#models.current_loged_users,
    Res = maps:get(Name, T, default),
    case Res of
        default ->  {error, user_is_not_loged};
        Key ->  {ok, Key}
    end.

handle_cast({send_msg, UserPid, ToName, Msg}, Models) ->
    %% TODO: proper validation
    {ok, To} = user_name_to_user_pid_(ToName, Models),
    {ok, UserName} = user_pid_to_user_name_(UserPid, Models),
    To ! {msg_from_user, Msg, UserName},
    {noreply, Models};

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

terminate(_Reason, _LoopData) ->
    ok.
