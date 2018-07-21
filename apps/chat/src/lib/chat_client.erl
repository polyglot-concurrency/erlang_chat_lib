%%%-------------------------------------------------------------------
%%% @author Albert Cruz
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 0X. Jul 2018 XX:XX
%%%------------------------------------------------------------------
-module(chat_client).
-author("Albert Cruz").

-behaviour(gen_server).

%% API
-export([start/1, stop/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2]).

%%%===================================================================
%%% API
%%%===================================================================


-export([create_user/2, create_group/2, login/2, logout/1, add_user_to_group/2,
         send_msg/3, send_msg_to_group/3, get_msgs/1, get_all_users_names/0]).

-define(SERVER, chat_server).


-spec create_user(string(), string()) -> ok | {error, _}.
create_user(Name, Password) ->
    gen_server:call(?MODULE, {{create_user, Name, Password}, self()}).

-spec get_all_users_names() -> list().
get_all_users_names() ->
    gen_server:call(?MODULE, {get_all_users_names, self()}).

-spec login(string(), string()) -> {ok, pid()} | {error, _}.
login(Name, Password) ->
    gen_server:call(?MODULE, {{login, Name, Password}, self()}).

-spec logout(pid()) -> ok | {error, _}.
logout(LogedUserPid) ->
    gen_server:call(?MODULE, {{logout, LogedUserPid}, self()}).

-spec create_group(pid(), string()) -> ok | {error, _}.
create_group(LogedUserPid, GroupName) ->
    gen_server:call(?MODULE, {{create_group, LogedUserPid, GroupName}, self()}).

-spec add_user_to_group(pid(), string()) -> ok | {error, _}.
add_user_to_group(LogedUserPid, GroupName) ->
    gen_server:call(?MODULE, {{add_user_to_group, LogedUserPid, GroupName}, self()}).

-spec get_msgs(pid()) -> list() | {error, _}.
get_msgs(LogedUserPid) ->
    gen_server:call(?MODULE, {{get_msgs, LogedUserPid}, self()}).

-spec send_msg(pid(), string(), string()) -> ok | {error, _}.
send_msg(LogedUserPid, To, Msg) ->
    gen_server:call(?MODULE, {{send_msg, LogedUserPid, To, Msg}, self()}).

-spec send_msg_to_group(pid(), string(), string()) -> ok | {error, _}.
send_msg_to_group(LogedUserPid, GroupName, Msg) ->
    gen_server:call(?MODULE, {{send_msg_to_group, LogedUserPid, GroupName, Msg}, self()}).


start(ServerNode) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, ServerNode, []).

init(_Args) ->
    {ok, _Args}.

stop() ->
    gen_server:cast(?MODULE, stop).

handle_call({{login, Name, Password}, _Pid}, _From, ServerNode) ->
    {?SERVER, ServerNode} ! {login, Name, Password, node(), self()},
    {reply, receive Response -> Response end, ServerNode};

handle_call({{create_user, Name, Password}, _Pid}, _From, ServerNode) ->
    {?SERVER, ServerNode} ! {create_user, Name, Password, self()},
    {reply, receive Response -> Response end, ServerNode};

handle_call({{logout, LogedUserPid}, _Pid}, _From, ServerNode) ->
    {?SERVER, ServerNode} ! {logout, LogedUserPid, self()},
    {reply, receive Response -> Response end, ServerNode};

handle_call({{add_user_to_group, LogedUserPid, GroupName}, _Pid}, _From, ServerNode) ->
    {?SERVER, ServerNode} ! {add_user_to_group, LogedUserPid, GroupName, self()},
    {reply, receive Response -> Response end, ServerNode};

handle_call({{get_msgs, LogedUserPid}, _Pid}, _From, ServerNode) ->
    {?SERVER, ServerNode} ! {get_msgs, LogedUserPid, self()},
    {reply, receive Response -> Response end, ServerNode};

handle_call({{send_msg, LogedUserPid, To, Msg}, _Pid}, _From, ServerNode) ->
    {?SERVER, ServerNode} ! {send_msg, LogedUserPid, To, Msg},
    {reply, ok, ServerNode};

handle_call({{send_msg_to_group, LogedUserPid, GroupName, Msg}, _Pid}, _From, ServerNode) ->
    {?SERVER, ServerNode} ! {send_msg_to_group, LogedUserPid, GroupName, Msg},
    {reply, ok, ServerNode};

handle_call({get_all_users_names, _Pid}, _From, ServerNode) ->
    {?SERVER, ServerNode} ! {get_all_users_names, self()},
    {reply, receive Response -> Response end, ServerNode}.

terminate(_Reason, _LoopData) ->
    ok.

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.
