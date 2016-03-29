%%%-------------------------------------------------------------------
%%% @author shibing
%%% @copyright (C) 2016, shibing
%%% @doc
%%%
%%% @end
%%% Created : 2016-03-28 17:09:13.857317
%%%-------------------------------------------------------------------
-module(participant_sup).

-behaviour(supervisor).

%% API
-export([start_link/1, start_child/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(LSock) ->
    {ok, Pid} =  supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]),
    {ok, _} = start_child(),
    {ok, Pid}.

start_child() ->
    supervisor:start_child(?SERVER, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([LSock]) ->
        RestartStrategy = simple_one_for_one,
        MaxRestarts = 0,
        MaxSecondsBetweenRestarts = 1,

        SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

        Restart = temporary,
        Shutdown = brutal_kill,
        Type = worker,

        AChild = {participant_spec, {participant, start_link, [LSock]},
                          Restart, Shutdown, Type, [participant]},

        {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



