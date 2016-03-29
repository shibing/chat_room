%%%-------------------------------------------------------------------
%%% @author shibing
%%% @copyright (C) 2016, shibing
%%% @doc
%%%
%%% @end
%%% Created : 2016-03-29 13:25:58.316736
%%%-------------------------------------------------------------------
-module(chat_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

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
        supervisor:start_link({local, ?SERVER}, ?MODULE, [LSock]).

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
        RestartStrategy = one_for_one,
        MaxRestarts = 1000,
        MaxSecondsBetweenRestarts = 3600,

        SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

        Restart = temporary,
        Shutdown = 2000,
        Type = supervisor,

        ParticipantSupChild = {participant_sup_spec, {participant_sup, start_link, [LSock]},
                          Restart, Shutdown, Type, [participant_sup]},

        RoomSupChild = {room_sup_spec, {room_sup, start_link, []},
                               Restart, Shutdown, supervisor, [room]},

        RoomManagerChild = {room_manager_spec, {room_manager, start_link, []},
                            Restart, Shutdown, worker, [room_manager]},

        {ok, {SupFlags, [ParticipantSupChild, RoomSupChild, RoomManagerChild] }}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



