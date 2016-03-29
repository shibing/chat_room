%%%-------------------------------------------------------------------
%%% @author shibing
%%% @copyright (C) 2016, shibing
%%% @doc
%%%
%%% @end
%%% Created : 2016-03-28 23:11:56.225921
%%%-------------------------------------------------------------------
-module(room_manager).

-behaviour(gen_server).

%% API
-export([start_link/0,
        addRoom/1, 
        getRoom/1,
        getDefaultRoom/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {rooms}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    {ok, _} = addRoom(default_room),
    {ok, Pid}.

addRoom(Name) ->
    gen_server:call(?SERVER, {addRoom, Name}).

getRoom(Name) ->
    gen_server:call(?SERVER, {getRoom, Name}).

getDefaultRoom() ->
    {ok, RoomRef} = getRoom(default_room),
    {ok, {default_room, RoomRef}}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{rooms = dict:new()}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({addRoom, Name}, _From, State) ->
    Rooms = State#state.rooms,
    case dict:find(Name, Rooms) of
        {ok, [ExistRoomRef]} ->
            {reply, {error, already_exist, ExistRoomRef}, State};
        error ->
            {ok, RoomRef} = room_sup:start_child(Name),
            NewRooms = dict:append(Name, RoomRef, Rooms),
            {reply, {ok, RoomRef}, #state{rooms = NewRooms}}
    end;

handle_call({getRoom, Name}, _From, State) ->
    Rooms  = State#state.rooms,
    case dict:find(Name, Rooms) of
        {ok, [RoomRef]} ->
            {reply, {ok, RoomRef}, State};
        error ->
            {reply, {error, no_such_room}, State}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
        {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
        ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
        {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================




