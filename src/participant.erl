-module(participant).
-compile([debug_info]).

-behaviour(gen_server).

%% API functions
-export([start_link/1,
        getNick/1,
        sendMsg/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          lsock, %监听套接字
          sock,
          nick , %用户昵称,
          room_name, %聊天室名称
          data_buf  %缓存用户发送的消息
         }).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

getNick(ParticipantRef) ->
    gen_server:call(ParticipantRef, {getNick}).

joinDefaultRoom() -> 
    {ok,{RoomName, DefaultRoom}} = room_manager:getDefaultRoom(),
    NickName = room:getAnonymousName(DefaultRoom),
    room:addParticipant(DefaultRoom, NickName, self()),
    {ok, NickName, RoomName}.

%给参与者发送消息
sendMsg(ParticipantRef, From, Msg) ->
    gen_server:cast(ParticipantRef, {sendMsg, From, Msg}).

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
init([LSock]) ->
    {ok, #state{lsock = LSock, data_buf = []}, 0}.

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

handle_call({getNick}, _From, State) ->
    {reply, {ok, State#state.nick}, State}.
    

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

handle_cast({sendMsg, From, Msg}, State) ->
    gen_tcp:send(State#state.sock, [From, " Say: ",  Msg]),
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

handle_info(timeout, State) ->
    {ok, Client} = gen_tcp:accept(State#state.lsock),
    participant_sup:start_child(),
    {ok, NickName, RoomName} = joinDefaultRoom(),
    {noreply, State#state{sock = Client, nick = NickName, room_name = RoomName}};

handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};

handle_info({tcp, _Sock, Data}, State) ->
    _Data = lists:append(State#state.data_buf, Data),
    NewState = processData(State#state{data_buf = _Data}),
    io:format("~p~n", [NewState]),
    case NewState of 
        {quit, _NewState} ->
            {stop, user_quit, _NewState};
        _ ->
            {noreply, NewState}
    end.


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
terminate(_Reason, State) ->
    gen_tcp:close(State#state.sock),
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

processData(State) ->
    Data = State#state.data_buf,
    case lists:splitwith(fun(C) -> C =/= $\r end, Data) of 
        {_Msg, []} -> % need more data
            State;
        {_Msg, [$\r]} -> % need more data
            State;
        {Msg, [$\r, $\n | Remain]} ->
            case Msg of
                [$/ | Command] -> 
                    case doCommand(Command, State) of 
                        {ok, NewState} ->
                            processData(NewState#state{data_buf = Remain});
                        {stop, NewState} -> 
                            {stop, NewState};
                        error ->
                            State#state{data_buf = []}
                    end;
                _ -> % chat to all
                    sayToAll(Msg, State),
                    processData(State#state{data_buf = Remain})
            end;
        {_Msg, [$\r, _ | _Remain]} -> % format error
            State#state{data_buf = []}
    end.

doCommand(Command, State) ->
    case Command of 
        [$n, $i, $c, $k | Remain] ->
            {ok, NewNick} = modifyNick(string:strip(Remain), State),
            {ok, State#state{nick = NewNick}};
        [$q, $u, $i, $t | _ ] ->
            {stop, State};
        _ ->
            ok
    end.

modifyNick([], _State) ->
    {error, empty_name};

modifyNick(Nick, State) ->
    if Nick =:=  State#state.nick ->
           {ok, Nick};
       true ->
           {ok, Room} = room_manager:getRoom(State#state.room_name),
           case room:modifyNick(Room, State#state.nick, Nick, self()) of 
               {ok} ->
                   {ok, Nick};
              Error ->
                   Error 
            end
    end.

sayToAll(Msg, State) ->
    {ok, Room} = room_manager:getRoom(State#state.room_name),
    room:sayToAll(Room, State#state.nick, lists:append(Msg, "\r\n")),
    ok.

