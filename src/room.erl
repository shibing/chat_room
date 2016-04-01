%%%-------------------------------------------------------------------
%%% @author shibing
%%% @copyright (C) 2016, shibing
%%% @doc
%%%
%%% @end
%%% Created : 2016-03-28 22:02:43.940321
%%%-------------------------------------------------------------------
-module(room).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         getName/1,
         getAnonymousName/1,
         addParticipant/3,
         getParticipant/2,
         modifyNick/4,
         sayToAll/3
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          name = default_room, %聊天室的名字
          participants, % 该聊天室下的所有参与者，用dict类型的映射保存
          max_id = 0 %用来生成匿名名字
         }).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [default_room], []).

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

getName(ServerRef) ->
    gen_server:call(ServerRef, {getName}).

getAnonymousName(ServerRef) ->
    gen_server:call(ServerRef, {getAnonymousName}).

addParticipant(ServerRef, Name, Ref) ->
    gen_server:call(ServerRef, {addParticipant, {Name, Ref}}).

getParticipant(ServerRef, Name) ->
    gen_server:call(ServerRef, {getParticipant, Name}).

modifyNick(ServerRef, OldName, NewName, Ref) ->
    gen_server:call(ServerRef, {modifyNick, {OldName, NewName, Ref}}).


sayToAll(ServerRef, SenderName, Data) ->
    gen_server:cast(ServerRef, {sayToAll, SenderName, Data}).



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
init([Name]) ->
    {ok, #state{name = Name, max_id = 0, participants = dict:new()}}.

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

handle_call({getName}, _From, State) ->
    Reply = State#state.name,
    {reply, Reply, State};

handle_call({getAnonymousName}, _From, State) ->
    Id = State#state.max_id + 1,
    {reply, lists:concat(['Anonymous', Id ]), State#state{max_id = Id}};

handle_call({addParticipant, {Name, Ref}}, _From, State) ->
    ParticipantList = State#state.participants,
    case dict:find(Name, ParticipantList) of 
        {ok, [Ref]} -> 
            {reply, ok, State};
        {ok, [_]} ->
            {reply, {error, already_exist_user}, State};
        error ->
            NewList = dict:append(Name, Ref, ParticipantList),
            {reply, ok, State#state{participants = NewList}}
    end;

handle_call({getParticipant, Name}, _From, State) ->
    ParticipantList = State#state.participants,
    case dict:find(Name, ParticipantList) of 
        {ok, [Ref]} ->
            {reply, {ok, Ref}, State};
        error ->
            {reply, {error, not_exist_user}, State}
    end;

handle_call({modifyNick, {OldName, NewName, Ref}}, _From, State) ->
    ParticipantList = State#state.participants,
    case dict:find(OldName, ParticipantList) of  
        {ok, [Ref]} ->
            case dict:find(NewName, ParticipantList) of 
                error ->
                    NewParticipants = dict:erase(OldName, dict:append(NewName, Ref, ParticipantList)),
                    {reply, {ok}, State#state{participants = NewParticipants}};
                {ok, [_]} ->
                    {reply, {error, already_exist_user}, State}
            end;
        error ->
            {reply, {error, user_not_exist}, State}
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

handle_cast({sayToAll, SenderName, Msg}, State) ->
    dict:map(
      fun(PName, [PRef]) ->
              case PName =:= SenderName of
                  true ->
                      ok;
                  false ->
                      participant:sendMsg(PRef, SenderName, Msg)
              end
      end,
      State#state.participants
     ),
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




