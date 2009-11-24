%%% File    : dialog_sessions.erl
%%% Author  : Dominique Boucher 
%%% Description : Dialog session server


-module(dialog_sessions).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register/2, unregister/1, session_process/1]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



register(SessionId, ProcessId) ->
    gen_server:call(?MODULE, {register, SessionId, ProcessId}, 1000).

unregister(SessionId) ->
    gen_server:call(?MODULE, {unregister, SessionId}, 1000).

session_process(SessionId) ->
    gen_server:call(?MODULE, {session_process, SessionId}, 1000).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    error_logger:info_msg("Starting dialog sessions server.~n"),
    ets:new(sessions, [ordered_set, named_table]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({register, SessionId, ProcessId}, _From, State) ->
    ets:insert(sessions, {SessionId, ProcessId}),
    {reply, ok, State};

handle_call({unregister, SessionId}, _From, State) ->
    ets:delete(sessions, SessionId),
    {reply, ok, State};

handle_call({session_process, SessionId}, _From, State) ->
    Pid = case ets:lookup(sessions, SessionId) of
	      [{_, Val} | _] -> Val;
	      []             -> undefined
	  end,
    {reply, Pid, State}.



%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    error_logger:info_msg("Terminating dialog sessions server.~n"),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

