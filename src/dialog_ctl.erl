%%% File    : dialog_ctl.erl
%%% Author  : Dominique Boucher 
%%% Description : Dialog API controller


-module(dialog_ctl).

-export([start/3, stop/0, client_send/1, client_receive/0, wait_for/3, viewer_send/2, viewer_receive/1]).
-include("dialog.hrl").


-define(DEFAULT_TIMEOUT, 60 * 1000).
-define(SESSIONID_KEY, dialog_session).
-record(ctlr_state, {client, client_msg, viewer, viewer_msg}).


%% Starts a new dialog controller process
%% Returns the process PID.
start(SessionId, Fun, Direction) ->
    Pid = spawn(fun() -> 
			ctl_loop(#ctlr_state{})
		end),
    spawn(fun() ->
		  dialog_sessions:register(SessionId, Pid),
		  install_sessionid(SessionId),
		  Fun(SessionId, Direction),
		  stop(),
		  uninstall_sessionid(),
		  dialog_sessions:unregister(SessionId)
	  end),
    Pid.
    

get_sessionid() ->
    get({?MODULE, ?SESSIONID_KEY}).

install_sessionid(SessionId) ->
    put({?MODULE, ?SESSIONID_KEY}, SessionId).

uninstall_sessionid() ->
    put({?MODULE, ?SESSIONID_KEY}, undefined).



stop() ->
    case get_controller() of
	undefined ->
	    ok;
	Pid ->
	    Pid ! stop,
	    ok
    end.


get_controller() ->
    SessionId = get_sessionid(),
    dialog_sessions:session_process(SessionId).


client_send(Interaction) ->
    Pid = get_controller(),
    send(Pid, {client_send, Interaction}).

viewer_send(Pid, InteractionResult) ->
    send(Pid, {viewer_send, InteractionResult}).

send(Pid, Message) ->
    Pid ! Message.

client_receive() ->
    Pid = get_controller(),
    Ref = make_ref(),
    send(Pid, {client_wait, {self(), Ref}}),
    wait_for(Ref, ?DEFAULT_TIMEOUT).

viewer_receive(Pid) ->
    Ref = make_ref(),
    send(Pid, {viewer_wait, {self(), Ref}}),
    wait_for(Ref, ?DEFAULT_TIMEOUT).


wait_for(Ref, Timeout) ->
    receive
	{Ref, Value} ->
	    Value
    after Timeout ->
	    {error, timeout}
    end.


wait_for(Message, Ref, Timeout) ->
    receive 
	{Ref, Message} ->
	    ok
    after Timeout ->
	    {error, timeout}
    end.



ctl_loop(State) ->
    receive
	{client_wait, {ClientPid, ClientRef}} ->
	    NewState = handle_client_wait(State, ClientPid, ClientRef),
	    ctl_loop(NewState);
	{viewer_wait, {ViewerPid, ViewerRef}} ->
	    NewState = handle_viewer_wait(State, ViewerPid, ViewerRef),
	    ctl_loop(NewState);
	{client_send, Interaction} ->
	    NewState = handle_client_send(State, Interaction),
	    ctl_loop(NewState);
	{viewer_send, InteractionResult} ->
	    NewState = handle_viewer_send(State, InteractionResult),
	    ctl_loop(NewState);
	stop ->
	    case State#ctlr_state.client of
		{ClientPid, ClientRef} ->
		    ClientPid ! {ClientRef, hungup};
		_ -> ok
	    end,
	    case State#ctlr_state.viewer of
		{ViewerPid, ViewerRef} ->
		    ViewerPid ! {ViewerRef, hangup};
		_ -> ok
	    end,
	    stopped
    end.

handle_client_send(State, Interaction) ->
    case State#ctlr_state.viewer of
	{_, _} ->
	    send_to_viewer(Interaction, State),
	    State#ctlr_state{client_msg = undefined, viewer = undefined};
	 _ ->
	    State#ctlr_state{client_msg = Interaction}
    end.

handle_client_wait(State, ClientPid, ClientRef) ->    
    case State#ctlr_state.viewer_msg of
	undefined ->
	    State#ctlr_state{client = {ClientPid, ClientRef}};
	InteractionResult ->
	    send_to_client(InteractionResult, State#ctlr_state{client={ClientPid, ClientRef}}),
	    State#ctlr_state{viewer_msg = undefined}
    end.

handle_viewer_send(State, InteractionResult) ->
    case State#ctlr_state.client of
	{_, _} ->
	    send_to_client(InteractionResult, State),
	    State#ctlr_state{viewer_msg = undefined, client = undefined};
	 _ ->
	    State#ctlr_state{viewer_msg = InteractionResult}
    end.

handle_viewer_wait(State, ViewerPid, ViewerRef) ->
    case State#ctlr_state.client_msg of
	undefined ->
	    State#ctlr_state{viewer = {ViewerPid, ViewerRef}};
	Interaction ->
	    send_to_viewer(Interaction, State#ctlr_state{viewer={ViewerPid, ViewerRef}}),
	    State#ctlr_state{client_msg= undefined}
    end.


send_to_viewer(Interaction, State) ->    
    {ViewerPid, ViewerRef} = State#ctlr_state.viewer,
    ViewerPid ! {ViewerRef, Interaction}.

send_to_client(InteractionResult, State) ->
    {ClientPid, ClientRef} = State#ctlr_state.client,
    ClientPid ! {ClientRef, InteractionResult}.
    
    
