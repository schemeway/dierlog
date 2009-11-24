%%% File    : xmppbot.erl
%%% Author  : Dominique Boucher
%%% Description : XMPP Bot server


-module(xmppbot).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([check_availability/1, start_outbound/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [dierlog:xmpp_account(), dierlog:xmpp_server()], []).

check_availability({Node, Domain}) ->
    gen_server:call(?MODULE, {check_availability, {Node, Domain}}, 1000).

start_outbound(JID) ->
    gen_server:call(?MODULE, {outbound, JID}, 1000).



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
init([Account, Server]) ->
    error_logger:info_msg("Starting XMPP bot server~n"),
    State = init_session(Account, Server),
    {ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({check_availability, {Node, Domain}}, _From, State) ->
    Reply = check_available({Node, Domain}),
    {reply, Reply, State};
handle_call({outbound, JID}, _From, State = {Session, ServerJID}) ->
    start_outbound(Session, ServerJID, JID),
    {reply, ok, State}.

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
handle_info(#received_packet{packet_type=presence, from=JIDStr, type_attr=Status}, State) ->
    set_availability(exmpp_jid:parse(JIDStr), Status),
    {noreply, State};

handle_info(#received_packet{packet_type=message, raw_packet=Packet}, State = {Session, ServerJID}) ->
    Sender = exmpp_stanza:get_sender(Packet),
    Body = exmpp_message:get_body(Packet),
    process_message(Session, ServerJID, Sender, Body),
    {noreply, State};
% ignore all other packets
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, {Session, _}) ->
    error_logger:info_msg("Terminating XMPP bot server~n"),
    exmpp_session:stop(Session),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

init_session({Node, Domain, Password}, {Host, Port}) ->
    application:start(exmpp),
    %% Start XMPP session: Needed to start service (Like
    %% exmpp_stringprep):
    MySession = exmpp_session:start(),
    %% Create XMPP ID (Session Key):
    MyJID = exmpp_jid:make(Node, Domain, random),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(MySession, MyJID, Password),
    %% Connect in standard TCP:
    _StreamId = exmpp_session:connect_TCP(MySession, Host, Port),
    session(MySession, MyJID).


%% We are connected. We now log in (and try registering if authentication fails)
session(MySession, MyJID) ->
    %% Login with defined JID / Authentication:
    try exmpp_session:login(MySession)
    catch
	throw:{auth_error, 'not-authorized'} ->
	    %% Try creating a new user:
	    io:format("Register~n",[]),
	    %% In a real life client, we should trap error case here
	    %% and print the correct message.
	    exmpp_session:register_account(MySession, "password"),
	    %% After registration, retry to login:
	    exmpp_session:login(MySession)
    end,
    %% We explicitely send presence:
    exmpp_session:send_packet(MySession,
			      exmpp_presence:set_status(
				exmpp_presence:available(), "XMPP Bot Ready")),
    {MySession, MyJID}.


set_availability(JID, "available") ->
    error_logger:info_msg("~p is available~n", [JID]),
    Node = exmpp_jid:node_as_list(JID),
    Domain = exmpp_jid:domain_as_list(JID),
    put({availability, Node, Domain}, JID);
set_availability(JID, "unavailable") ->
    Node = exmpp_jid:node_as_list(JID),
    Domain = exmpp_jid:domain_as_list(JID),
    put({availability, Node, Domain}, undefined).

check_available({Node, Domain}) ->
    case get({availability, Node, Domain}) of
	undefined ->
	    offline;
	JID ->
	    {online, JID}
    end.
    

send_message(MySession, MyJID, Recipient, Msg) ->
    Packet = exmpp_stanza:set_sender(
	       exmpp_stanza:set_recipient(
		 exmpp_message:chat(Msg),
		 Recipient), 
	       MyJID),
    exmpp_session:send_packet(MySession, Packet).


start_outbound(MySession, MyJID, JID) ->
    SessionID = exmpp_jid:to_list(JID),
    Answer = im_dialog:start_dialog(SessionID, outbound, dierlog:dialog_module()),
    send_message(MySession, MyJID, JID, Answer).


process_message(_, _, _, undefined) ->
    ignored;
process_message(MySession, MyJID, Sender, Body) ->
    BodyStr = binary_to_list(Body),
    SessionID = binary_to_list(Sender),
    Answer = im_dialog:process_message(SessionID, BodyStr, dierlog:dialog_module()),
    send_message(MySession, MyJID, Sender, Answer).

