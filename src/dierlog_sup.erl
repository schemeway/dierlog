%%% File    : dialog_sup.erl
%%% Author  : Dominique Boucher
%%% Description : The Dierlog supervisor


-module(dierlog_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
    SessionsServer = {dialog_sessions, 
		      {dialog_sessions, start_link, []},
		      permanent, 2000, worker, 
		      [dialog_sessions]},
    XmppServer     = {xmppbot, 
		      {xmppbot, start_link, []},
		      permanent, 2000, worker, 
		      [xmppbot]},
    case dierlog:xmpp_enabled() of
	true ->
	    {ok, {{one_for_all, 2, 10}, [SessionsServer, XmppServer]}};
	_ ->
	    {ok, {{one_for_all, 2, 10}, [SessionsServer]}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
