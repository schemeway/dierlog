%%% File    : dierlog.erl
%%% Author  : Dominique Boucher 
%%% Description : 


-module(dierlog).

-export([start/0, stop/0]).
-export([dialog_module/0, xmpp_enabled/0, xmpp_account/0, xmpp_server/0, nugram_account/0]).

-include_lib("yaws.hrl").

start() ->
    application:start(dierlog),
    start_yaws().

stop() ->
    application:stop(yaws),
    application:stop(dierlog),
    init:stop().


start_yaws() ->
    application:load(yaws),
    application:set_env(yaws, embedded, true),
    application:start(yaws),    
    Id = "dierlog",
    GC = yaws_config:make_default_gconf(false, Id),
    yaws_api:setconf(GC#gconf{logdir=log_dir()}, [[]]),

    IM_SC = #sconf{port = yaws_port(),
		   servername = yaws_servername(),
		   listen = {0,0,0,0},
		   appmods = [{"/im", imified}]},
    yaws_config:add_sconf(IM_SC),
    
    VXML_SC = #sconf{port = yaws_port() + 1,
		     servername = yaws_servername(),
		     listen = {0,0,0,0},
		     docroot = htdocs_dir(),
		     opaque = vxml_view,
		     appmods = [{"/vxml", dialog_http}]},
    yaws_config:add_sconf(VXML_SC),

    HTML_SC = #sconf{port = yaws_port() + 2,
		     servername = yaws_servername(),
		     listen = {0,0,0,0},
		     docroot = htdocs_dir(),
		     opaque = im_view,
		     appmods = [{"/html", dialog_http}]},
    yaws_config:add_sconf(HTML_SC),

    ok.

dialog_module() ->
    {ok, ModuleName} = application:get_env(?MODULE, dialog_module),
    ModuleName.


nugram_account() ->
    case application:get_env(?MODULE, nugram_account) of
	{ok, {Account, Password}} ->
	    {Account, Password};
	_ ->
	    undefined
    end.


xmpp_enabled() ->
    case xmpp_account() of
	undefined -> false;
	_         -> true
    end.

xmpp_account() ->
    case application:get_env(?MODULE, xmpp_account) of
	{ok, {Node, Domain, Password}} ->
	    {Node, Domain, Password};
	_ ->
	    undefined
    end.

xmpp_server() ->
    case application:get_env(?MODULE, xmpp_server) of
	{ok, {Host, Port}} ->
	    {Host, Port};
	_ ->
	    undefined
    end.

yaws_port() ->
    case application:get_env(?MODULE, yaws_port) of
	{ok, Port} ->
	    Port;
	_ ->
	    8080
    end.
    
yaws_servername() ->
    case application:get_env(?MODULE, yaws_servername) of
	{ok, Name} ->
	    Name;
	_ ->
	    {ok, Hostname} = inet:gethostname(),
	    Hostname
    end.
    
log_dir() -> relative_dir("log").
htdocs_dir() ->relative_dir("htdocs").

relative_dir(Dir) ->
    {ok, Cwd} = file:get_cwd(),
    Cwd ++ "/" ++ Dir.
