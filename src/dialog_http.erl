%%% File    : dialog_http.erl
%%% Author  : Dominique Boucher 
%%% Description : HTTP front-end for Dialog API


-module(dialog_http).

-export([out/1]).

-include("dialog.hrl").
-include_lib("yaws_api.hrl").

-define(CHARS, {$0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$a,$b,$c,$d,$e,$f}).


out(Arg) ->
    case {Arg#arg.clidata, Arg#arg.cont, (Arg#arg.headers)#headers.content_type} of 
	{{partial,_Data}, _, _} ->
	    process_fileupload(Arg);
	{_, {cont, _}, _} ->
	    process_fileupload(Arg);
	{_, undefined, "multipart/form-data;" ++ _} ->
	    process_fileupload(Arg);
	V ->
	    {Interaction, SessionId} = next_interaction(Arg, get_sessionid(Arg)),
	    View = get_view(Arg),
	    Path = Arg#arg.server_path,
	    render(Interaction, View, SessionId, Path)
    end.

process_fileupload(Arg) ->
    case yaws_multipart:read_multipart_form(Arg, [no_temp_file]) of
	{done, Params} ->
	    SessionId = get_sessionid(Arg),
	    Pid = dialog_sessions:session_process(SessionId),
	    Result = #recording{attributes = dict:fetch("recording", Params)},
	    dialog_ctl:viewer_send(Pid, Result),
	    Interaction = dialog_ctl:viewer_receive(Pid),
	    View = get_view(Arg),
	    Path = Arg#arg.server_path,
	    render(Interaction, View, SessionId, Path);
	T ->
	    T
    end.
    

get_view(Arg) ->
    {Arg#arg.opaque, render}.
    

render(hangup, View, _, Path) ->
    [View(hangup, Path), yaws_api:setcookie("sessionid", "", "/")] ;
render(Interaction, View, SessionId, Path) ->
    [View(Interaction, Path), yaws_api:setcookie("sessionid", SessionId, "/")].


get_sessionid(Arg) ->
    Headers = Arg#arg.headers,
    Cookies = Headers#headers.cookie,
    case yaws_api:find_cookie_val("sessionid", Cookies) of
	[] ->
	    new_session;
	Id -> Id
    end.
    

next_interaction(Arg, new_session) ->
    case yaws_api:getvar(Arg, "session.sessionid") of
	{ok, SessionId} ->
	    SessionId;
	_ ->
	    SessionId = new_sessionid()
    end,
    {start_dialog(SessionId), SessionId};
next_interaction(Arg, SessionId) ->
    Pid = dialog_sessions:session_process(SessionId),
    Result = decode_result(Arg),
    dialog_ctl:viewer_send(Pid, Result),
    {dialog_ctl:viewer_receive(Pid), SessionId}.


start_dialog(SessionId) ->
    DialogModule = dierlog:dialog_module(),
    AppFun = {DialogModule, start_dialog},
    Pid = dialog_ctl:start(SessionId, AppFun, inbound),
    dialog_ctl:viewer_send(Pid, answer),
    dialog_ctl:viewer_receive(Pid).

decode_result(Arg) ->
    case yaws_api:postvar(Arg, "result") of
	{ok, "dtmf"} ->
	    decode_dtmf_result(Arg);
	{ok, "event"} ->
	    decode_event_result(Arg);
	{ok, "text"} ->
	    decode_text_result(Arg);
	{ok, "hangup"} ->
	    hangup;
	{ok, "recording"} ->
	    recording;
	{ok, "next"} ->
	    next;
	{ok, "nbest"} ->
	    decode_nbest_result(Arg);
	_ ->
	    error
    end.


decode_dtmf_result(Arg) ->
    case yaws_api:postvar(Arg, "value") of
	{ok, Dtmfs} ->
	    #dtmf{string = Dtmfs};
	_ ->
	    error
    end.

decode_text_result(Arg) ->
    case yaws_api:postvar(Arg, "value") of
	{ok, Str} ->
	    #nbest{values = [Str]};
	_ ->
	    error
    end.

decode_event_result(Arg) ->
    Name = case yaws_api:postvar(Arg, "name") of
	       {ok, EventName} ->
		   list_to_atom(EventName);
	       _ ->
		   'error.unknown'
	   end,
    Msg  = case yaws_api:postvar(Arg, "msg") of
	       {ok, Message} ->
		   Message;
	       _ ->
		   ""
	   end,
    #event{name = Name, message = Msg}.

decode_nbest_result(Arg) ->
    case yaws_api:postvar(Arg, "hypotheses") of
	{ok, Hyps} ->
            {ok, JsonObject} = json:decode_string(Hyps),
	    #nbest{values = tuple_to_list(json_utils:simplify(JsonObject))};
	_ ->
	    #nbest{values = []}
    end.



new_sessionid() ->
    {A,B,C} = now(),
    random:seed(A,B,C),
    lists:map(fun(_) -> element(random:uniform(16), ?CHARS) end,
	      lists:seq(1,20)).
