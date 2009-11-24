%%% File    : im_dialog.erl
%%% Author  : Dominique Boucher
%%% Description : IM dialog abstraction level


-module(im_dialog).

-include("dialog.hrl").


-export([process_message/3, start_dialog/3]).


process_message(SessionId, Message, DialogModule) ->
    case dialog_sessions:session_process(SessionId) of
	undefined ->
	    start_dialog(SessionId, inbound, DialogModule);
	Pid ->
	    dialog_ctl:viewer_send(Pid, msg_to_result(Message)),
	    get_output(Pid)
    end.


start_dialog(SessionId, Direction, DialogModule) ->
    Pid = dialog_ctl:start(SessionId, {DialogModule, start_dialog}, Direction),
    dialog_ctl:viewer_send(Pid, answer),
    get_output(Pid).

get_output(Pid) ->
    PromptsSets = lists:reverse(collect_prompts(Pid)),
    lists:flatten(render_prompts(PromptsSets)).


msg_to_result(Message) ->
    case Message of
	""  -> #event{name = noinput};
	Msg -> #text{string = Msg}
    end.


collect_prompts(Pid) ->
    collect_prompts(Pid, []).

collect_prompts(Pid, CollectedPrompts) ->
    case dialog_ctl:viewer_receive(Pid) of
	hangup ->
	    [#tts{text = "Bye."} | CollectedPrompts];
	#interaction{prompts = Prompts, grammars = []} ->
	    dialog_ctl:viewer_send(Pid, next),
	    collect_prompts(Pid, Prompts ++ CollectedPrompts);
	#interaction{prompts = Prompts} ->
	    Prompts ++ CollectedPrompts;
	_ ->
	    [error | CollectedPrompts]
    end.


render_prompts(Prompts) ->
    lists:map(fun render/1, Prompts).

render(hangup) ->
    imified:reset("Bye.");
render(error) ->
    imified:error("Internal error");
render(#tts{text = Text}) ->
    Text ++ " ";
render(#audio{url = Url}) ->
    Url ++ " ".
