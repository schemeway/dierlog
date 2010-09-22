%%% File    : vxml_view.erl
%%% Author  : Dominique Boucher
%%% Description : VoiceXML view


-module(vxml_view).


-export([render/2]).

-include("dialog.hrl").

-define(SCRIPT, "//////  JavaScript utility functions.

function encodeHypotheses(hypotheses) {
  var result = new Array();
  for (key in hypotheses) {
    var hypothesis = hypotheses[key];
    result[key] = hypothesis.interpretation;
  }
  return JSON.stringify(result);			  
}

").


render(hangup, _) ->
    vxml([{disconnect, []}]);
render(no_module, _) ->
    vxml([{disconnect, []}]);
render(Interaction = #interaction{grammars=[]}, AppUrl) ->
    vxml(
      [{form, [{id, "start"}], 
	[{block, [],
	  [{prompt, [],
	    render_prompts(Interaction#interaction.prompts)},
	   {var, [{name, "result"}, {expr, "'next'"}]},
	   {submit, [{method, "POST"}, {namelist, "result"}, {next, AppUrl}]}]}]}]);
render(#record{prompts = Prompts, beep = Beep}, AppUrl) ->
    vxml(
      [{form, [{id, "start"}], 
	[{record, [{name, "recording"}, {type, "audio/wav"}, {maxtime, "500ms"}, {beep, true_value(Beep)}], 
	  [{prompt, [], render_prompts(Prompts)},
	   {'catch', [{event, "connection.disconnect.hangup"}],
            [{var, [{name, "result"}, {expr, "'hangup'"}]},
             {submit, [{method, "POST"}, {namelist, "result"}, {next, AppUrl}]}]},
           {'catch', [{event, "noinput"}],
            [{var, [{name, "result"}, {expr, "'event'"}]},
             {var, [{name, "name"}, {expr, "'noinput'"}]},
             {submit, [{method, "POST"}, {namelist, "result name"}, {next, AppUrl}]}]}]},
	{block, [],
	 [{var, [{name, "result"}, {expr, "'recording'"}]},
	  {submit, [{method, "POST"}, {namelist, "result recording"}, {next, AppUrl}]}]}]}]);
render(Interaction = #interaction{}, AppUrl) ->
    vxml(
      [{form, [{id, "start"}],
	[{field, [{name, "input"}],
	  [{prompt, [],
	    render_prompts(Interaction#interaction.prompts)}] ++
	  render_grammars(Interaction#interaction.grammars) ++
          [{filled, [],
	    [{'if', [{'cond', "input$.inputmode == 'dtmf'"}],
	      [{var, [{name, "result"}, {expr, "'dtmf'"}]},
	       {var, [{name, "value"}, {expr, "input$.utterance"}]},
	       {log, [{label, "debug"}],
		["dtmf value = ", {value, [{expr, "value"}]}]},
	       {submit, [{method, "POST"}, {namelist, "result value"}, {next, AppUrl}]},
	       {'else', []},
	       {var, [{name, "result"}, {expr, "'nbest'"}]},
	       {var, [{name, "hypotheses"}, {expr, "encodeHypotheses(application.lastresult$);"}]},
	       {log, [{label, "debug"}],
		["hypotheses = ", {value, [{expr, "hypotheses"}]}]},
	       {submit, [{method, "POST"}, {namelist, "result hypotheses"}, {next, AppUrl}]}]}]},
           {'catch', [{event, "connection.disconnect.hangup"}],
            [{var, [{name, "result"}, {expr, "'hangup'"}]},
             {submit, [{method, "POST"}, {namelist, "result"}, {next, AppUrl}]}]},
           {'catch', [{event, "noinput"}],
            [{var, [{name, "result"}, {expr, "'event'"}]},
             {var, [{name, "name"}, {expr, "'noinput'"}]},
             {submit, [{method, "POST"}, {namelist, "result name"}, {next, AppUrl}]}]},
           {'catch', [{event, "nomatch"}],
            [{var, [{name, "result"}, {expr, "'event'"}]},
             {var, [{name, "name"}, {expr, "'nomatch'"}]},
             {submit, [{method, "POST"}, {namelist, "result name"}, {next, AppUrl}]}]},
           {'catch', [{event, "."}],
            [{var, [{name, "result"}, {expr, "'error'"}]},
             {submit, [{method, "POST"}, {namelist, "result"}, {next, AppUrl}]}]}]}]}]).


vxml(Doc) ->
    ["\n" | XmlContent] = yaws_api:ehtml_expand(
			     {vxml, [{xmlns, "http://www.w3.org/2001/vxml"},
				     {'xmlns:xsi', "http://www.w3.org/2001/XMLSchema-instance"},
				     {'xsi:schemaLocation', "http://www.w3.org/2001/vxml http://www.w3.org/TR/voicexml20/vxml.xsd"},
				     {version, "2.1"}],
			      [{script, [],
				[?SCRIPT]}]
			      ++ Doc}),
    {content, "text/xml", "<?xml version='1.0' encoding='ISO-8859-1'?>\n" ++ lists:flatten(XmlContent)}.


true_value(true) -> "true";
true_value(_) -> "false".
    


render_prompts(Prompts) ->
    [render_prompt(Prompt) || Prompt <- Prompts].

render_prompt(#prompt{text=undefined, audio = Src}) ->
    {audio, [{src, Src}], [Src]};
render_prompt(#prompt{text = Txt, audio = undefined}) ->
    Txt ++ " ";
render_prompt(#prompt{text = Txt, audio = Src}) ->
    {audio, [{src, Src}], Txt}.



render_grammars(Urls) ->
    [{grammar, [{src, Url}]} || Url <- Urls].
