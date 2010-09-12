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
render(Interaction = #interaction{}, AppUrl) ->
    vxml(
      [{form, [{id, "start"}],
	[{field, [{id, "name"}],
	  [{prompt, [],
	    render_prompts(Interaction#interaction.prompts)}] ++
	  render_grammars(Interaction#interaction.grammars) ++
          [{filled, [],
	    [{'if', [{'cond', "application.lastresult$.inputmode == 'dtmf'"}],
	      [{var, [{name, "result"}, {expr, "'dtmf'"}]},
	       {var, [{name, "value"}, {expr, "application.lastresult$.utterance"}]},
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



render_prompts([]) ->
    "";
render_prompts([#tts{text = Text} | Prompts]) ->
    [Text ++ " " | render_prompts(Prompts)];
render_prompts([#audio{url = Url} | Prompts]) ->
    [{audio, [{src, Url}]} | render_prompts(Prompts)].

render_grammars([]) ->
    [];
render_grammars([Url | Urls]) ->
    [{grammar, [{src, Url}]} | render_grammars(Urls)].
