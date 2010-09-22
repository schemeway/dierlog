%%% File    : html_view.erl
%%% Author  : Dominique Boucher 
%%% Description : HTML rendering of interactions


-module(html_view).

-export([render/2]).

-include("dialog.hrl").

-define(SCRIPT, "
var resulttype = undefined;
var eventname = undefined;
var eventmsg = undefined;

function updateForm(form) {
  form['result'].value = resulttype;
  form['name'].value = eventname;
}
").

render(hangup, _) ->
    {ehtml, "bye"};
render(no_module, _) ->
    {ehtml, "Oups, dialog module not properly configured"};
render(Interaction = #interaction{grammars=[]}, AppUrl) ->
    {ehtml, 
     {html, [], 
      [{script, [], ?SCRIPT},
       {h2, [], "Message interaction"},
       {h3, [], "prompts"}]
      ++ render_prompts(Interaction#interaction.prompts) ++ 
      [{form, [{action, AppUrl}, {method, "POST"}],
	[{br, []},
	 {input, [{type, "hidden"}, {name, "result"}, {value, "next"}]},
	 {input, [{type, "hidden"}, {name, "name"}]},
	 {input, [{type, "submit"}, {value, "Next"}, {onClick, "resulttype = 'event'; eventname = 'hangup'"}]},
	 {input, [{type, "submit"}, {value, "Hangup"},  {onClick, "resulttype = 'event', eventname = 'hangup'"}]}]}]}};
render(Interaction = #interaction{}, AppUrl) ->
    {ehtml,
     {html, [],
      [{script, [], ?SCRIPT},
       {h2, [], "Speech interaction"},
       {h3, [], "prompts"}]
      ++ render_prompts(Interaction#interaction.prompts) ++ 
      [{h3, [], "grammars"}] 
      ++ [{ul, [], 
	   render_grammars(Interaction#interaction.grammars)}] ++
      [{form, [{action, AppUrl}, {method, "POST"}, {onSubmit, "updateForm(this)"}],
	[{input, [{type, "hidden"}, {name, "result"}, {value, "text"}]},
	 {input, [{type, "hidden"}, {name, "name"}]},
	 {input, [{type, "text"}, {name, "value"}]},
	 {input, [{type, "submit"}, {value, "Answer"},  {onClick, "resulttype = 'text';"}]},
	 {input, [{type, "submit"}, {value, "noinput"}, {onClick, "resulttype = 'event'; eventname = 'noinput'"}]},
	 {input, [{type, "submit"}, {value, "nomatch"}, {onClick, "resulttype = 'event', eventname = 'nomatch'"}]},
	 {input, [{type, "submit"}, {value, "Hangup"},  {onClick, "resulttype = 'event', eventname = 'hangup'"}]}]}]}}.

render_prompts(Prompts) ->
    lists:flatten(lists:map(fun render_prompt/1, Prompts)).

render_prompt(#prompt{text = undefined, audio = Src}) ->
    {a, [{href, Src}], Src};
render_prompt(#prompt{text = Text, audio = undefined}) ->
    Text;
render_prompt(#prompt{text = Text, audio = Src}) ->
    {a, [{href, Src}], Text}.

render_grammars(Grammars) ->
    lists:map(fun(Grammar) ->
		      {li, [], Grammar}
	      end,
	      Grammars).
