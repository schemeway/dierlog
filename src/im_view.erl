%%% File    : im_view.erl
%%% Author  : Dominique Boucher
%%% Description : Rendering of interactions for the IMified platform

-module(im_view).

-export([render/2]).

-include("dialog.hrl").


render(hangup, _) ->
    {ehtml, json:encode({struct, [{type, "hangup"}, {text, "bye"}]})};
render({error, timeout}, _) ->
    {ehtml, json:encode({struct, [{type, "hangup"}, {text, "Oups, got a timeout"}]})};
render(no_module, _) ->
    {ehtml, json:encode({struct, [{type, "hangup"}, {text, "Oups, dialog module not properly configured"}]})};
render(Interaction = #interaction{grammars = []}, _AppUrl) ->
    {ehtml, json:encode({struct, [{type, "message"}, {text, render_prompts(Interaction#interaction.prompts)}]})};
render(Interaction = #interaction{}, _AppUrl) ->
    {ehtml, json:encode({struct, [{type, "question"}, {text, render_prompts(Interaction#interaction.prompts)}]})}.


render_prompts(Prompts) ->
    lists:flatten(lists:map(fun render_prompt/1, Prompts)).

render_prompt(#prompt{text = undefined, audio = Src}) ->
    {a, [{href, Src}], Src};
render_prompt(#prompt{text = Text, audio = undefined}) ->
    Text;
render_prompt(#prompt{text = Text, audio = Src}) ->
    {a, [{href, Src}], Text}.



