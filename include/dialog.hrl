%%% File    : dialog.hrl
%%% Author  : Dominique Boucher <>
%%% Description : Voice API
%%% Created : 26 Jun 2009 by Dominique Boucher <>

-record(interaction_options, {timeout = 30 * 1000, interpret_text = false}).

-record(interaction, {prompts, grammars = [], options = #interaction_options{}}).

-record(tts,   {text}).
-record(audio, {url}).

-record(dtmf,  {string}).
-record(nbest, {values = []}).
-record(event, {name, message = ""}).
-record(text,  {string}).
