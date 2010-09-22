%%% File    : dialog.hrl
%%% Author  : Dominique Boucher <>
%%% Description : Voice API
%%% Created : 26 Jun 2009 by Dominique Boucher <>

-record(interaction_options, {timeout = 30 * 1000, interpret_text = false}).

-record(interaction, {prompts, grammars = [], options = #interaction_options{}}).

-record(prompt,   {audio = undefined, text = undefined}).

-record(record,   {prompts = [], beep = false, maxtime = 3000, finalsilence = 3000, dtmfterm = true}).

-record(dtmf,  {string}).
-record(nbest, {values = []}).
-record(event, {name, message = ""}).
-record(recording, {attributes = []}).
