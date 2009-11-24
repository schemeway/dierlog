%%% File    : dialog.erl
%%% Author  : Dominique Boucher
%%% Description : Dialog API


-module(dialog).

-export([play/1, play/2, collect/1, ask/2, ask/3, hangup/0, answer/0, nugram_session/0]).

-include("dialog.hrl").
-include("nugramserver.hrl").

-define(MAIN_GRAMMAR, "dierlog_main.abnf").

%%====================================================================
%% API
%%====================================================================

collect(Interaction = #interaction{}) ->
    dialog_ctl:client_send(Interaction),
    dialog_ctl:client_receive().


play(Prompts) ->
    play(Prompts, #interaction_options{}).
play(Prompts, Options) ->
    collect(#interaction{prompts = Prompts, 
			 grammars = [],
			 options = Options}).

ask(Prompts, Grammars) ->
    ask(Prompts, Grammars, #interaction_options{}).
ask(Prompts, Grammars, Options) ->
    Result = collect(#interaction{prompts = Prompts,
				  grammars = Grammars,
				  options = Options}),
    interpret_text(Result, Grammars, Options#interaction_options.interpret_text).

hangup() ->
    dialog_ctl:client_send(hangup),
    dialog_ctl:stop(),
    stop_nugram_session(get({?MODULE, nugram_session})),
    done.

answer() ->
    answer = dialog_ctl:client_receive().

nugram_session() ->
    case get({?MODULE, nugram_session}) of
	undefined ->
	    start_nugram_session(dierlog:nugram_account());
	NuGramSession ->
	    NuGramSession
    end.

start_nugram_session({Account, Password}) ->
    NuGramSession = nugramserver:create_session(Account, Password),
    put({?MODULE, nugram_session}, NuGramSession),
    NuGramSession.

stop_nugram_session(Session = #gserver_session{}) ->
    nugramserver:disconnect(Session);
stop_nugram_session(_) ->
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

interpret_text(#text{string = Answer}, Grammars = [_ | _], true) ->
    NuGramSession = nugram_session(),
    Grammar = nugramserver:instantiate(NuGramSession, ?MAIN_GRAMMAR, [{grammars, Grammars}]),
    case nugramserver:interpret(Grammar, Answer) of
	false ->
	    #event{name=nomatch};
	Interpretation ->
	    #nbest{values = tuple_to_list(Interpretation)}
    end;
interpret_text(Result, _, _) ->
    Result.


