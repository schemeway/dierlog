%%% File    : dialog.erl
%%% Author  : Dominique Boucher
%%% Description : Dialog API


-module(dialog).

-export([play/1, play/2, collect/1, ask/2, ask/3, hangup/0, answer/0, nugram_session/0]).
-export([record/2]).
-export([language/0, set_language/1, app/0, set_app/1]).
-export([voicefile/1, grammarfile/1]).

-export([throw_on_hangup/0]).

-include("dialog.hrl").
-include("nugramserver.hrl").

-define(MAIN_GRAMMAR, "dierlog_main.abnf").

%%====================================================================
%% API
%%====================================================================

collect(Interaction = #interaction{}) ->
    dialog_ctl:client_send(Interaction),
    check_hangup(dialog_ctl:client_receive()).


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

record(Prompts, Options) ->
    Record = parse_record_options(Options, #record{prompts = Prompts}),
    dialog_ctl:client_send(Record),
    check_hangup(dialog_ctl:client_receive()).


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


language() ->
    case get({dialog, language}) of
	undefined ->
	    "en";
	Lang ->
	    Lang
    end.

set_language(Lang) ->
    put({dialog, language}, Lang).

app() ->
    case get({dialog, app}) of
	undefined ->
	    "app";
	App ->
	    App
    end.

set_app(Name) ->
    put({dialog, app}, Name).


voicefile(Name) ->
    "/" ++ app() ++ "/prompts/" ++ language() ++ "/" ++ Name.

grammarfile(Name) ->
    "/" ++ app() ++ "/grammars/" ++ language() ++ "/" ++ Name.


throw_on_hangup() ->
    put({dialog, throw_on_hangup}, true).

check_hangup(Result) ->
    case Result of
	hangup ->
	    case get({dialog, throw_on_hangup}) of
		true ->
		    throw(hangup);
		_ ->
		    hangup
	    end;
	Other ->
	    Other
    end.


    


%%====================================================================
%% Internal functions
%%====================================================================

interpret_text(#nbest{values = [Answer]}, Grammars = [_ | _], true) when is_list(Answer) ->
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



parse_record_options([], Rec) ->
    Rec;
parse_record_options([beep|Options], Rec) ->
    parse_record_options(Options, Rec#record{beep = true});
parse_record_options([{maxtime, Val}|Options], Rec)
  when is_integer(Val) andalso Val >= 0 ->
    parse_record_options(Options, Rec#record{maxtime = Val});
parse_record_options([{finalsilence, Val}|Options], Rec)
  when is_integer(Val) andalso Val >= 0 ->
    parse_record_options(Options, Rec#record{finalsilence = Val}).
