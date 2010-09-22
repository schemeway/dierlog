%%% File    : dialog_test.erl
%%% Author  : Dominique Boucher
%%% Description : Dialog API test


-module(dialog_test).

-define(MENU_GRAMMAR, "builtin:dtmf/digits?minlength=1&amp;maxlength=1").
-define(PIN_GRAMMAR, "builtin:dtmf/digits?minlength=4&amp;maxlength=6").
-define(NOINPUT,#event{name=noinput}).
-define(NOMATCH,#event{name=nomatch}).

-export([test/0, start_dialog/2]).
-import(dialog, [voicefile/1]).

-include("dialog.hrl").

start_dialog(SessionId, _Direction) ->
    io:format("Starting session: ~p~n", [SessionId]),
    dialog:throw_on_hangup(),
    try
	dialog:answer(),
	welcome()
    catch
	throw:hangup ->
	    io:format("  - Hangup trapped!~n", []),
	    finish()
    end,
    io:format("Finished session: ~p~n", [SessionId]).



welcome() ->
    case dialog:play([#prompt{text = "Welcome to my demo application!",
			      audio = voicefile("welcome.wav")}]) of
	?NOINPUT   -> change_language();
	next       -> change_language()
    end.

change_language() ->
    case dialog:ask([#prompt{text = "For service in French, press 9.",
			     audio = voicefile("changelanguage.wav")}],
		    [?MENU_GRAMMAR]) of
	#dtmf{string = "9"}  -> dialog:set_language("fr"),
			        ask_pin(initial);
	#dtmf{string = _}    -> ask_pin(initial);
	?NOINPUT             -> ask_pin(initial)
    end.

ask_pin(Context) ->
    Prefix = case Context of
		  initial -> [];
		  noinput -> [#prompt{text = "Please answer the question.",
				      audio = voicefile("pleaseanswer.wav")}];
		  nomatch -> [#prompt{text = "I did not understand.",
				      audio = voicefile("didnotunderstand.wav")}]
	      end,
    case dialog:ask(Prefix ++ [#prompt{text = "What is your pin?",
				       audio = voicefile("pin.wav")}], 
		    [?PIN_GRAMMAR]) of
	#dtmf{string = Pin} ->
	    dialog:play([#prompt{text = "Thanks, your pin is " ++ Pin ++ "."}]),
	    record();
	?NOINPUT ->
	    ask_pin(noinput);
	?NOMATCH ->
	    ask_pin(nomatch)
    end.

record() ->
    case dialog:record([#prompt{text = "Record your message after the beep."}], [beep, {maxtime, 500}]) of
	#recording{} ->
	    dialog:play([#prompt{text = "Thanks. Bye."}]),
	    dialog:hangup(),
	    finish();
	?NOINPUT ->
	    dialog:play([#prompt{text = "Bye."}]),
	    dialog:hangup(),
	    finish();
	_ ->
	    dialog:hangup(),
	    finish()
    end.
	    

finish() ->				       
    ok.




test() ->
    dialog_sessions:start_link(),
    Pid = dialog_ctl:start("test", fun start_dialog/2, inbound),

    dialog_ctl:viewer_send(Pid, answer),
    #interaction{prompts=[#prompt{text="Welcome to my demo application!"}]}
	= receive_msg(Pid),

    dialog_ctl:viewer_send(Pid, next),
    #interaction{prompts=[#prompt{text="What is your pin?"}],
		 grammars=[?PIN_GRAMMAR]} 
	= receive_msg(Pid),

    dialog_ctl:viewer_send(Pid, #event{name=nomatch}),
    #interaction{prompts=[#prompt{text="I did not understand."},
			  #prompt{text="What is your pin?"}],
		 grammars=[?PIN_GRAMMAR]}
	= receive_msg(Pid),

    dialog_ctl:viewer_send(Pid, #dtmf{string = ["123456"]}),
    #interaction{prompts=[#prompt{text="Thanks, your pin is 123456."}]}
	= receive_msg(Pid),

    dialog_ctl:viewer_send(Pid, ok),
    hangup = dialog_ctl:viewer_receive(Pid).


receive_msg(Pid) ->
    Msg = dialog_ctl:viewer_receive(Pid),
    Msg.


