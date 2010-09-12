%%% File    : dialog_test.erl
%%% Author  : Dominique Boucher
%%% Description : Dialog API test


-module(dialog_test).

-define(PIN_GRAMMAR, "builtin:dtmf/digits?minlength=3&amp;maxlength=8").

-export([test/0, start_dialog/2]).

-include("dialog.hrl").

start_dialog(_SessionId, _Direction) ->
    dialog:answer(),
    welcome().


welcome() ->
    case dialog:play([#tts{text = "Welcome to my demo application!"}]) of
	#event{name=noinput} -> ask_pin(initial);
	next                 -> ask_pin(initial);
	hangup               -> finish()
    end.

ask_pin(Context) ->
    Prefix = case Context of
		  initial -> [];
		  noinput -> [#tts{text = "Please answer the question."}];
		  nomatch -> [#tts{text = "I did not understand."}]
	      end,
    case dialog:ask(Prefix ++ [#tts{text = "What is your pin?"}], 
		    [?PIN_GRAMMAR]) of
	#nbest{values = [Pin]} ->
	    dialog:play([#tts{text = "Thanks, your pin is " ++ Pin}]),
	    dialog:hangup(),
	    finish();
	#event{name=noinput}     -> ask_pin(noinput);
	#event{name=nomatch}     -> ask_pin(nomatch);
	#event{name=hangup}      -> finish();
	Error                    -> 
	    dialog:hangup(), 
	    finish()
    end.

finish() ->				       
    ok.




test() ->
    dialog_sessions:start_link(),
    Pid = dialog_ctl:start("test", fun start_dialog/2, inbound),

    dialog_ctl:viewer_send(Pid, answer),
    #interaction{prompts=[#tts{text="Welcome to my demo application!"}]}
	= receive_msg(Pid),

    dialog_ctl:viewer_send(Pid, next),
    #interaction{prompts=[#tts{text="What is your pin?"}],
		 grammars=[?PIN_GRAMMAR]} 
	= receive_msg(Pid),

    dialog_ctl:viewer_send(Pid, #event{name=nomatch}),
    #interaction{prompts=[#tts{text="I did not understand."},
			  #tts{text="What is your pin?"}],
		 grammars=[?PIN_GRAMMAR]}
	= receive_msg(Pid),

    dialog_ctl:viewer_send(Pid, #nbest{values = ["123456"]}),
    #interaction{prompts=[#tts{text="Thanks, your pin is 123456"}]}
	= receive_msg(Pid),

    dialog_ctl:viewer_send(Pid, ok),
    hangup = dialog_ctl:viewer_receive(Pid).


receive_msg(Pid) ->
    Msg = dialog_ctl:viewer_receive(Pid),
    Msg.


