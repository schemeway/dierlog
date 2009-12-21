%%% File    : dialog_test.erl
%%% Author  : Dominique Boucher
%%% Description : Dialog API test


-module(dialog_test).


-export([test/0, start_dialog/2]).

-include("dialog.hrl").

start_dialog(_SessionId, _Direction) ->
    dialog:answer(),
    welcome().


welcome() ->
    case dialog:play([#tts{text = "Welcome to my demo application!"}]) of
	#event{name=noinput} -> ask_postalcode();
	next                 -> ask_postalcode();
	hangup               -> finish()
    end.

ask_postalcode() ->
    case dialog:ask([#tts{text = "What is your postal code?"}],
		    ["postalcode.abnf"]) of
	#text{string=PostalCode} -> dialog:play([#tts{text = "Thanks, your postal code is " ++ PostalCode}]),
				    dialog:hangup(),
				    finish();
	#event{name=noinput}     -> retry_postalcode(noinput);
	#event{name=nomatch}     -> retry_postalcode(nomatch);
	#event{name=hangup}      -> finish();
	_Error                   -> dialog:hangup(), finish()
    end.


retry_postalcode(ErrorType) ->
    FirstPrompt = case ErrorType of
		      noinput -> "Please answer the question.";
		      nomatch -> "I did not understand.";
		      _       -> "ERROR"
		  end,
    case dialog:ask([#tts{text = FirstPrompt},
		     #tts{text = "What is your postal code?"}],
		    ["postalcode.abnf"]) of
	#text{string=PostalCode} -> dialog:play([#tts{text = "Thanks, your postal code is " ++ PostalCode}]),
				    dialog:hangup(),
				    finish();
	#event{name=noinput}     -> retry_postalcode(noinput);
	#event{name=nomatch}     -> retry_postalcode(nomatch);
	#event{name=hangup}      -> finish();
	_Error                   -> dialog:hangup(), finish()
    end.

finish() ->				       
    io:format("Done!~n"),
    ok.




test() ->
    dialog_sessions:start_link(),
    Pid = dialog_ctl:start("test", fun start_dialog/2, inbound),

    dialog_ctl:viewer_send(Pid, answer),
    #interaction{prompts=[#tts{text="Welcome to my demo application!"}]}
	= receive_msg(Pid),
    dialog_ctl:viewer_send(Pid, next),

    #interaction{prompts=[#tts{text="What is your pin?"}],
		 grammars=["postalcode.abnf"]} 
	= receive_msg(Pid),
    dialog_ctl:viewer_send(Pid, #event{name=nomatch}),

    #interaction{prompts=[#tts{text="I did not understand."},
			  #tts{text="What is your pin?"}],
		 grammars=["postalcode.abnf"]}
	= receive_msg(Pid),
    dialog_ctl:viewer_send(Pid, #text{string = "123456"}),

    #interaction{prompts=[#tts{text="Thanks, your pin is 123456"}]}
	= receive_msg(Pid),
    dialog_ctl:viewer_send(Pid, ok),

    hangup = dialog_ctl:viewer_receive(Pid),
    dialog_sessions:stop().


receive_msg(Pid) ->
    Msg = dialog_ctl:viewer_receive(Pid),
    Msg.


