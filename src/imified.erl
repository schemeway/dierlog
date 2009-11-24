%%% File    : imified.erl
%%% Author  : Dominique Boucher
%%% Description : Simplified interface to IMified


-module(imified).

-export([out/1, handle_msg/1]).

-export([reset/1, goto/2, error/1, line/1]).

-include("imified_api.hrl").
-include("yaws_api.hrl").


out(Arg) ->
    Options = Arg#arg.opaque,
    HandlingModule = handler_module(Options),
    Handler = {HandlingModule, handle_msg},
    Msg = extract_msg(Arg),
    Response =  case (catch {ok, Handler(Msg, Options)}) of
		    {ok, Val} -> Val;
		    Exception -> 
			error_logger:error_msg("An exception occurred: ~p~n", [Exception]),
			error("An internal error occurred. Please contact the support team.")
		end,
    {ehtml, Response}.


handler_module(Options) ->
    case lists:keysearch("im_app", 1, Options) of
	{value, {_, Value}} ->
	    list_to_atom(Value);
	_ ->
	    ?MODULE
    end.


handle_msg(_) ->
    error("Invalid IMbot module. Contact the system administrator.").
				    

extract_msg(Arg) ->
    {ok, Step} = yaws_api:postvar(Arg, "step"),
    {ok, Msg} = yaws_api:postvar(Arg, "msg"),
    {ok, Network} = yaws_api:postvar(Arg, "network"),
    {ok, UserKey} = yaws_api:postvar(Arg, "userkey"),
    Values = collect_values(Arg),
    #im_msg{step    = list_to_integer(Step),
	    msg     = Msg,
	    network = Network,
	    userkey = UserKey,
	    values  = Values}.

collect_values(Arg) ->
    collect_values(Arg, 1).

collect_values(Arg, N) ->
    case yaws_api:postvar(Arg, "value" ++ integer_to_list(N)) of
	{ok, Val} ->
	    [Val | collect_values(Arg, N + 1)];
	_ ->
	    []
    end.


reset(Msg) ->
    Msg ++ "<reset>".

goto(Step, Msg) when is_integer(Step) ->
    Msg ++ "<goto=" ++ integer_to_list(Step) ++ ">".

error(Msg) ->
    Msg ++ "<error>".

line(Msg) ->
    Msg ++ "<br>".
