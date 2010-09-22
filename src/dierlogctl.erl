%%% File    : dierlogctl.erl
%%% Author  : Dominique Boucher
%%% Description : Controller or command-line tool

-module(dierlogctl).


-export([start/0]).


start() ->
    process(init:get_plain_arguments()),
    init:stop().


process(["status", NodeName]) ->
    Node = list_to_atom(NodeName),
    case net_kernel:connect_node(Node) of
	true ->
	    io:format("dierlog is running~n", []);
	_ ->
	    io:format("dierlog is not running~n", [])
    end;

process(["start", NodeName]) ->
    Node = list_to_atom(NodeName),
    case net_kernel:connect_node(Node) of
	true ->
	    io:format("dierlog is already running~n", []);
	_ ->
	    Erl = os:find_executable("erl"),
	    Cookie = atom_to_list(erlang:get_cookie()),
	    YawsEbin = case code:where_is_file("yaws.beam") of
			   non_existing ->
			       io:format("** Warning: unable to find the 'yaws' ebin directory~n", []),
			       ".";
			   File ->
			       filename:dirname(File)
		       end,
	    DierlogEbin = case code:where_is_file("dierlog.beam") of
			      non_existing ->
				  io:format("** Warning: unable to find the 'dierlog' ebin directory~n", []),
				  ".";
			      File2 ->
				  filename:dirname(File2)
			  end,
	    Path = " -pa " ++ YawsEbin ++ " -pa " ++ DierlogEbin,
	    CmdLine = Erl ++ " -sname " ++ NodeName ++ Path ++ " -noinput -detached -boot start_sasl -setcookie " ++ Cookie ++ " -s dierlog",
	    os:cmd(CmdLine),
	    ok
    end;

process(["stop", NodeName]) ->
    Node = list_to_atom(NodeName),
    case net_kernel:connect_node(Node) of
	true ->
	    rpc:call(Node, dierlog, stop, []);
	_ ->
	    io:format("dierlog is not running~n", [])
    end;

process(["restart", NodeName]) ->
    process(["stop", NodeName]),
    timer:sleep(2000),
    process(["start", NodeName]);


process(_) ->
    io:format("Usage: dierlog [start|status|stop|restart]~n").
