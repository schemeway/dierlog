%%% File    : astman.erl
%%% Author  : Dominique Boucher
%%% Description : Asterisk Manager Interface

-module(astman).

-export([originate/2]).
-include("astman.hrl").


originate(AmiAccount = #ami_account{}, Data = #originate_data{}) ->
    {ok, Socket} = gen_tcp:connect(AmiAccount#ami_account.hostname,
				   AmiAccount#ami_account.port,
				   [{active, false}],
				   1000),
    {ok, "Asterisk Call Manager" ++ _} = gen_tcp:recv(Socket, 0),
    gen_tcp:send(Socket, 
		 "Action: login\r\nUsername: " ++ AmiAccount#ami_account.username 
		 ++ "\r\nSecret: " ++ AmiAccount#ami_account.secret
		 ++ "\r\nEvents: off\r\n\r\n"),
    {ok, "Response: Success" ++ _} = gen_tcp:recv(Socket, 0),
    gen_tcp:send(Socket, 
		 "Action: originate\r\nChannel: " ++ Data#originate_data.channel
		 ++ "\r\nExten: " ++ Data#originate_data.extension
		 ++ "\r\nContext: " ++ Data#originate_data.context
		 ++ "\r\nPriority: " ++ Data#originate_data.priority
		 ++ "\r\n\r\n"),
    OriginateAnswer = gen_tcp:recv(Socket, 0),
    gen_tcp:send(Socket, 
		 "Action: Logoff\r\n\r\n"),
    gen_tcp:close(Socket),
    case OriginateAnswer of
	{ok, "Response: Success" ++ _} ->
	    ok;
	{ok, "Response: " ++ _} ->
	    failed;
	_ ->
	    io:format("answer = ~p~n", OriginateAnswer),
	    failed
    end.
