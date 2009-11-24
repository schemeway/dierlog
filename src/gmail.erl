%%% File    : gmail.erl
%%% Author  : Dominique Boucher <>
%%% Description : Sending mail via an GMail account
%%% Created : 14 Jun 2009 by Dominique Boucher <>

-module(gmail).
-export([send/4]).


send({Account, Password}, To, Subject, Content) ->
    {ok, Socket} = ssl:connect("smtp.gmail.com", 465, [{active, false}], 1000),
    recv(Socket),
    send(Socket, "HELO"),
    send(Socket, "AUTH LOGIN"),
    send(Socket, binary_to_list(base64:encode(Account))),
    send(Socket, binary_to_list(base64:encode(Password))),
    send(Socket, "MAIL FROM: <" ++ Account ++ ">"),
    send(Socket, "RCPT TO:<" ++ To ++ ">"),
    send(Socket, "DATA"),
    send_no_receive(Socket, "From: <" ++ Account ++ ">"),
    send_no_receive(Socket, "To: <" ++ To ++ ">"),
    send_no_receive(Socket, "Date: " ++ httpd_util:rfc1123_date()),
    send_no_receive(Socket, "Subject: " ++ Subject),
    send_no_receive(Socket, ""),
    send_no_receive(Socket, Content),
    send_no_receive(Socket, ""),
    send(Socket, "."),
    send(Socket, "QUIT"),
    ssl:close(Socket).

send_no_receive(Socket, Data) when is_binary(Data) ->
    ssl:send(Socket, Data),
    ssl:send("\r\n");
send_no_receive(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n").


send(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n"),
    recv(Socket).

recv(Socket) -> 
    case ssl:recv(Socket, 0, 1000) of
	{ok, _Return} ->
	    ok;
	{error, Reason} ->
	    io:format("ERROR: ~p~n", [Reason])
    end.


