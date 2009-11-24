%%% File    : imified_api.erl
%%% Author  : Dominique Boucher
%%% Description : IMified API implementation


-module(imified_api).

-include("imified_api.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([get_users/1, check_availability/2, send_message/3]).

-define(IMIFIED_URL, "http://www.imified.com/api/bot/").


%%
%% Retrieves all the users of an IMified bot. Returns an XML element.
%%

get_users(Account = #im_account{}) ->
    case http:request(post,
		      {?IMIFIED_URL, 
		       [{"Authorization", auth_string(Account)}], 
		       "application/x-www-form-urlencoded",
		       "apimethod=getAllUsers&botkey=" ++ Account#im_account.botkey}, 
		      [{timeout, 5000}],
		      []) of
	{ok, {{_,200,_}, _Headers, Data}} ->
	    {RootElement, _} = xmerl_scan:string(Data),
	    RootElement;
	_ ->
	    failed
    end.


%%
%% Checks the availability of a User. Returns 'offline' or {online, #im_user{}}
%%

check_availability(Account = #im_account{}, User) ->
    case get_users(Account) of
	failed ->
	    failed;
	Users ->
	    case xmerl_xpath:string("//user[user = '" ++ User ++ "' and status = 'online']", Users) of
		[] ->
		    offline;
		[Node | _] ->
		    {online, #im_user{name = text_value("/user/user/text()", Node),
				      key  = text_value("/user/userkey/text()", Node)}}
	    end
    end.


%%
%% Sends a message to a IMbot user
%%


send_message(Account = #im_account{}, User = #im_user{}, Text) ->
    case http:request(post,
		      {?IMIFIED_URL, 
		       [{"Authorization", auth_string(Account)}], 
		       "application/x-www-form-urlencoded",
		       "apimethod=send&botkey=" ++ Account#im_account.botkey
		       ++ "&userkey=" ++ User#im_user.key
		       ++ "&msg=" ++ yaws_api:url_encode(Text)}, 
		      [{timeout, 5000}],
		      []) of
	{ok, {{_,200,_}, _Headers, _Data}} ->
	    ok;
	_ ->
	    failed
    end.


text_value(Path, Xml) ->
    [Node] = xmerl_xpath:string(Path, Xml),
    Node#xmlText.value.

auth_string(Account = #im_account{}) ->
    "Basic " ++ binary_to_list(base64:encode(Account#im_account.username ++ ":" ++ Account#im_account.password)).
    
    
