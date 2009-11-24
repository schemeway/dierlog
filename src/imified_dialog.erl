%%% File    : imified_test.erl
%%% Author  : Dominique Boucher
%%% Description : IMbot high-level API


-module(imified_dialog).


-export([handle_msg/2, start_outbound/3]).


-include("imified_api.hrl").

handle_msg(Request, Options) ->
    SessionId = Request#im_msg.userkey,
    DialogModule = dierlog:dialog_module(Options),
    Message = Request#im_msg.msg,
    im_dialog:process_message(SessionId, Message, DialogModule).


start_outbound(Account, User, DialogModule) ->
    Output = im_dialog:start_dialog(User#im_user.key, outbound, DialogModule),
    imified_api:send_message(Account, User, Output).
