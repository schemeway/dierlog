%% Include file for imified.erl and client applications

-record(im_msg, {userkey, network, msg, values = [], step = 1}).
-record(im_account, {botkey, username, password}).
-record(im_user, {name, key}).
