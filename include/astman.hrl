%%% File    : astman.hrl
%%% Author  : Dominique Boucher <>
%%% Description : Record definitions for the Asterisk Manager API
%%% Created : 10 Jul 2009 by Dominique Boucher <>


-record(ami_account, {hostname = "localhost", port = 5038, username, secret}).

-record(originate_data, {channel, extension, context, priority = "1"}).

