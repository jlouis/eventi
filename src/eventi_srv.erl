-module(eventi_srv).

-export([
	handle_msg/1
	]).
	
%% handle_msg/1 handles Venti client messages on the server side
-spec handle_msg(Request) -> {reply, Reply} | {stop, Reason}
	when
	  Request :: eventi_proto:t_msg(),
	  Reply :: eventi_proto:r_msg(),
	  Reason :: term().

handle_msg({t_hello, Tag, eventi_02, _Uid, _Strength, _Crypto, _Codec}) ->
	{reply, {r_hello, Tag, <<"eventi">>, 0,0}};
handle_msg({t_ping, Tag}) -> {reply, {r_ping, Tag}};
handle_msg({t_goodbye, _Tag}) -> {stop, goodbye}.

