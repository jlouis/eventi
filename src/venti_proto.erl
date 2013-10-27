%%% venti_proto implements the venti low level binary protocol
-module(venti_proto).

-export([
	decode_t/1,
	decode_r/1,
	decode_packet_t/1,
	decode_packet_r/1
	]).

%% Message types in Venti (plan9port)
-define(VtRerror, 1).
-define(VtTping, 2).
-define(VtRping, 3).
-define(VtThello, 4).
-define(VtRhello, 5).
-define(VtTgoodbye, 6).
-define(VtRgoodbye, 7). % Not used
-define(VtTauth0 , 8).
-define(VtRauth0, 9).
-define(VtTauth1, 10).
-define(VtRauth1, 11).
-define(VtTread, 12).
-define(VtRread, 13).
-define(VtTwrite, 14).
-define(VtRwrite, 15).
-define(VtTsync, 16).
-define(VtRsync, 17).

%% decode/1 decodes a message including 2 byte size header
decode_t(<<L:16/integer, P:L/binary>>) ->
	decode_packet_t(P).

decode_r(<<L:16/integer, P:L/binary>>) ->
	decode_packet_r(P).
	
%% decode_packet/1 decodes a message when the two byte size header has been stripped off
decode_packet_t(<<?VtThello:8/integer, Tag:8/integer, Rest/binary>>) ->
	{Version, RestVersion} = decode_string(Rest),
	{Uid, RestUid} = decode_string(RestVersion),
	<<Strength:8/integer, RestStrength/binary>> = RestUid,
	{Crypto, RestCrypto} = decode_parameter(RestStrength),
	{Codec, RestCodec} = decode_parameter(RestCrypto),
	<<>> = RestCodec,
	{t_hello, Tag, Version, Uid, Strength, Crypto, Codec};
decode_packet_t(<<?VtTping:8/integer, Tag:8/integer>>) ->
	{tping, Tag};
decode_packet_t(<<?VtTread:8/integer, Tag:8/integer, Score:20/binary, Type:8/integer, Pad:8/integer, Count:16/integer>>) ->
	{t_read, Tag, Score, Type, Pad, Count};
decode_packet_t(<<?VtTwrite:8/integer, Tag:8/integer, Type:8/integer, Pad:3/binary, Data/binary>>) ->
	{t_write, Tag, Type, Pad, Data};
decode_packet_t(<<?VtTsync, Tag>>) ->
	{t_sync, Tag};
decode_packet_t(<<?VtTgoodbye, Tag>>) ->
	{t_goodbye, Tag}.

decode_packet_r(<<?VtRhello:8/integer, Tag:8/integer, SidL:16/integer, Sid:SidL/binary, RCrypto:8/integer, RCodec:8/integer>>) ->
	{r_hello, Tag, Sid, RCrypto, RCodec};
decode_packet_r(<<?VtRping:8/integer, Tag:8/integer>>) ->
	{r_ping, Tag};
decode_packet_r(<<?VtRread:8/integer, Tag:8/integer, Data/binary>>) ->
	{r_read, Tag, Data};
decode_packet_r(<<?VtRwrite:8/integer, Tag:8/integer, Score:20/binary>>) ->
	{r_write, Tag, Score};
decode_packet_r(<<?VtRsync, Tag>>) ->
	{r_sync, Tag};
decode_packet_r(<<?VtRerror, Tag, ErrL:16/integer, Err:ErrL/binary>>) ->
	{r_error, Tag, Err}.

	
decode_string(<<L:16/integer, Str:L/binary, Rest/binary>>) -> {Str, Rest}.
decode_parameter(<<L:8/integer, Param:L/binary, Rest/binary>>) -> {Param, Rest}.