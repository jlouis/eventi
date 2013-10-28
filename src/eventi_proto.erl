%%% venti_proto implements the venti low level binary protocol
-module(eventi_proto).

-export([
	decode_t/1,
	decode_r/1,
	decode_packet_t/1,
	decode_packet_r/1,

	encode_t/1,
	encode_r/1,
	encode_packet_t/1,
	encode_packet_r/1
	]).

-type t_msg() ::
	  {t_hello, byte(), binary(), binary(), byte(), binary(), binary()}
	| {t_ping, byte()}
	| {t_read, byte(), binary(), byte(), integer()}
	| {t_write, byte(), byte(), binary()}
	| {t_sync, byte()}
	| {t_goodbye, byte()}.

-type r_msg() ::
	  {r_hello, byte(), binary(), byte(), byte()}
	| {r_ping, byte()}
	| {r_read, byte(), binary()}
	| {r_write, byte(), binary()}
	| {r_sync, byte()}
	| {r_error, byte(), atom() | {string, binary()}}.

-export_type([
	t_msg/0, r_msg/0
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

%% DECODING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% decode_t/1 decodes a T-type packet including size header
-spec decode_t(binary()) -> t_msg().
decode_t(<<L:16/integer, P:L/binary>>) ->
	decode_packet_t(P).

%% decode_r/1 decodes a R-type packet including size header
-spec decode_r(binary()) -> r_msg().
decode_r(<<L:16/integer, P:L/binary>>) ->
	decode_packet_r(P).

%% decode_packet_t/1 decodes a T-message when the two byte size header has been stripped off
-spec decode_packet_t(binary()) -> t_msg().
decode_packet_t(<<?VtThello, Tag, Rest/binary>>) ->
	{Version, RestVersion} = decode_string(Rest),
	{Uid, RestUid} = decode_string(RestVersion),
	<<Strength, RestStrength/binary>> = RestUid,
	{Crypto, RestCrypto} = decode_parameter(RestStrength),
	{Codec, RestCodec} = decode_parameter(RestCrypto),
	<<>> = RestCodec,
	{t_hello, Tag, Version, Uid, Strength, Crypto, Codec};
decode_packet_t(<<?VtTping, Tag>>) ->
	{t_ping, Tag};
decode_packet_t(<<?VtTread, Tag, Score:20/binary, Type, _Pad, Count:16/integer>>) ->
	{t_read, Tag, Score, Type, Count};
decode_packet_t(<<?VtTwrite, Tag, Type, _Pad:3/binary, Data/binary>>) ->
	{t_write, Tag, Type, Data};
decode_packet_t(<<?VtTsync, Tag>>) ->
	{t_sync, Tag};
decode_packet_t(<<?VtTgoodbye, Tag>>) ->
	{t_goodbye, Tag}.

%% decode_packet_r/1 decodes a R-message when the two byte size header has been stripped off
-spec decode_packet_r(binary()) -> r_msg().
decode_packet_r(<<?VtRhello, Tag, SidL:16/integer, Sid:SidL/binary, RCrypto, RCodec>>) ->
	{r_hello, Tag, Sid, RCrypto, RCodec};
decode_packet_r(<<?VtRping, Tag>>) ->
	{r_ping, Tag};
decode_packet_r(<<?VtRread, Tag, Data/binary>>) ->
	{r_read, Tag, Data};
decode_packet_r(<<?VtRwrite, Tag, Score:20/binary>>) ->
	{r_write, Tag, Score};
decode_packet_r(<<?VtRsync, Tag>>) ->
	{r_sync, Tag};
decode_packet_r(<<?VtRerror, Tag, ErrL:16/integer, Err:ErrL/binary>>) ->
	{r_error, Tag, {string, Err}}.

decode_string(<<L:16/integer, Str:L/binary, Rest/binary>>) -> {Str, Rest}.
decode_parameter(<<L:8/integer, Param:L/binary, Rest/binary>>) -> {Param, Rest}.

%% ENCODING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec encode_t(t_msg()) -> iodata().
encode_t(Msg) ->
	Payload = encode_packet_t(Msg),
	Sz = iolist_size(Payload),
	true = Sz < 65536,
	[<<Sz:16/integer>>, Payload].

-spec encode_r(r_msg()) -> iodata().
encode_r(Msg) ->
	Payload = encode_packet_r(Msg),
	Sz = iolist_size(Payload),
	true = Sz < 65536,
	[<<Sz:16/integer>>, Payload].

encode_packet_t({t_hello, Tag, Version, Uid, Strength, Crypto, Codec}) ->
	[?VtThello, Tag, encode_string(Version), encode_string(Uid), Strength, encode_param(Crypto), encode_param(Codec)];
encode_packet_t({t_ping, Tag}) -> [?VtTping, Tag];
encode_packet_t({t_read, Tag, Score, Type, Count}) ->
	20 = byte_size(Score),
	[?VtTread, Tag, Score, Type, 0, <<Count:16/integer>>];
encode_packet_t({t_write, Tag, Type, Data}) ->
	[?VtTwrite, Tag, Type, 0, 0, 0, Data];
encode_packet_t({t_sync, Tag}) -> [?VtTsync, Tag];
encode_packet_t({t_goodbye, Tag}) -> [?VtTgoodbye, Tag].

encode_packet_r({r_hello, Tag, Sid, RCrypto, RCodec}) ->
	[?VtRhello, Tag, encode_string(Sid), RCrypto, RCodec];
encode_packet_r({r_ping, Tag}) -> [?VtRping, Tag];
encode_packet_r({r_read, Tag, Data}) -> [?VtRread, Tag, Data];
encode_packet_r({r_write, Tag, Score}) ->
	20 = byte_size(Score),
	[?VtRwrite, Tag, Score];
encode_packet_r({r_sync, Tag}) -> [?VtRsync, Tag];
encode_packet_r({r_error, Tag, Err}) -> [?VtRerror, Tag, encode_string(err_to_string(Err))].


err_to_string(count_exceeded) -> <<"Count Exceeded">>;
err_to_string(not_found) -> <<"Not Found">>.

encode_string(Str) ->
	Sz = byte_size(Str),
	true = Sz < 65536,
	<<Sz:16/integer, Str/binary>>.

encode_param(Param) ->
	Sz = byte_size(Param),
	true = Sz < 256,
	<<Sz, Param/binary>>.
