-module(eventi_srv).

-behaviour(gen_server).

-export([
	code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2
	]).
	
-define(TIMEOUT, 128 * 1000).

-export([
	start_link/0,
	handle_msg/1
	]).
	
-record(state, {
	db
	}).

%% API

%% start_link/0 starts up a new Venti instance
start_link() ->
	{ok, VentiDir} = application:get_env(eventi, dir),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [VentiDir], []).

%% handle_msg/1 handles Venti client messages on the server side
%% We try hard to avoid going through the gen_server if possible
-spec handle_msg(Request) -> {reply, Reply} | {stop, Reason}
	when
	  Request :: eventi_proto:t_msg(),
	  Reply :: eventi_proto:r_msg(),
	  Reason :: term().
handle_msg({t_hello, Tag, eventi_02, _Uid, _Strength, _Crypto, _Codec}) ->
	{reply, {r_hello, Tag, <<"eventi">>, 0,0}};
handle_msg({t_ping, Tag}) -> {reply, {r_ping, Tag}};
handle_msg({t_write, Tag, _Type, _Pad, Data}) ->
	Score = score(Data),
	ok = gen_server:call(?MODULE, {write, Score, Data}, ?TIMEOUT),
	{reply, {r_write, Tag, Score}};
handle_msg({t_read, Tag, Score, _Type, _Pad, Count}) ->
	case gen_server:call(?MODULE, {read, Score}, ?TIMEOUT) of
		{ok, Data} when byte_size(Data) =< Count ->
		  {reply, {r_read, Tag, Data}};
		{ok, _Data} ->
		  {reply, {r_error, Tag, <<"Count Exceeded">>}};
		not_found ->
		  {reply, {r_error, Tag, <<"Data not found">>}}
	end;
handle_msg({t_sync, Tag}) ->
	%% Current setup makes every call sync for simplicity
	{reply, {r_sync, Tag}};
handle_msg({t_goodbye, _Tag}) -> {stop, goodbye}.

%% Callbacks

init([VentiDir]) ->
	{ok, DBRef} = eleveldb:open(VentiDir, [{create_if_missing, true}]),
	{ok, #state{ db = DBRef }}.
	
handle_call({write, Score, Data}, _From, #state { db = Db } = State) ->
	ok = eleveldb:write(Db, [{put, Score, Data}], [{sync, true}]),
	{reply, ok, State};
handle_call({read, Score}, _From, #state { db = Db } = State) ->
	R = eleveldb:read(Db, Score, [{verify_checksums, true}, {fill_cache, true}]),
	{reply, R, State};
handle_call(Msg, _From, State) ->
	_ = lager:error("Unknown call: ~p", [Msg]),
	{reply, {error, unknown}, State}.
	
handle_cast(Msg, State) ->
	_ = lager:error("Unknown cast: ~p", [Msg]),
	{noreply, State}.
	
handle_info(Msg, State) ->
	_ = lager:error("Unknown handle_info: ~p", [Msg]),
	{noreply, State}.
	
terminate(_Reason, _State) ->
	ok.
	
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	
%% Internal

score(B) when is_binary(B) ->
	crypto:hash(sha1, B).
	
