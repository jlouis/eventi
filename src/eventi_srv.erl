-module(eventi_srv).

-behaviour(gen_server).

-export([
	code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2
	]).
	
-define(TIMEOUT, 128 * 1000).

-export([
	start_link/0,
	handle_msg/1,
	read/3,
	write/2
	]).
	
-record(state, {
	db
	}).

%% API

%% start_link/0 starts up a new Venti instance
start_link() ->
	{ok, VentiDir} = application:get_env(eventi, dir),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [VentiDir], []).

%% read/3 executes a read of `{Score, Type}' towards the venti store
-spec read(Score, Type, MaxCount) -> {ok, Data} | {error, Reason}
	when
	  Score :: binary(),
	  Type :: char(),
	  MaxCount :: integer(),
	  Data :: binary(),
	  Reason :: term().
read(Score, Type, MaxCount) ->
	case handle_msg({t_read, 0, Score, Type, MaxCount}) of
	  {reply, {r_read, 0, Data}} -> {ok, Data};
	  {reply, {r_error, 0, Reason}} -> {error, Reason}
	end.

%% write/2 writes a pair of `{Type, Data}' to eVenti
-spec write(Type, Data) -> Score
	when
	  Type :: char(),
	  Data :: binary(),
	  Score :: binary().
write(Type, Data) ->
	{reply, {r_write, 0, Score}} = handle_msg({t_write, 0, Type, Data}),
	Score.

%% handle_msg/1 handles Venti client messages on the server side
%% We try hard to avoid going through the gen_server if possible
-spec handle_msg(Request) -> {reply, Reply} | {stop, Reason}
	when
	  Request :: eventi_protocol:t_msg(),
	  Reply :: eventi_protocol:r_msg(),
	  Reason :: term().
handle_msg({t_hello, Tag, <<"02">>, _Uid, _Strength, _Crypto, _Codec}) ->
	{reply, {r_hello, Tag, <<"eventi">>, 0,0}};
handle_msg({t_ping, Tag}) -> {reply, {r_ping, Tag}};
handle_msg({t_write, Tag, Type, Data}) ->
	Score = score(Data),
	ok = gen_server:call(?MODULE, {write, Score, Type, Data}, ?TIMEOUT),
	ok = lager:debug("Writing ~B bytes with Score=~p, type=~p", [byte_size(Data), Score, Type]),
	{reply, {r_write, Tag, Score}};
handle_msg({t_read, Tag, Score, Type, Count}) ->
	case gen_server:call(?MODULE, {read, Score, Type}, ?TIMEOUT) of
		{ok, Data} when byte_size(Data) =< Count ->
		  {reply, {r_read, Tag, Data}};
		{ok, _Data} ->
		  {reply, {r_error, Tag, count_exceeded}};
		not_found ->
		  {reply, {r_error, Tag, not_found}}
	end;
handle_msg({t_sync, Tag}) ->
	%% Current setup makes every call sync for simplicity
	ok = lager:debug("Tsync"),
	{reply, {r_sync, Tag}};
handle_msg({t_goodbye, _Tag}) -> {stop, goodbye}.

%% Callbacks

init([VentiDir]) ->
	process_flag(trap_exit, true),
	ok = lager:debug("Opening venti data store at ~p", [VentiDir]),
	{ok, DBRef} = eleveldb:open(VentiDir, [{create_if_missing, true}]),
	{ok, #state{ db = DBRef }}.
	
handle_call({status, Score, Type}, _From, #state { db = Db } = State) ->
	Key = <<Score/binary, Type>>,
	Res = eleveldb:status(Db, Key),
	{reply, Res, State};
handle_call({write, Score, Type, Data}, _From, #state { db = Db } = State) ->
	Key = <<Score/binary, Type>>,
	lager:debug("Writing: ~p", [Key]),
	ok = eleveldb:write(Db, [{put, Key, Data}], []),
	{reply, ok, State};
handle_call({read, Score, Type}, _From, #state { db = Db } = State) ->
	Key = <<Score/binary, Type>>,
	lager:debug("Reading: ~p", [Key]),
	R = eleveldb:get(Db, Key, []),
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
	
terminate(_Reason, #state { db = DBRef }) ->
	eleveldb:close(DBRef),
	ok.
	
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
	
%% Internal

score(B) when is_binary(B) ->
	crypto:hash(sha, B).
	
