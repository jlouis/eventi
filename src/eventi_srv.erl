-module(eventi_srv).

-behaviour(gen_server).

-export([
	code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2
	]).
	
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
handle_msg({t_goodbye, _Tag}) -> {stop, goodbye}.

%% Callbacks

init([VentiDir]) ->
	{ok, DBRef} = eleveldb:open(VentiDir, [{create_if_missing, true}]),
	{ok, #state{ db = DBRef }}.
	
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
	
