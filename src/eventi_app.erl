-module(eventi_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	%% Ranch listener
	{ok, _} = ranch:start_listener(eventi_tcp, 10, ranch_tcp, [{port, 17034}], eventi_protocol, []),
	eventi_sup:start_link().

stop(_State) ->
    ok.
