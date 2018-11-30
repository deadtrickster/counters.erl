%%%-------------------------------------------------------------------
%% @doc counters public API
%% @hidden
%%%-------------------------------------------------------------------

-module(counters_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([start/0, stop/0]).
-define(APP, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) -> counters_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) -> ok.

start() -> application:start(?APP).

stop() -> application:stop(?APP).
