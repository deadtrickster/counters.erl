%%%-------------------------------------------------------------------
%% @doc counters top level supervisor.
%% @hidden
%%%-------------------------------------------------------------------

-module(counters_sup).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-behaviour(supervisor).

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  counters_counter:init(),
  counters_boolean:init(),
  counters_simple:init(),
  counters_sum:init(),
  counters_distribution:init(),
  {ok, {{one_for_one, 5, 1}, []}}.

%%====================================================================
%% Private Parts
%%====================================================================
