%% @doc
%% @end
-module(counters_boolean).

%%% metric
-export([new/3,
         set/3,
         toggle/2,
         remove/1,
         remove/2,
         value/1,
         value/2]).

-export([init/0]).

%%====================================================================
%% Macros
%%====================================================================

-define(TABLE, ?MODULE).
-define(KEY, {Name, Tags}).
-define(BOOLEAN_POS, 2).

%%====================================================================
%% API
%%====================================================================

%% @doc
%%
%% @end
new(Name, Tags, Value) ->
  case ets:insert_new(?TABLE, {?KEY, v_(Value)}) of
    false -> %% deliberately explicit
      false;
    true ->
      ok
  end.

%% @doc
%%
%% @end
set(Name, Tags, Value) ->
  case ets:update_element(?TABLE, ?KEY,
                          {?BOOLEAN_POS, v_(Value)}) of
    false ->
      unknown;
    true ->
      ok
  end.

%% @doc
%%
%% @end
toggle(Name, Tags) ->
  Key = ?KEY,
  try
    ets:update_counter(?TABLE, Key,
                       {?BOOLEAN_POS, 1, 1, 0}),
    ok
  catch error:badarg ->
      case ets:lookup(?TABLE, Key) of
        [{_Key, undefined}] ->
          erlang:error({invalid_value, undefined,
                        "can't toggle undefined boolean"});
        _ ->
          unknown
      end
  end.

%% @doc
%%
%% @end
remove(Name) ->
  ets:match_delete(?TABLE, {{Name, '_'}, '_'}).

%% @doc
%%
%% @end
remove(Name, Tags) ->
  case ets:take(?TABLE, ?KEY) of
    [] -> false;
    _ -> true
  end.

%% @doc
%%
%% @end
value(Name) ->  
  lists:foldl(fun([Tags, V], Acc) ->
                  Acc#{Tags => V}
              end, #{}, load_all_values(Name)).

%% @doc
%%
%% @end
value(Name, Tags) ->
  case ets:lookup(?TABLE, ?KEY) of
    [{_Key, Value}] -> v(Value);
    [] -> unknown
  end.

init() ->
  ets:new(?TABLE, [set, named_table, public, {write_concurrency, true}]).

%%====================================================================
%% Private Parts
%%====================================================================

load_all_values(Name) ->
  ets:match(?TABLE, {{Name, '$1'}, '$2'}).

v(undefined) ->
  undefined;
v(1) ->
  true;
v(0) ->
  false.

v_(Value) ->
  case Value of
    true -> 1;
    false -> 0;
    1 -> 1;
    0 -> 0;
    [] -> 0;
    _ when is_number(Value) andalso Value > 0 -> 1;
    _ when is_list(Value) -> 1;
    undefined -> undefined;
    _ -> erlang:error({invalid_value, Value, "value is not boolean"})
  end.
