%% @doc
%% @end
-module(counters_simple).

%%% metric
-export([new/3,
         set/3,
         inc/3,
         remove/1,
         remove/2,
         value/1,
         value/2]).

-export([init/0]).

%%====================================================================
%% Macros
%%====================================================================

-define(TABLE, ?MODULE).
-define(WIDTH, 16).
-define(KEY, {Name, Tags, erlang:system_info(scheduler_id) band (?WIDTH-1)}).
-define(I_POS, 2).
-define(F_POS, 3).

%%====================================================================
%% API
%%====================================================================

%% @doc
%%
%% @end
new(Name, Tags, Value) ->
  case ets:insert_new(?TABLE, {?KEY, 0, Value}) of
    false -> %% deliberately explicit
      false;
    true ->
      ok
  end.


%% @doc
%%
%% @end
set(Name, Tags, Value) ->
  case ets:update_element(?TABLE, ?KEY, [{?I_POS, 0}, {?F_POS, Value}]) of
    false ->
      unknown;
    true ->
      ok
  end.

%% @doc
%%
%% @end
inc(Name, Tags, Value) when is_integer(Value) ->
  try
    ets:update_counter(?TABLE, ?KEY, {?I_POS, Value}),
    ok
  catch error:badarg ->
      unknown
  end;
inc(Name, Tags, Value) when is_number(Value) ->
  Key = ?KEY,
  case ets:select_replace(?TABLE,
                          [{{Key, '$1', '$2'},
                            [],
                            [{{{Key}, '$1', {'+', '$2', Value}}}]}]) of
    0 ->
      unknown;
    1 ->
      ok
  end;
inc(_Name, _Tags, Value) ->
  erlang:error({invalid_value, Value,
                "inc accepts only numbers"}).

%% @doc
%%
%% @end
remove(Name) ->
  ets:match_delete(?TABLE, {{Name, '_', '_'}, '_', '_'}).

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
  lists:foldl(fun([Tags, I, F], Acc) ->
                  Sum = maps:get(Tags, Acc, 0),
                  Acc#{Tags => Sum + I + F}
              end, #{}, load_all_values(Name)).

%% @doc
%%
%% @end
value(Name, Tags) ->
  case ets:select(?TABLE, [{{{Name, Tags, '_'}, '$1', '$2'},
                            [],
                            [{'+', '$1', '$2'}]}]) of
    [] -> undefined;
    List -> lists:sum(List)
  end.

init() ->
  ets:new(?TABLE, [set, named_table, public, {write_concurrency, true}]).

%%====================================================================
%% Private Parts
%%====================================================================

load_all_values(Name) ->
  ets:match(?TABLE, {{Name, '$1', '_'}, '$2', '$3'}).
