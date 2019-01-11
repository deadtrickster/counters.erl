%% @doc
%% @end
-module(counters_sum).

%%% metric
-export([new/4,
         set/4,
         observe/3,
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
-define(C_POS, 2).
-define(I_POS, 3).
-define(F_POS, 4).

%%====================================================================
%% API
%%====================================================================

%% @doc
%%
%% @end
new(Name, Tags, Count, Sum) ->
  case ets:insert_new(?TABLE, {?KEY, Count, 0, Sum}) of
    false -> %% deliberately explicit
      false;
    true ->
      ok
  end.


%% @doc
%%
%% @end
set(Name, Tags, Count, Sum) ->
  case ets:update_element(?TABLE, ?KEY, [{?C_POS, Count}, {?I_POS, 0}, {?F_POS, Sum}]) of
    false ->
      unknown;
    true ->
      ok
  end.

%% @doc
%%
%% @end
observe(Name, Tags, Value) when is_integer(Value) ->
  try
    ets:update_counter(?TABLE, ?KEY, [{?C_POS, 1}, {?I_POS, Value}]),
    ok
  catch error:badarg ->
      unknown
  end;
observe(Name, Tags, Value) when is_number(Value) ->
  Key = ?KEY,
  case ets:select_replace(?TABLE,
                          [{{Key, '$1', '$2', '$3'},
                            [],
                            [{{{Key}, {'+', '$1', 1}, '$2', {'+', '$3', Value}}}]}]) of
    0 ->
      unknown;
    1 ->
      ok
  end;
observe(_Name, _Tags, Value) ->
  erlang:error({invalid_value, Value,
                "observe accepts only numbers"}).

%% @doc
%%
%% @end
remove(Name) ->
  ets:match_delete(?TABLE, {{Name, '_', '_'}, '_', '_', '_'}).

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
  lists:foldl(fun([Tags, C, I, F], Acc) ->
                  {Ca, Sa} = maps:get(Tags, Acc, {0, 0}),
                  Acc#{Tags => {Ca + C, Sa + I + F}}
              end, #{}, load_all_values(Name)).

%% @doc
%%
%% @end
value(Name, Tags) ->
  case ets:match(?TABLE, {{Name, Tags, '_'}, '$1', '$2', '$3'}) of
    [] -> undefined;
    List -> lists:foldl(fun([C, I, F], {Ca, Sa}) ->
                            {Ca + C, Sa + I + F}
                        end, {0, 0}, List)
  end.

init() ->
  ets:new(?TABLE, [set, named_table, public, {write_concurrency, true}]).

%%====================================================================
%% Private Parts
%%====================================================================

load_all_values(Name) ->
  ets:match(?TABLE, {{Name, '$1', '_'}, '$2', '$3', '$4'}).
