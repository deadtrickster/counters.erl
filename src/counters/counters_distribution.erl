%% @doc
%% @end
-module(counters_distribution).

%%% metric
-export([new/5,
         set/5,
         observe/4,
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
-define(KEY(Bucket), {Name, Tags, Bucket,
                      erlang:system_info(scheduler_id) band (?WIDTH-1)}).
-define(C_POS, 2).
-define(I_POS, 3).
-define(F_POS, 4).

%%====================================================================
%% API
%%====================================================================

%% @doc
%%
%% @end
new(Name, Tags, Bucket, Count, Sum) ->
  case ets:insert_new(?TABLE, {?KEY(Bucket), Count, 0, Sum}) of
    false -> %% deliberately explicit
      false;
    true ->
      ok
  end.


%% @doc
%%
%% @end
set(Name, Tags, Bucket, Count, Sum) ->
  case ets:update_element(?TABLE, ?KEY(Bucket), Count, [{?I_POS, 0}, {?F_POS, Sum}]) of
    false ->
      unknown;
    true ->
      ok
  end.

%% @doc
%%
%% @end
observe(Name, Tags, Bucket, Value) when is_integer(Value) ->
  try
    ets:update_counter(?TABLE, ?KEY(Bucket), [{?C_POS, 1}, {?I_POS, Value}]),
    ok
  catch error:badarg ->
      unknown
  end;
observe(Name, Tags, Bucket, Value) when is_number(Value) ->
  Key = ?KEY(Bucket),
  case ets:select_replace(?TABLE,
                          [{{Key, '$1', '$2', '$3'},
                            [],
                            [{{{Key}, {'+', '$1', 1}, '$2', {'+', '$3', Value}}}]}]) of
    0 ->
      unknown;
    1 ->
      ok
  end;
observe(_Name, _Tags, _Bucket, Value) ->
  erlang:error({invalid_value, Value,
                "observe accepts only numbers"}).

%% @doc
%%
%% @end
remove(Name) ->
  ets:match_delete(?TABLE, {{Name, '_', '_', '_'}, '_', '_', '_'}).

%% @doc
%%
%% @end
remove(Name, Tags) ->
  ets:match_delete(?TABLE, {{Name, Tags, '_', '_'}, '_', '_', '_'}).

%% @doc
%%
%% @end
value(Name) ->
  lists:foldl(fun([Tags, B, C, I, F], Acc) ->
                  BMap = maps:get(Tags, Acc, #{}),
                  Acc#{Tags => add_to_bucket_map(BMap, B, C, I, F)}
              end, #{}, load_all_values(Name)).

%% @doc
%%
%% @end
value(Name, Tags) ->
  case ets:match(?TABLE, {{Name, Tags, '$1', '_'}, '$2', '$3', '$4'}) of
    [] -> undefined;
    List -> lists:foldl(fun([B, C, I, F], BMap) ->
                            add_to_bucket_map(BMap, B, C, I, F)
                        end, #{}, List)
  end.

init() ->
  ets:new(?TABLE, [set, named_table, public, {write_concurrency, true}]).

%%====================================================================
%% Private Parts
%%====================================================================

load_all_values(Name) ->
  ets:match(?TABLE, {{Name, '$1', '$2', '_'}, '$3', '$4', '$5'}).

add_to_bucket_map(BMap, B, C, I, F) ->
  {Ca, Sa} = maps:get(B, BMap, {0, 0}),
  BMap#{B => {Ca + C, Sa + I + F}}.
