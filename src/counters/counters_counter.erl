%% @doc
%% @end
-module(counters_counter).

%%% metric
-export([new/3,
         set/3,
         inc/3,
         dec/3,
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
  Update =
    case Value of
      _ when is_number(Value) ->
        [{?I_POS, 0}, {?F_POS, Value}];
      undefined ->
        [{?I_POS, undefined}, {?F_POS, undefined}];
      _ -> erlang:error({invalid_value, Value, "set accepts only numbers and 'undefined'"})
    end,

  case ets:update_element(?TABLE, ?KEY, Update) of
    false ->
      unknown;
    true ->
      ok
  end.

%% @doc
%%
%% @end
inc(Name, Tags, Value) when is_integer(Value) ->
  Key = ?KEY,
  try
    ets:update_counter(?TABLE, Key, {?I_POS, Value}),
    ok
  catch error:badarg ->
      maybe_inc_unknown(Key)
  end;
inc(Name, Tags, Value) when is_number(Value) ->
  Key = ?KEY,
  try
    case ets:select_replace(?TABLE,
                            [{{Key, '$1', '$2'},
                              [],
                              [{{{Key}, '$1', {'+', '$2', Value}}}]}]) of
      0 ->
        unknown;
      1 ->
        ok
    end
  catch error:badarg ->
      maybe_inc_unknown(Key)
  end;
inc(_Name, _Tags, Value) ->
  erlang:error({invalid_value, Value,
                "inc accepts only numbers"}).

%% @doc
%%
%% @end
dec(Name, Tags, Value) ->
  inc(Name, Tags, -Value).

%% @doc
%%
%% @end
remove(Name) ->
  ets:match_delete(?TABLE, {{Name, '_'}, '_', '_'}).

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
                  Acc#{Tags => I + F}
              end, #{}, load_all_values(Name)).

%% @doc
%%
%% @end
value(Name, Tags) ->
  case ets:lookup(?TABLE, ?KEY) of
    [{_Key, IValue, FValue}] -> sum(IValue, FValue);
    [] -> unknown
  end.

init() ->
  ets:new(?TABLE, [set, named_table, public, {write_concurrency, true}]).

%%====================================================================
%% Private Parts
%%====================================================================

maybe_inc_unknown(Key) ->
  case ets:lookup(?TABLE, Key) of
    [{Key, undefined, undefined}] ->
      erlang:error({invalid_operation, 'inc/dec', "Can't inc/dec undefined"});
    _ ->
      unknown
  end.

sum(_IValue, undefined) ->
  undefined;
sum(IValue, FValue) ->
  IValue + FValue.

load_all_values(Name) ->
  ets:match(?TABLE, {{Name, '$1'}, '$2', '$3'}).
