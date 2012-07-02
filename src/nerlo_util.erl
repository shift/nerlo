-module(nerlo_util).

-export([get_value/2, get_value/3, ensure_started/1]).
-export([to_binary/1, to_int/1, to_list/1, to_float/1, to_atom/1]).

-define(t2b(T), term_to_binary(T)).
-define(b2t(B), binary_to_term(B)).
-define(l2b(L), list_to_binary(L)).
-define(b2l(B), binary_to_list(B)).
-define(l2a(L), list_to_atom(L)).
-define(a2l(A), atom_to_list(A)).
-define(l2i(L), list_to_integer(L)).
-define(i2l(I), integer_to_list(I)).

%% @doc get_value/2 and /3 replaces proplist:get_value/2 and /3
%%      this list function is a bit more efficient
%% @end
get_value(Key, List) ->
  get_value(Key, List, undefined).

get_value(Key, List, Default) ->
  case lists:keysearch(Key, 1, List) of
    {value, {Key,Value}} ->
      Value;
    false ->
      Default
  end.

%% @doc make sure an application is started
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

%% conversion functions
to_binary(undefined)            -> undefined;
to_binary(V) when is_integer(V) -> to_binary(?i2l(V));
to_binary(V) when is_list(V)    -> to_binary(?l2b(V));
to_binary(V) when is_float(V)   -> to_binary(float_to_list(V));
to_binary(V) when is_atom(V)    -> to_binary(?a2l(V));
to_binary(V) when is_binary(V)  -> V.

to_int(undefined)            -> undefined;
to_int(V) when is_float(V)   -> round(V);
to_int(V) when is_integer(V) -> V;
to_int(V) when is_list(V)    -> ?l2i(V);
to_int(V) when is_binary(V)  -> to_int(?b2l(V)).

to_list(undefined)            -> undefined;
to_list(V) when is_integer(V) -> integer_to_list(V);
to_list(V) when is_list(V)    -> V;
to_list(V) when is_binary(V)  -> ?b2l(V);
to_list(V) when is_atom(V)    -> ?a2l(V).

to_float(undefined)            -> undefined;
to_float(V) when is_integer(V) -> V + 0.0;
to_float(V) when is_list(V)    -> list_to_float(V);
to_float(V) when is_binary(V)  -> to_float(?b2l(V)).

to_atom(undefined)         -> undefined;
to_atom(V) when is_atom(V) -> V;
to_atom(V) when is_list(V) -> list_to_atom(V);
to_atom(V)                 -> to_atom(to_list(V)).
