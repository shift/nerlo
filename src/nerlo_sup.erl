-module(nerlo_sup).

-behaviour(supervisor).

-export([start_link/1, init/1]).


start_link(Args) when is_list(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, [Args]).

init(_Args) ->
    Spec =
    {ok,{{one_for_one,10,10}, [
         get_spec(ej_log, []),
         get_spec(ej_srv, []),
         get_spec(neo4j_srv, [])
    ]}},
    Spec.

get_spec(Sup, Args) ->
    {Sup ,{Sup, start_link, Args}, permanent, 5000, worker, []}.
