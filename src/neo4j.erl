
%% @doc This is the Erlang API of the graph database.
%%
%% @author Ingo Schramm

-module(neo4j).

% public interface
-export([start/0
        ,stop/0
        ,has_db/0
        ,add_vertex/0
        ,del_vertex/1
        ,add_edge/2
        ,add_edge/3
        ,del_edge/1
        ,vertex_get_edges/1
        ,vertex_get_edges/2
        ,vertex_get_neighbourhood/1
        ,vertex_get_neighbourhood/2
        ,vertex_set_property/3
        ,vertex_del_property/2
        ,vertex_get_property/2
        ,vertex_get_properties/1
        ,edge_get_adjacent_vertices/1
        ,edge_get_start_vertex/1
        ,edge_get_end_vertex/1
        ,edge_get_type/1
        ,edge_set_property/3
        ,edge_del_property/2
        ,edge_get_property/2
        ,edge_get_properties/1
        ,create_index/2
        ,index_add_vertex_prop/4
        ,index_del_vertex_prop/4
        ,index_get_vertex/3
        ,index_query_vertex/2
        ,order/0
        ,size/0
        ,types/0
        ,scorch/0
        ]).

-author("Ingo Schramm").

-include("ej.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(HANDLER, {handler,graphdb}).
-define(VERTEX(Id), {vertex,Id}).
-define(EDGE(Id,Type,A,B), {edge,Id,Type,A,B}).

% @doc Start the database.
start() ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,init}]) of
        {ok,[{result,true}]} -> ok;
        _Any                 -> error
    end.

% @doc Stop the database.
stop() ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,stop}]) of
        {ok,[{result,ok}]} -> ok;
        _Any               -> error
    end.

% @doc Test whether a graph database is running.
has_db() ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,has_db}]) of
        {ok, Data} ->
            case lists:keyfind(result,1,Data) of
                false          -> false;
                {result,Value} -> Value
            end;
        _Error -> false
    end.

% @doc Add a vertex.
add_vertex() ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,add_vertex}]) of
        {ok, Data} ->
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> {ok, ?VERTEX(Value)}
            end;
        Error -> Error
    end.

% @doc Delete a vertex.
del_vertex(?VERTEX(Id)) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,del_vertex},{id,Id}]) of
        {ok, Data} ->
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> Value
            end;
        Error -> Error
    end.

% @doc Get the list of edges of a given vertex.
vertex_get_edges(?VERTEX(Id)) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,vertex_get_edges},{id,Id}]) of
        {ok, Data} ->
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_content};
                {result,Value} -> Value
            end;
        Error -> Error
    end.

% @doc Get the list of edges of a given type for a vertex.
vertex_get_edges(V=?VERTEX(_Id), Type) ->
    [ E || E=?EDGE(_EId,_A,_B,T) <- vertex_get_edges(V), T =:= Type ].

% @doc Get all vertices connected ith a given vertex.
vertex_get_neighbourhood(V=?VERTEX(Id)) ->
    get_other_nodes(Id,vertex_get_edges(V)).

% @doc Get all vertices connected with a given vertex
% by an edge of a given type.
vertex_get_neighbourhood(V=?VERTEX(Id), Type) ->
    get_other_nodes(Id,vertex_get_edges(V, Type)).

% @doc Set a property at a vertex.
vertex_set_property(V, Key, Val) when not is_list(Key) ->
    vertex_set_property(V, nerlo_util:to_list(Key), Val);
vertex_set_property(V, Key, Val) when not is_list(Val) ->
    vertex_set_property(V, Key, nerlo_util:to_list(Key));
vertex_set_property(?VERTEX(Id), Key, Val) ->
    private_set_property(vertex, Id, Key, Val).

% @doc Delete a property at a vertex.
vertex_del_property(?VERTEX(Id), Key) ->
    private_del_property(vertex, Id, Key).

% @doc Get a property at a vertex.
vertex_get_property(?VERTEX(Id), Key) ->
    private_get_property(vertex, Id, Key).

% @doc Get all properties at a vertex.
vertex_get_properties(?VERTEX(Id)) ->
    private_get_properties(vertex, Id).

% @doc Add an undirected edge.
% This will actually add two edges, one in each direction.
add_edge(Va=?VERTEX(_A), Vb=?VERTEX(_B), Type) ->
    private_add_edge(Va, Vb, Type).

% @doc Add an undirected edge with type EDGE.
% This will actually add two edges, one in each direction.
add_edge(Va=?VERTEX(_A), Vb=?VERTEX(_B)) ->
    private_add_edge(Va, Vb, 'EDGE').

% @doc Delete an edge.
del_edge(?EDGE(Id,_Type,_A,_B)) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,del_edge},{id,Id}]) of
        {ok, Data} ->
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> Value
            end;
        Error -> Error
    end.

% @doc Get the adjacent vertices connected by an edge.
edge_get_adjacent_vertices(?EDGE(_Id,_Type,A,B)) ->
    {?VERTEX(A),?VERTEX(B)}.

% @doc Get the start vertex of an edge.
edge_get_start_vertex(Edge) ->
    {Start,_End} = edge_get_adjacent_vertices(Edge),
    Start.

% @doch Get the end ertex of an edge.
edge_get_end_vertex(Edge) ->
    {_Start,End} = edge_get_adjacent_vertices(Edge),
    End.

% @doc Get the type of an edge.
edge_get_type(?EDGE(_Id,Type,_A,_B)) ->
    Type.

% @doc Set a property at an edge.
edge_set_property(?EDGE(Id,_Type,_A,_B), Key, Val) ->
    private_set_property(edge, Id, Key, Val).

% @doc Delete a property at an edge.
edge_del_property(?EDGE(Id,_Type,_A,_B), Key) ->
    private_del_property(edge, Id, Key).

% @doc Get a property at an edge.
edge_get_property(?EDGE(Id,_Type,_A,_B), Key) ->
    private_get_property(edge, Id, Key).

% @doc Get all properties at an edge.
edge_get_properties(?EDGE(Id,_Type,_A,_B)) ->
    private_get_properties(edge, Id).

%% It kind of sucks to have to manage indices in the app.  le sigh.

% @doc Create an index
create_index(Type, Name) ->
    private_index(create, Type, Name, -1, nil, nil).

%% TODO: delete_index

% @doc Add a vertex k/v pair to the index.
index_add_vertex_prop(?VERTEX(Id), Name, Key, Val) ->
    private_index(add, node, Name, Id,
                  nerlo_util:to_list(Key),
                  nerlo_util:to_list(Val)).

% @doc Remove a vertex k/v pair from the index.
index_del_vertex_prop(?VERTEX(Id), Name, Key, Val) ->
    private_index(del, node, Name, Id, Key, Val).

% @doc Lookup a vertex in the index.
index_get_vertex(Name, Key, Val) ->
    case private_index(lookup, node, Name, -1,
                       nerlo_util:to_list(Key),
                       nerlo_util:to_list(Val)) of
        Error = {error, _} -> Error;
        Id when is_integer(Id) -> {ok, ?VERTEX(Id)};
        [First | _] = All ->
            ej_log:warn("multiple vertices found in index: ~p. "
                        "Returning first", [All]),
            {ok, ?VERTEX(First)};
        [] -> {error, not_found};
        ok -> {error, not_found};
        Result -> {error, bad_index_get_vertex, Result}
    end.

%% TODO: index_add_edge and family
%%       Type = 'edge'

index_query_vertex(Name, Query) ->
    case private_index('query', node, Name, -1, '', Query) of
        Error = {error, _} -> Error;
        Results when is_list(Results) -> Results;
        Result -> {error, bad_index_query_vertex, Result}
    end.

% @doc Determine the order of the graph, the number of vertices.
order() ->
    private_info(order).

% @doc Determine the size of the graph, the number of edges.
size() ->
    private_info(size).

% @doc Get a list with all used relationship types.
types() ->
    private_info(types).

% @doc wipe the database clean, off the file system, and start from scratch
scorch() ->
    case ej_srv:call(?TAG_CALL, [{call, scorch}]) of
        {ok,[{result,"scorched"}]} ->
            start(); % do a {call, init} again for clean start
        _Any ->
            error
    end.


%% ----- PRIVATE ------

private_add_edge(?VERTEX(A), ?VERTEX(B), Type) ->
    case ej_srv:call(?TAG_CALL,
                     [?HANDLER,{call,add_edge},{a,A},{b,B},{type,Type}]) of
        {ok, Data} ->
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> {ok, ?EDGE(Value,Type,A,B)}
            end;
        Error -> Error
    end.

private_set_property(Type, Id, Key, Val) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER
                                ,{call,set_property}
                                ,{type,Type}
                                ,{id,Id}
                                ,{key,Key}
                                ,{value,Val}]) of
        {ok, _} -> ok;
        Error   -> Error
    end.

private_del_property(Type, Id, Key) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER
                                ,{call,del_property}
                                ,{type,Type}
                                ,{id,Id}
                                ,{key,Key}]) of
        {ok, _} -> ok;
        Error   -> Error
    end.

private_get_property(Type, Id, Key) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER
                                ,{call,get_property}
                                ,{type,Type}
                                ,{id,Id}
                                ,{key,Key}]) of
        {ok, Data} ->
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_result};
                {result,Value} -> {ok, Value}
            end;
        Error   -> Error
    end.

private_get_properties(Type, Id) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER
                                ,{call,get_properties}
                                ,{type,Type}
                                ,{id,Id}]) of
        {ok, Data} ->
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_result};
                {result,Value} -> {ok, Value}
            end;
        Error   -> Error
    end.

private_info(Item) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER,{call,info},{item,Item}]) of
        {ok, Data} ->
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_id};
                {result,Value} -> {ok, Value}
            end;
        Error -> Error
    end.

private_index(Op, Type, Name, Id, Key, Val) ->
    case ej_srv:call(?TAG_CALL, [?HANDLER
                                ,{call,index}
                                ,{op,Op}
                                ,{type,Type}
                                ,{name,Name}
                                ,{id,Id}
                                ,{key,Key}
                                ,{value,Val}]) of
        {ok, Data} ->
            case lists:keyfind(result,1,Data) of
                false          -> {error, answer_has_no_result};
                {result,"ok"} -> ok;
                {result,Value} -> Value
            end;
        Error -> Error
    end.

get_other_nodes(Id,Edges) ->
    lists:map(
        fun(?EDGE(_EId,A,B,_T))->
            if
                A =:= Id -> ?VERTEX(B);
                true     -> ?VERTEX(A)
            end
        end,
        Edges).


%% ------ TESTS ------

start_test() ->
    ej_srv:start(1,"../bin"),
    R = ?MODULE:start(),
    timer:sleep(300),
    ?assertEqual(R, ok).

stop_test() ->
    R = ?MODULE:stop(),
    ?assertEqual(R, ok),
    ej_srv:stop().
