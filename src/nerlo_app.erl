%% @doc Application module for Erlang/Java bridge..
%%
%% @author Ingo Schramm

-module(nerlo_app).

-behaviour(application).

-export([start/0,start/2,stop/1,stop/0]).

-author("Ingo Schramm").

-include("ej.hrl").

start() ->
    application:start(?APP).

stop() ->
    %% TODO: shut down jnode if it's still running
    application:stop(?APP).

start(Type, _Args) ->
    case Type of
        normal   -> nerlo_sup:start_link();
        takeover -> ok;
        failover -> ok
    end.

stop(_State) ->
    ok.
