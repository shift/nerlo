%% @doc Application module for Erlang/Java bridge..
%%
%% @author Ingo Schramm

-module(nerlo_app).

-behaviour(application).

-export([start/0,start/2,stop/1,stop/0]).

-author("Ingo Schramm").

-define(APPNAME, nerlo).

start() ->
    application:start(?APPNAME).

stop() ->
    ej_svr:stop(), %% will this get rid of jnode?
    application:stop(?APPNAME).

start(Type, _Args) ->
    case Type of
        normal   -> nerlo_sup:start_link([]);
        takeover -> ok;
        failover -> ok
    end.

stop(_State) ->
    ok.
