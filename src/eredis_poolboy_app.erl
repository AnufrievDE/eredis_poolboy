%%%----------------------------------------------------------------------------
%%% @doc
%%% This file is part of eredis + poolboy.
%%% eredis_poolboy public API
%%% @end
%%%----------------------------------------------------------------------------

-module(eredis_poolboy_app).

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
start(_StartType, _StartArgs) -> eredis_poolboy_sup:start_link().

stop(_State) -> ok.
