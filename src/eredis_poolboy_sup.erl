
%%%----------------------------------------------------------------------------
%%% @doc
%%% This file is part of eredis + poolboy.
%%% eredis_poolboy top level supervisor.
%%% @end
%%%----------------------------------------------------------------------------

-module(eredis_poolboy_sup).

-behaviour(supervisor).

%% API
-export([
    start_link/0,
    add_pool/2, add_pool/3, add_pool/4
]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("eredis/include/eredis.hrl").

%%%----------------------------------------------------------------------------
%%% API functions
%%%----------------------------------------------------------------------------
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, application:get_all_env()).

%%%----------------------------------------------------------------------------
%%% @doc
%%% Add new pool connections.
%%% @end
%%%----------------------------------------------------------------------------
-spec add_pool(PoolName, EredisAndPoolArgs) -> supervisor:startchild_ret() when
    PoolName :: atom(),
    EredisAndPoolArgs :: proplists:proplist().
add_pool(PoolName, EredisAndPoolArgs) ->
    add_pool(PoolName, EredisAndPoolArgs, EredisAndPoolArgs).

-spec add_pool(PoolName, PoolArgs, EredisArgs) ->
    supervisor:startchild_ret() when
    PoolName :: atom(),
    PoolArgs :: proplists:proplist(),
    EredisArgs   :: options().
add_pool(PoolName, PoolArgs, EredisArgs) ->
    add_pool(PoolName, PoolArgs, local, EredisArgs).

-spec add_pool(PoolName, PoolArgs, GlobalOrLocal, EredisArgs) ->
    supervisor:startchild_ret() when
    PoolName :: atom(),
    PoolArgs :: proplists:proplist(),
    GlobalOrLocal :: global | local,
    EredisArgs   :: options().
add_pool(PoolName, PoolArgs, GlobalOrLocal, EredisArgs) ->
    UPoolArgs = pool_args(GlobalOrLocal, PoolName, PoolArgs),
    PoolSpec = poolboy:child_spec(PoolName, UPoolArgs, EredisArgs),
    supervisor:start_child(?MODULE, PoolSpec).

%%%----------------------------------------------------------------------------
%%% Supervisor callbacks
%%%----------------------------------------------------------------------------
init(Env) ->
    GlobalOrLocal = proplists:get_value(global_or_local, Env),
    Pools = proplists:get_value(pools, Env),
    PoolSpecs =
        lists:map(fun({PoolName, EredisAndPoolArgs}) ->
            PoolArgs = pool_args(GlobalOrLocal, PoolName, EredisAndPoolArgs),
            #{
                id => PoolName,
                start => {poolboy, start_link, [PoolArgs, EredisAndPoolArgs]}
            }
        end, Pools),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 2,
        period => 5
    },
    {ok, {SupFlags, PoolSpecs}}.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
pool_args(GlobalOrLocal, PoolName, ExtraArgs) ->
    lists:ukeysort(1, [
        {name, {GlobalOrLocal, PoolName}},
        {worker_module, eredis} | ExtraArgs]).