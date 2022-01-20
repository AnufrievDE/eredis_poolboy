%%%----------------------------------------------------------------------------
%%% @doc
%%% This file is part of eredis + poolboy.
%%% @end
%%%----------------------------------------------------------------------------

-module(eredis_poolboy).

%% API
-export([
    add_pool/2, add_pool/3, add_pool/4,
    q/1, q/2, q/3,
    qp/1, qp/2, qp/3,
    q_noreply/1, q_noreply/2
]).

-include_lib("eredis/include/eredis.hrl").

-define(DEFAULT_POOL, eredis_pool).

%%%----------------------------------------------------------------------------
%%% @doc
%%% Add new pool connections.
%%% @end
%%%----------------------------------------------------------------------------
-spec add_pool(PoolName, EredisAndPoolArgs) -> supervisor:startchild_ret() when
    PoolName :: atom(),
    EredisAndPoolArgs :: proplists:proplist().
add_pool(PoolName, EredisAndPoolArgs) ->
    eredis_poolboy_sup:add_pool(PoolName, EredisAndPoolArgs).

-spec add_pool(PoolName, PoolArgs, EredisArgs) ->
    supervisor:startchild_ret() when
    PoolName :: atom(),
    PoolArgs :: proplists:proplist(),
    EredisArgs   :: options().
add_pool(PoolName, PoolArgs, EredisArgs) ->
    eredis_poolboy_sup:add_pool(PoolName, PoolArgs, EredisArgs).

-spec add_pool(PoolName, PoolArgs, GlobalOrLocal, EredisArgs) ->
    supervisor:startchild_ret() when
    PoolName :: atom(),
    PoolArgs :: proplists:proplist(),
    GlobalOrLocal :: global | local,
    EredisArgs   :: options().
add_pool(PoolName, PoolArgs, GlobalOrLocal, EredisArgs) ->
    eredis_poolboy_sup:add_pool(PoolName, PoolArgs, GlobalOrLocal, EredisArgs).

%%%----------------------------------------------------------------------------
%%% @doc
%%% Executes the given command. The command must be a valid Redis command.
%%% @end
%%%----------------------------------------------------------------------------
-spec q(Command :: [any()]) ->
    {ok, return_value()} | {error, Reason :: binary() | no_connection}.
q(Command) -> q(?DEFAULT_POOL, Command).

-spec q(PoolName :: atom(), Command :: [any()]) ->
    {ok, return_value()} | {error, Reason :: binary() | no_connection}.
q(PoolName, Command) ->
    Fun = fun(Worker) -> eredis:q(Worker, Command) end,
    poolboy:transaction(PoolName, Fun).

-spec q(PoolName, Command, Timeout) ->
    {ok, return_value()} | {error, Reason :: binary() | no_connection} when
    PoolName :: atom(),
    Command  :: [any()],
    Timeout  :: non_neg_integer() | infinity.
q(PoolName, Command, Timeout) ->
    Fun = fun(Worker) -> eredis:q(Worker, Command, Timeout) end,
    poolboy:transaction(PoolName, Fun).

%%%----------------------------------------------------------------------------
%%% @doc
%%% Executes the given pipeline (list of commands) in the
%%% specified connection. The commands must be valid Redis commands.
%%% @end
%%%----------------------------------------------------------------------------
-spec qp(pipeline()) ->
    [{ok, return_value()} | {error, Reason::binary()}] | {error, no_connection}.
qp(Pipeline) -> qp(?DEFAULT_POOL, Pipeline).

-spec qp(PoolName :: atom(), pipeline()) ->
    [{ok, return_value()} | {error, Reason::binary()}] | {error, no_connection}.
qp(PoolName, Pipeline) ->
    Fun = fun(Worker) -> eredis:qp(Worker, Pipeline) end,
    poolboy:transaction(PoolName, Fun).

-spec qp(PoolName :: atom(), pipeline(), Timeout) ->
    [{ok, return_value()} | {error, Reason::binary()}] | {error, no_connection}
    when Timeout :: non_neg_integer() | infinity.
qp(PoolName, Pipeline, Timeout) ->
    Fun = fun(Worker) -> eredis:qp(Worker, Pipeline, Timeout) end,
    poolboy:transaction(PoolName, Fun).

%%%----------------------------------------------------------------------------
%%% @doc
%%% Executes the command but does not wait for a response and ignores any errors.
%%% @end
%%%----------------------------------------------------------------------------
-spec q_noreply(Command :: [any()]) -> ok.
q_noreply(Command) -> q_noreply(?DEFAULT_POOL, Command).

-spec q_noreply(PoolName :: atom(), Command :: [any()]) -> ok.
q_noreply(PoolName, Command) ->
    Fun = fun(Worker) -> eredis:q_noreply(Worker, Command) end,
    poolboy:transaction(PoolName, Fun).
