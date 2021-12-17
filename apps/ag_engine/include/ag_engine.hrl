%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-ifndef(_GEN_GAME_LIB_HRL).
-define(_GEN_GAME_LIB_HRL, 1).

-type ag_game_id() :: string()|binary()|integer().

-record(temporary_security, {id :: term(), security :: string() | binary()}).

-record(online_player, {
    id :: ag_game_id(),
    idtype :: term(),
    agent_pid :: pid() | disabled | undefined,
    agent_node :: node() | disabled,
    gate_pid :: pid() | undefined,
    gate_node :: node(),
    archive :: ag_game_id(),
    session :: ag_game_id(),
    security :: ag_game_id(),
    login_time :: integer() | undefined
}).

-record(gate_node, {
    node :: node(),
    gate_count :: integer()
}).

-record(agent_node, {
    node :: node(),
    agent_count :: integer()
}).

-define(AUTH_SYSTEM_TYPE, seconds).

-define(ENGINE_CACHE_POOL, engine_cache_pool).

-endif. %%_GEN_GAME_LIB_HRL
