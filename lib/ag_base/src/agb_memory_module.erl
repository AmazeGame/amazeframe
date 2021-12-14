%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------

-module(agb_memory_module).

-callback get_mods() ->
    [atom()].

-export([init_behaviour/3]).

-spec init_behaviour(MemMod :: atom(), DestBehaviour :: atom(), AppendString :: string()) ->
    ok|{error, badarg | code:load_error_rsn()}.
init_behaviour(MemMod, DestBehaviour, AppendString) ->
    DstBehaviorMods = agb_behaviour:get_behaviour_modules(DestBehaviour),
    HeaderString = agb_string:sprintf(
            "-module(~s).\n-behaviour(~s).\n-export([get_mods/0]).\nget_mods()->~p.\n",
            [MemMod, ?MODULE, DstBehaviorMods]),
    agb_behaviour:ensure_mem_module(MemMod, HeaderString ++ AppendString).
