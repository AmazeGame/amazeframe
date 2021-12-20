-module(ag_engine_3platform_variable).
-behaviour(agb_variable).
-export([table/0]).
-include_lib("ag_base/include/agb_variable.hrl").

-record(auth_type, {type :: term(), module :: module()}).
table()->
    {'AMAZEGAME_3PLATFORM_VARIABLE',[{keypos, #auth_type.type}]}.
