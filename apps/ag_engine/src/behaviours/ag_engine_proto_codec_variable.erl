-module(ag_engine_proto_codec_variable).
-behaviour(agb_variable).
-export([table/0]).
-include_lib("ag_base/include/agb_variable.hrl").

table()->
    'AMAZEGAME_PROTO_CODEC_VARIABLE'.
