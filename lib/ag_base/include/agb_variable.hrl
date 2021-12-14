-ifndef(_AGB_VARIABLE_HRL_).
-define(_AGB_VARIABLE_HRL_,1).

-export([put/2,geto/1,getv/1]).

-spec put(Key :: any(), Value :: term()) -> boolean().
put(Key, Value) ->
    agb_ets:put(table(), Key, Value).

-spec geto(Key :: any()) -> term().
geto(Key) ->
    case agb_ets:lookup(table(), Key) of
        [] ->
            undefined;
        Obj ->
            Obj
    end.

-spec getv(Key :: any()) ->   atom().
getv(Key) ->
    case agb_ets:lookup(table(), Key) of
        [] ->
            undefined;
        {_, ObjV} ->
            ObjV
    end.

-endif.