%% this file should be included after export and befor source code

-ifndef(_AGB_VARIABLE_HRL_).
-define(_AGB_VARIABLE_HRL_,1).

-export([put/2,puto/1,geto/1,getv/1,delete/1]).

-spec put(Key :: any(), Value :: term()) -> boolean().
put(Key, Value) ->
    agb_ets:put(table(), Key, Value).

-spec puto(Object :: term()) -> boolean().
puto(Object) ->
    agb_ets:put(table(), Object).


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

-spec delete(Key :: any()) ->  boolean().
delete(Key) ->
    agb_ets:delete(table(), Key).


-endif.