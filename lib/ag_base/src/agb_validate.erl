%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.13
%%%-------------------------------------------------------------------
-module(agb_validate).

-export([
    validate_integer/1,
    validate_atom/1,
    validate_atom_list/1,
    validate_tuple/1,
    validate_tuple_list/1,
    validate_integer_list/1,
    validate_float_list/1,
    validate_integer_list_list/1,
    validate_number/1,
    validate_number_list/1,
    validate_string_list_list/1,
    validate_string_list/1,
    validate_var/1,
    validate_any_list/1,
    to_round_int_str/1
]).

%%
%% API Functions
%%
-spec validate_number(NumberString :: string()) ->
    boolean().
validate_number(NumberString) ->
    try
        try
            _ = list_to_integer(NumberString), %% try integer
            true
        catch
            _:_ ->
                _ = list_to_float(NumberString), %% try float
                true
        end
    catch
        _:_ ->
            false
    end.

-spec validate_integer(IntString :: string()) ->
    boolean().
validate_integer(IntString) ->
    try
        _ = list_to_integer(IntString),
        true
    catch
        _:_ ->
            false
    end.

-spec validate_var(VariantString :: string()) ->
    boolean().
validate_var(VariantString) ->
    try
        _ = agb_string:string_to_term(VariantString),
        true
    catch
        _:_ ->
            false
    end.

-spec validate_atom(AtomString :: string()) ->
    boolean().
validate_atom(AtomString) ->
    try
        {ok, Atom} = agb_string:string_to_term(AtomString),
        erlang:is_atom(Atom)
    catch
        _:_ ->
            false
    end.

-spec validate_atom_list(AtomArray :: string()) ->
    boolean().
validate_atom_list(AtomArray) ->
    try
        {ok, AtomList} = agb_string:string_to_term(AtomArray),
        IsList = erlang:is_list(AtomList),
        if
            not IsList ->
                false;
            true ->
                lists:all(
                    fun(Atom) ->
                        erlang:is_atom(Atom)
                    end,
                    AtomList
                )
        end
    catch
        _:_ ->
            false
    end.

-spec validate_tuple(TupleString :: string()) ->
    boolean().
validate_tuple(TupleString) ->
    try
        {ok, Tuple} = agb_string:string_to_term(TupleString),
        erlang:is_tuple(Tuple)
    catch
        _:_ ->
            false
    end.

-spec validate_tuple_list(TupleArray :: string()) ->
    boolean().
validate_tuple_list(TupleArray) ->
    try
        {ok, TupleList} = agb_string:string_to_term(TupleArray),
        IsList = erlang:is_list(TupleList),
        if
            not IsList ->
                false;
            true ->
                lists:all(
                    fun(Tuple) ->
                        erlang:is_tuple(Tuple)
                    end,
                    TupleList
                )
        end
    catch
        _:_ ->
            false
    end.


-spec validate_any_list(Array :: string()) ->
    boolean().
validate_any_list(Array) ->
    try
        {ok, List} = agb_string:string_to_term(Array),
        erlang:is_list(List)
    catch
        _:_ ->
            false
    end.

-spec validate_integer_list(IntArray :: string()) ->
    boolean().
validate_integer_list(IntArray) ->
    try
        {ok, IntList} = agb_string:string_to_term(IntArray),
        case is_list(IntList) of
            true ->
                lists:all(
                    fun(IntStr) ->
                        try
                            _ = is_integer(IntStr),
                            true
                        catch
                            _:_ ->
                                false
                        end
                    end,
                    IntList
                );
            false ->
                false
        end
    catch
        _:_ ->
            false
    end.

-spec validate_float_list(FltArray :: string()) ->
    boolean().
validate_float_list(FltArray) ->
    try
        {ok, IntList} = agb_string:string_to_term(FltArray),
        lists:all(
            fun(IntStr) ->
                try
                    is_float(IntStr)
                catch
                    _:_ ->
                        false
                end
            end,
            IntList
        )
    catch
        _:_ ->
            false
    end.

-spec validate_integer_list_list(IntArray :: string()) ->
    boolean().
validate_integer_list_list(IntArray) ->
    try
        {ok, IntListList} = agb_string:string_to_term(IntArray),

        ListFlag = erlang:is_list(IntListList),
        if
            not ListFlag ->
                false;
            true ->
                lists:all(
                    fun(IntList) ->
                        IntListFlag = erlang:is_list(IntList),
                        if
                            not IntListFlag ->
                                false;
                            true ->
                                lists:all(
                                    fun(Int) ->
                                        erlang:is_integer(Int)
                                    end,
                                    IntList
                                )
                        end
                    end,
                    IntListList
                )
        end
    catch
        _:_ ->
            false
    end.

-spec validate_string_list_list(StringArray :: string()) ->
    boolean().
validate_string_list_list(StringArray) ->
    try
        {ok, StringListList} = agb_string:string_to_term(StringArray),
        ListFlag = erlang:is_list(StringListList),
        if
            not ListFlag ->
                false;
            true ->
                lists:all(
                    fun(StringList) ->
                        StringListFlag = erlang:is_list(StringList),
                        if
                            not StringListFlag ->
                                false;
                            true ->
                                lists:all(
                                    fun(String) ->
                                        erlang:is_list(String)
                                    end,
                                    StringList
                                )
                        end
                    end,
                    StringListList
                )
        end
    catch
        _:_ ->
            false
    end.

-spec validate_string_list(StringArray :: string()) ->
    boolean().
validate_string_list(StringArray) ->
    try
        {ok, StringList} = agb_string:string_to_term(StringArray),
        ListFlag = erlang:is_list(StringList),
        if
            not ListFlag ->
                talse;
            true ->
                lists:all(
                    fun(String) ->
                        erlang:is_list(String)
                    end,
                    StringList
                )
        end
    catch
        _:_ ->
            false

    end.

-spec validate_number_list(NumArray :: string()) ->
    boolean().
validate_number_list(NumArray) ->
    try
        {ok, NumList} = agb_string:string_to_term(NumArray),
        lists:all(
            fun(NumStr) ->
                try
                    is_integer(NumStr) orelse is_float(NumStr)
                catch
                    _:_ ->
                        false
                end
            end,
            NumList
        )
    catch
        _:_ ->
            false
    end.

-spec to_round_int_str(CellStr :: string()) -> boolean().
to_round_int_str(CellStr) ->
    try
        Flt = list_to_float(CellStr),
        integer_to_list(trunc(Flt + 0.5))
    catch
        _:_ ->
            try
                _ = list_to_integer(CellStr), CellStr
            catch
                _:_ ->
                    false
            end
    end.
