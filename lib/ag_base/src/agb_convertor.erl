%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------

-module(agb_convertor).
-export([
    to_binary/1,
    to_string/1,
    to_atom/1,
    to_integer/1
]).

-spec to_binary(Value :: any()) ->
    binary().
to_binary(X) when is_list(X) ->
    list_to_binary(X);
to_binary(X) when is_atom(X) ->
    atom_to_binary(X, utf8);
to_binary(X) when is_binary(X) ->
    X;
to_binary(X) when is_integer(X) ->
    integer_to_binary(X);
to_binary(X) when is_float(X) ->
    throw({cannot_store_floats, X});
to_binary(X) ->
    term_to_binary(X).

-spec to_string(Value :: any()) ->
    string().
to_string(X) ->
    agb_string:to_string(X).

-spec to_atom(Value :: any()) ->
    atom().
to_atom(X) when is_binary(X) ->
    binary_to_atom(X, utf8);
to_atom(X) when is_atom(X) ->
    X;
to_atom(X) when is_list(X) ->
    list_to_atom(X);
to_atom(X) ->
    throw({cannot_convertor_atom, X}).

-spec to_integer(Value :: any()) ->
    integer().
to_integer(X) when is_list(X) ->
    list_to_integer(X);
to_integer(X) when is_binary(X) ->
    binary_to_integer(X);
to_integer(X) when is_integer(X) ->
    X;
to_integer(X) ->
    throw({cannot_convertor_integer, X}).
