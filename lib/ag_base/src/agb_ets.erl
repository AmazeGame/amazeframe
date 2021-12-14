%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------
-module(agb_ets).

%% API
-export([
    init/1, init/2,
    put/2, put/3,
    get/2,
    lookup/2,
    match_object/2,
    clone/1, clone/2,
    delete/2
]).

-spec init(Table :: atom()) ->
    atom().
init(Table) ->
    case ets:info(Table) of
        undefined ->
            Table = ets:new(Table, [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}]);
        _InfoList ->
            Table
    end.

-spec init(Table :: atom(), Opts :: [term()]) ->
    atom().
init(Table, Opts) ->
    case ets:info(Table) of
        undefined ->
            ComOpts = [set, named_table, public, {write_concurrency, true}, {read_concurrency, true}],
            Table = ets:new(Table, combin_ets_opts(ComOpts, Opts));
        _InfoList ->
            Table
    end.

combin_ets_opts(ComOpts, NewOpts) ->
    do_combin_ets_opt(ComOpts, NewOpts).

do_combin_ets_opt(ComOpts, []) ->
    ComOpts;
do_combin_ets_opt(ComOpts, [public | Opts]) ->
    Opts1 = lists:delete(private, ComOpts),
    Opts2 = lists:delete(protected, Opts1),
    do_combin_ets_opt([public | Opts2], Opts);
do_combin_ets_opt(ComOpts, [private | Opts]) ->
    Opts1 = lists:delete(public, ComOpts),
    Opts2 = lists:delete(protected, Opts1),
    do_combin_ets_opt([private | Opts2], Opts);
do_combin_ets_opt(ComOpts, [protected | Opts]) ->
    Opts1 = lists:delete(public, ComOpts),
    Opts2 = lists:delete(private, Opts1),
    do_combin_ets_opt([protected | Opts2], Opts);
do_combin_ets_opt(ComOpts, [set | Opts]) ->
    Opts1 = lists:delete(ordered_set, ComOpts),
    Opts2 = lists:delete(bag, Opts1),
    Opts3 = lists:delete(duplicate_bag, Opts2),
    do_combin_ets_opt([set | Opts3], Opts);
do_combin_ets_opt(ComOpts, [ordered_set | Opts]) ->
    Opts1 = lists:delete(set, ComOpts),
    Opts2 = lists:delete(bag, Opts1),
    Opts3 = lists:delete(duplicate_bag, Opts2),
    do_combin_ets_opt([ordered_set | Opts3], Opts);
do_combin_ets_opt(ComOpts, [bag | Opts]) ->
    Opts1 = lists:delete(ordered_set, ComOpts),
    Opts2 = lists:delete(set, Opts1),
    Opts3 = lists:delete(duplicate_bag, Opts2),
    do_combin_ets_opt([bag | Opts3], Opts);
do_combin_ets_opt(ComOpts, [duplicate_bag | Opts]) ->
    Opts1 = lists:delete(ordered_set, ComOpts),
    Opts2 = lists:delete(set, Opts1),
    Opts3 = lists:delete(set, Opts2),
    do_combin_ets_opt([duplicate_bag | Opts3], Opts);
do_combin_ets_opt(ComOpts, [{heir, Pid, HeirData} | Opts]) ->
    Opts1 = lists:keydelete(heir, 1, ComOpts),
    do_combin_ets_opt([{heir, Pid, HeirData} | Opts1], Opts);
do_combin_ets_opt(ComOpts, [{heir, none} | Opts]) ->
    Opts1 = lists:keydelete(heir, 1, ComOpts),
    do_combin_ets_opt([{heir, none} | Opts1], Opts);
do_combin_ets_opt(ComOpts, [{read_concurrency, Cocurrenty} | Opts]) ->
    Opts1 = lists:keydelete(read_concurrency, 1, ComOpts),
    do_combin_ets_opt([{read_concurrency, Cocurrenty} | Opts1], Opts);
do_combin_ets_opt(ComOpts, [compressed | Opts]) ->
    Opts1 = lists:keydelete(compressed, 1, ComOpts),
    do_combin_ets_opt([compressed | Opts1], Opts);
do_combin_ets_opt(ComOpts, [{keypos, Pos} | Opts]) ->
    Opts1 = lists:keydelete(keypos, 1, ComOpts),
    do_combin_ets_opt([{keypos, Pos} | Opts1], Opts).

-spec put(Table :: atom(), Key :: any(), Value :: term()) ->
    boolean().
put(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}).

-spec put(Table :: atom(), Objects :: tuple() | [tuple()]) ->
    boolean().
put(Table, Objects) ->
    ets:insert(Table, Objects).

-spec get(Table :: atom(), Key :: any()) ->
    term().
get(Table, Key) ->
    ets:lookup_element(Table, Key, 2).

-spec lookup(Table :: atom(), Key :: any()) ->
    []|tuple().
lookup(Table, Key) ->
    case ets:lookup(Table, Key) of
        [] ->
            [];
        [Object] ->
            Object
    end.

-spec match_object(Table :: atom(), Object :: tuple()) ->
    []|[tuple()].
match_object(Table, Object) ->
    ets:match_object(Table, Object).

-spec clone(SrcEts :: ets:tid()) ->
    ets:tid()|false.
clone(SrcEts) ->
    case get_opts(SrcEts) of
        undefined ->
            false;
        {Opt, OriginName} ->
            OutEts = ets:new(OriginName, lists:delete(named_table, Opt)),
            ets:foldl(
                fun(Object, Result) ->
                    ets:insert(OutEts, Object),
                    Result
                end,
                true,
                SrcEts
            ),
            OutEts
    end.

-spec clone(SrcEts :: ets:tid(), DstName :: atom()) ->
    atom()|false.
clone(SrcEts, DstName) ->
    case get_opts(SrcEts) of
        undefined ->
            false;
        {Opt, _OriginName} ->
            OutEts = ets:new(DstName, Opt),
            ets:foldl(
                fun(Object, Result) ->
                    ets:insert(OutEts, Object),
                    Result
                end,
                true,
                SrcEts),
            OutEts
    end.

get_opts(Ets) ->
    case ets:info(Ets) of
        undefined ->
            undefined;
        Infos ->
            ReadOpt = lists:keyfind(read_concurrency, 1, Infos),
            WriteOpt = lists:keyfind(write_concurrency, 1, Infos),
            KeyposOpt = lists:keyfind(keypos, 1, Infos),
            {name, Name} = lists:keyfind(name, 1, Infos),
            {_, Type} = lists:keyfind(type, 1, Infos),

            NewOpt = [ReadOpt, WriteOpt, KeyposOpt, Type],

            NewOpt2 =
                case lists:keyfind(named_table, 1, Infos) of
                    {named_table, true} ->
                        [named_table | NewOpt];
                    _ ->
                        NewOpt
                end,
            NewOpt3 =
                case lists:keyfind(protection, 1, Infos) of
                    {protection, Protection} ->
                        [Protection | NewOpt2];
                    _ ->
                        NewOpt2
                end,
            {NewOpt3, Name}
    end.
-spec delete(Table :: atom(), Key :: term()) ->
    boolean().
delete(Table, Key) ->
    ets:delete(Table, Key).
