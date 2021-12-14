%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------
-module(agb_behaviour).


-define(LIB_KEY, <<"/ag_">>).
%% API
-export([
    check_behaviour/2,
    get_behaviour_modules/1,
    ensure_mem_module/2
]).
-export([
    get_function_modules/1, get_function_modules/2,
    check_functions/2
]).

-export([get_module_attribute/2]).


-spec check_behaviour(Module :: atom(), Behavior :: atom()) ->
    boolean().
check_behaviour(Module, Behaviour) ->
    ensure_mod_load(Module),
    lists:member({behaviour, [Behaviour]}, Module:module_info(attributes)).

ensure_mod_load(Module) ->
    case code:is_loaded(Module) of
        false ->
            {module, Module} = code:load_file(Module);
        _ ->
            ok
    end.

-spec check_otp_lib(Path, Pattern) ->
    Bool when
    Path :: iodata(),
    Pattern :: binary(),
    Bool :: boolean().
check_otp_lib(Path, Pattern) when is_binary(Path) ->
    nomatch =/= binary:match(Path, Pattern);
check_otp_lib(Path, Pattern) when is_list(Path) ->
    check_otp_lib(list_to_binary(Path), Pattern).

%%@doc 获取behavior搜索路径
-spec user_path() ->
    UserPath when
    UserPath :: [string(), ...].
user_path() ->
    {AmazeGamePath, Other} =
        lists:partition(
            fun(Path) ->
                check_otp_lib(Path, ?LIB_KEY)
            end,
            code:get_path()
        ),
    InludePath = include_path(Other, app_patttern()),
    InludePath ++ AmazeGamePath.

%%@private 筛选ag_base中include_path相关的路径
-spec include_path(CodePath, Patterns) ->
    IncludePaths when
    CodePath :: [string(), ...],
    Patterns :: [binary(), ...],
    IncludePaths :: [string(), ...].
include_path(CodePath, Patterns) ->
    include_path_1(CodePath, Patterns, []).

include_path_1([], _Patterns, Acc) ->
    Acc;
include_path_1(CodePath, [H | T], Acc) ->
    {Paths, Other} =
        lists:partition(
            fun(Path) ->
                check_otp_lib(Path, H)
            end,
            CodePath
        ),
    include_path_1(Other, T, Paths ++ Acc);
include_path_1(_CodePath, [], Acc) ->
    Acc.

%%@private 获取配置
-spec app_patttern() ->
    [binary() | string()].
app_patttern() ->
    {_,_, ModuleFile}= code:get_object_code(?MODULE),
    FGetApp = fun(Path,FGetApp0)->
                CurDir = filename:dirname(Path),
                case filename:basename(CurDir) of 
                    "ebin" ->
                        EbUpper = filename:dirname(CurDir),
                        {filename:dirname(EbUpper),filename:basename(EbUpper)};
                    _ ->
                        FGetApp0(CurDir,FGetApp0)
                end
            end,
    
    {_AppDir,AppName} = FGetApp(ModuleFile,FGetApp),
    [filename:absname_join(<<"/lib">>, AppName)].

-spec get_behaviour_modules(Behaviour :: atom()) ->
    list().
get_behaviour_modules(Behaviour) ->
    ModFiles =
        lists:flatmap(
            fun(F) ->
                filelib:wildcard(filename:absname_join(F, "*.beam"))
            end,
            user_path()
        ),
    lists:filtermap(
        fun(F) ->
            Mod = list_to_atom(filename:basename(F, ".beam")),
            case check_behaviour(Mod, Behaviour) of
                false ->
                    false;
                true ->
                    {true, Mod}
            end
        end, ModFiles).

-spec ensure_mem_module(Module :: atom(), String :: string()) ->
    ok | {error, badarg | code:load_error_rsn()}.
ensure_mem_module(Module, String) ->
    {Module, Code} = agb_dynamic_compile:from_string(String),
    case code:load_binary(Module, [], Code) of
        {module, Module} ->
            ok;
        {error, What} ->
            {error, What}
    end.

-spec get_module_attribute(Module :: atom(), Key :: atom()) ->
    list() | no_return.
get_module_attribute(Module, Key) ->
    case lists:keyfind(Key, 1, Module:module_info(attributes)) of
        false ->
            agb_error:sprintf("~s have not ~s for attribute", [Module, Key]);
        {_, Value} ->
            Value
    end.

-spec get_function_modules(Functions :: list()) ->
    list().
get_function_modules(Functions) ->
    get_function_modules("*.beam", Functions).

-spec get_function_modules(Wild :: string(), Functions :: list()) ->
    list().
get_function_modules(Wild, Functions) ->
    ModFiles =
        lists:flatmap(
            fun(F) ->
                filelib:wildcard(filename:absname_join(F, Wild))
            end,
            user_path()
        ),
    lists:filtermap(
        fun(F) ->
            Mod = list_to_atom(filename:basename(F, ".beam")),
            case check_functions(Mod, Functions) of
                false ->
                    false;
                true ->
                    {true, Mod}
            end
        end, ModFiles).

-spec check_functions(Module :: module(), Functions :: [atom()]) ->
    boolean().
check_functions(Module, Functions) ->
    lists:all(
        fun({Function, Arity}) ->
            erlang:function_exported(Module, Function, Arity)
        end,
        Functions
    ).
