%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------

-module(agb_uri).

-define(REPLACE(Map,Scheme,Port), Scheme-> case maps:is_key(port,Map) of true-> maps:get(port,Map); false -> Port end ).
-define(NOREPLACE(Map,Scheme,Port), Scheme-> case maps:is_key(port,Map) of true-> maps:get(port,Map); false -> Port end ).
%% API
-export([parse/1]).
-spec parse(Uri :: string()) ->
    {ok, http_uri:parse_result()} | {error, term()}.
parse(Uri) ->
    Result = uri_string:parse(Uri),
    maps:fold(
        fun(scheme,Scheme,AccIn)->
                P = case Scheme of
                        ?REPLACE(AccIn,"http",80);
                        ?REPLACE(AccIn,"https",443);
                        ?REPLACE(AccIn,"ftp",21);
                        ?REPLACE(AccIn,"ws",80);
                        ?REPLACE(AccIn,"wss",443);
                        ?REPLACE(AccIn,"mysql",3306);
                        ?REPLACE(AccIn,"mongodb",27017);
                        ?REPLACE(AccIn,"redis",6379);
                        ?REPLACE(AccIn,"postgresql",5432);
                        ?REPLACE(AccIn,"orcale",7080);
                        _->agb_error:error("not support scheme : ~s" , [Scheme])
                    end,
                maps:put(scheme,Scheme,AccIn#{port=>P});
            (K,V,AccIn)-> 
                maps:put(K,V,AccIn)
        end,Result,Result).
