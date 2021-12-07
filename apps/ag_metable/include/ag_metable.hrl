%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.02
%%%-------------------------------------------------------------------
%%%

-ifndef(_GEN_DYNAMIC_XLSX_HRL_).
-define(_GEN_DYNAMIC_XLSX_HRL_,1).

-record(table_field, {column::integer(), index ::integer(),name=[]::string(), type = string:: ag_metable:field_type(), descript=[]::string(),enabled=true::boolean()}).
-record(table_header, {table::atom(), fields=[]::[ag_metable:table_field()], tabid::ets:tid(),with_table=false::boolean()}).

-endif.%_GEN_DYNAMIC_XLSX_HRL_