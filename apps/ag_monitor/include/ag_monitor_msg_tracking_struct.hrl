%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.11.08
%%%-------------------------------------------------------------------
%%%

-define(TRACKING_INFO_KEY, <<"##tracking_Info##">>).
-define(TRACKING_ID, <<"tracking_id">>).
-define(TRACKING_REQ_START, <<"tracking_req_start">>).
-define(TRACKING_REQ_END, <<"tracking_req_end">>).
-define(TRACKING_EXEC_START, <<"tracking_exec_start">>).
-define(TRACKING_EXEC_END, <<"tracking_exec_end">>).
-define(TRACKING_REQUEST_NAME, <<"tracking_request_name">>).
-define(TRACKING_RESPONSE_NAME, <<"tracking_response_name">>).
-define(TRACKING_MSG_TYPE, <<"tracking_msg_type">>).
-define(TRACKING_IN_MSG, <<"tracking_in_msg">>).
-define(TRACKING_OUT_MSG, <<"tracking_out_msg">>).
-define(TRACKING_STATUS, <<"tracking_status">>).
-define(TRACKING_REQ_TIME, <<"tracking_req_time">>).
-define(TRACKING_EXEC_TIME, <<"tracking_exec_time">>).
-define(TRACKING_CLIENT_IP,<<"tracking_client_ip">>).
-define(TRACKING_USER_ID, <<"tracking_user_id">>).

-type tracking_status() :: fin|timeout|exit.