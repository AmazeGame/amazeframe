%%%-------------------------------------------------------------------
%%% @author BlackCat 
%%% @author adrianx  <adrianx.lau@gmail.com> <adrianx@163.com>
%%% @copyright (C) 2021, AmazeGame
%%% @doc
%%% @end
%%% Created : 2021.11.04
%%%-------------------------------------------------------------------
-ifndef(__ENGINE_CODE_DEFINES_HRL_).
-define(__ENGINE_CODE_DEFINES_HRL_,1).


-define(REQUEST_SUCCESSED,                                           				 0).
-define(ERROR_CODE_INTERNAL_SERVER_ERROR,                                           -1).    %服务器内部错误
-define(ERROR_CODE_MSG_WITHOUT_HANDLER,                                             -2).    %意外的消息名

-define(ERROR_CODE_CANNOT_FIND_DEVICE,                                              -3).    %错误的登陆类型
-define(ERROR_CODE_AUTH_PLAYER_FAILED,                                              -4).    %验签错误

-define(ERROR_CODE_AUTH_3RD_FAILED,                                                 -5).    %第三方验证错误
-define(ERROR_CODE_AUTH_PASSWORD_FAILED,                                            -6).    %登录密码错误
-define(ERROR_CODE_IS_ONLINE,                                                       -7).    %登录的id已经在线了

-define(ERROR_CODE_CANNOT_FIND_SESSION,                                             -12).   %session失效
-define(ERROR_CODE_ILLEGAL_VERSION_REQUEST,                                         -13).   %客户端版本不匹配
-define(ERROR_CODE_ILLEGAL_REQUEST,	                                            	-14).   %消息时间验证失败
-define(ERROR_CODE_SIGNATURE_CHECK_ERROR,                                           -15).
-define(ERROR_CODE_PACKAGE_NUMBER,                                                  -16).   %包号校验失败
-define(ERROR_CODE_USER_NOT_EXIST,                                                  -17).



-endif. %%__ENGINE_CODE_DEFINES_HRL_
