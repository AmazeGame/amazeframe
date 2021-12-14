%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------

-module(agb_txtcolor).

-export([
    black/1,
    blackb/1,
    red/1,
    redb/1,
    green/1,
    greenb/1,
    blue/1,
    blueb/1
]).
-export([
    yellow/1,
    yellowb/1,
    magenta/1,
    magentab/1,
    cyan/1,
    cyanb/1,
    white/1,
    whiteb/1
]).
-export([
    on_black/1,
    on_red/1,
    on_green/1,
    on_blue/1,
    on_yellow/1,
    on_magenta/1,
    on_cyan/1,
    on_white/1
]).
-export([
    rgb/2,
    on_rgb/2
]).
-export([
    true/2,
    on_true/2
]).

-define(ESC, <<"\e[">>).
-define(RST, <<"0">>).
-define(BOLD, <<"1">>).
-define(SEP, <<";">>).
-define(END, <<"m">>).

%% Colors
-define(BLACK, <<"30">>).
-define(RED, <<"31">>).
-define(GREEN, <<"32">>).
-define(YELLOW, <<"33">>).
-define(BLUE, <<"34">>).
-define(MAGENTA, <<"35">>).
-define(CYAN, <<"36">>).
-define(WHITE, <<"37">>).
-define(DEFAULT, <<"39">>).

%% Background colors
-define(BLACK_BG, <<"40">>).
-define(RED_BG, <<"41">>).
-define(GREEN_BG, <<"42">>).
-define(YELLOW_BG, <<"43">>).
-define(BLUE_BG, <<"44">>).
-define(MAGENTA_BG, <<"45">>).
-define(CYAN_BG, <<"46">>).
-define(WHITE_BG, <<"47">>).
-define(DEFAULT_BG, <<"49">>).

%% RGB
-define(RGB_FG, [<<"38">>, ?SEP, <<"5">>]).
-define(RGB_BG, [<<"48">>, ?SEP, <<"5">>]).

%% True 24-bit colors
-define(TRUE_COLOR_FG, [<<"38">>, ?SEP, <<"2">>]).
-define(TRUE_COLOR_BG, [<<"48">>, ?SEP, <<"2">>]).


-spec black(Text :: string() | binary()) ->
    [iolist()].
black(Text) ->
    [color(?BLACK), Text, reset()].
-spec blackb(Text :: string() | binary()) ->
    [iolist()].
blackb(Text) ->
    [colorb(?BLACK), Text, reset()].
-spec red(Text :: string() | binary()) ->
    [iolist()].
red(Text) ->
    [color(?RED), Text, reset()].
-spec redb(Text :: string() | binary()) ->
    [iolist()].
redb(Text) ->
    [colorb(?RED), Text, reset()].
-spec green(Text :: string() | binary()) ->
    [iolist()].
green(Text) ->
    [color(?GREEN), Text, reset()].
-spec greenb(Text :: string() | binary()) ->
    [iolist()].
greenb(Text) ->
    [colorb(?GREEN), Text, reset()].
-spec yellow(Text :: string() | binary()) ->
    [iolist()].
yellow(Text) ->
    [color(?YELLOW), Text, reset()].
-spec yellowb(Text :: string() | binary()) ->
    [iolist()].
yellowb(Text) ->
    [colorb(?YELLOW), Text, reset()].
-spec blue(Text :: string() | binary()) ->
    [iolist()].
blue(Text) ->
    [color(?BLUE), Text, reset()].
-spec blueb(Text :: string() | binary()) ->
    [iolist()].
blueb(Text) ->
    [colorb(?BLUE), Text, reset()].
-spec magenta(Text :: string() | binary()) ->
    [iolist()].
magenta(Text) ->
    [color(?MAGENTA), Text, reset()].
-spec magentab(Text :: string() | binary()) ->
    [iolist()].
magentab(Text) ->
    [colorb(?MAGENTA), Text, reset()].
-spec cyan(Text :: string() | binary()) ->
    [iolist()].
cyan(Text) ->
    [color(?CYAN), Text, reset()].
-spec cyanb(Text :: string() | binary()) ->
    [iolist()].
cyanb(Text) ->
    [colorb(?CYAN), Text, reset()].
-spec white(Text :: string() | binary()) ->
    [iolist()].
white(Text) ->
    [color(?WHITE), Text, reset()].
-spec whiteb(Text :: string() | binary()) ->
    [iolist()].
whiteb(Text) ->
    [colorb(?WHITE), Text, reset()].
-spec on_black(Text :: string() | binary()) ->
    [iolist()].
on_black(Text) ->
    [color(?BLACK_BG), Text, reset_bg()].
-spec on_red(Text :: string() | binary()) ->
    [iolist()].
on_red(Text) ->
    [color(?RED_BG), Text, reset_bg()].
-spec on_green(Text :: string() | binary()) ->
    [iolist()].
on_green(Text) ->
    [color(?GREEN_BG), Text, reset_bg()].
-spec on_blue(Text :: string() | binary()) ->
    [iolist()].
on_blue(Text) ->
    [color(?BLUE_BG), Text, reset_bg()].
-spec on_yellow(Text :: string() | binary()) ->
    [iolist()].
on_yellow(Text) ->
    [color(?YELLOW_BG), Text, reset_bg()].
-spec on_magenta(Text :: string() | binary()) ->
    [iolist()].
on_magenta(Text) ->
    [color(?MAGENTA_BG), Text, reset_bg()].
-spec on_cyan(Text :: string() | binary()) ->
    [iolist()].
on_cyan(Text) ->
    [color(?CYAN_BG), Text, reset_bg()].
-spec on_white(Text :: string() | binary()) ->
    [iolist()].
on_white(Text) ->
    [color(?WHITE_BG), Text, reset_bg()].

-type color_dim_value() :: 0..255.
-type rgb_value() :: [color_dim_value()].
-spec rgb(RGB :: rgb_value(), Text :: string() | binary()) ->
    [iolist()].
rgb(RGB, Text) ->
    [?ESC, ?RGB_FG, ?SEP, rgb_color(RGB), ?END, Text, reset()].

-spec on_rgb(RGB :: rgb_value(), Text :: string() | binary()) ->
    [iolist()].
on_rgb(RGB, Text) ->
    [?ESC, ?RGB_BG, ?SEP, rgb_color(RGB), ?END, Text, reset_bg()].

-spec true(RGB :: rgb_value(), Text :: string() | binary()) ->
    [iolist()].
true(RGB, Text) ->
    [?ESC, ?TRUE_COLOR_FG, ?SEP, true_color(RGB), ?END, Text, reset()].

-spec on_true(RGB :: rgb_value(), Text :: string() | binary()) ->
    [iolist()].
on_true(RGB, Text) ->
    [?ESC, ?TRUE_COLOR_BG, ?SEP, true_color(RGB), ?END, Text, reset()].

%% Internal
color(Color) ->
    <<?ESC/binary, Color/binary, ?END/binary>>.

colorb(Color) ->
    <<?ESC/binary, Color/binary, ?SEP/binary, ?BOLD/binary, ?END/binary>>.

rgb_color([R, G, B]) when R >= 0, R =< 5, G >= 0, G =< 5, B >= 0, B =< 5 ->
    integer_to_list(16 + (R * 36) + (G * 6) + B).

true_color([R1, R2, G1, G2, B1, B2]) ->
    R = erlang:list_to_integer([R1, R2], 16),
    G = erlang:list_to_integer([G1, G2], 16),
    B = erlang:list_to_integer([B1, B2], 16),
    true_color([R, G, B]);
true_color([R, G, B]) when R >= 0, R =< 255, G >= 0, G =< 255, B >= 0, B =< 255 ->
    [integer_to_list(R), ?SEP, integer_to_list(G), ?SEP, integer_to_list(B)].

reset() ->
    <<?ESC/binary, ?RST/binary, ?END/binary>>.

reset_bg() ->
    <<?ESC/binary, ?DEFAULT_BG/binary, ?END/binary>>.
