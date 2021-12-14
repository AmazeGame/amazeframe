%%%-------------------------------------------------------------------
%%% @author adrianx 
%%% @email  adrianx.lau@gmail.com adrianx@163.com
%%% @copyright (C) 2021, Amaze Game framwork
%%% @doc
%%%
%%% @end
%%% Created : 2021.10.12 
%%%-------------------------------------------------------------------
-module(agb_string).

-export([
    sprintf/2,
    make_node/2,
    make_node_str/2,
    make_socket_str/1,
    make_ipaddress_str/1
]).

-export([
    string_to_term/1,
    term_to_string/1,
    eval_string/1, eval_string/2,
    datetime_to_ftp_lmtm/1,
    datetime_to_string/1,
    datetime_to_short_string/1,
    string_to_print_term/1,
    token_str/2,
    replace/3,
    replace_between/4,
    validate_visible/1,
    suffixed_string_len/3,
    prefixed_string_len/3,
    to_string/1
]).

-export([
    encode_unicode_escape/1,
    decode_unicode_escape/1
]).

-export([make_int_str/2, make_int_str/1]).
-export([upper_first_char/1]).
-export([
    integer_to_hexstring/1,
    binary_to_hexstring/1,
    hex_octet/1,
    hexstring_to_binary/1
]).
-export([
    md5_string/1,
    md5_binary_str/1
]).
-export([sha256_binary_str/1]).

%%
%% API Functions
%%
-spec make_int_str(Int :: integer()) ->
    string().
make_int_str(Int) ->
    make_int_str(0, Int).
-spec make_int_str(LenNum :: integer(), Int :: integer()) ->
    string().
make_int_str(LenNum, Int) ->
    Str = integer_to_list(Int),
    StrLen = string:len(Str),
    NeedAdd = LenNum - StrLen,
    if
        NeedAdd > 0 ->
            lists:foldl(
                fun(_, AccStr) ->
                    string:concat("0", AccStr)
                end,
                Str,
                lists:seq(1, NeedAdd)
            );
        true ->
            Str
    end.

-spec sprintf(Format :: io:format(), Data :: [term()]) ->
    string().
sprintf(Format, Data) ->
    lists:flatten(io_lib:format(Format, Data)).

-spec string_to_term(string()) ->
    term().
string_to_term(String) ->
    case erl_scan:string(lists:append(String, ".")) of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} ->
                    {ok, Term};
                _ ->
                    error(invalidate_term)
            end;
        _ ->
            error(invalidate_term)
    end.

-spec term_to_string(term()) ->
    string().
term_to_string(Term) ->
    lists:flatten(io_lib:format("~w", [Term])).

-spec upper_first_char(Str :: string()) ->
    string().
upper_first_char([First | String]) ->
    NewFirst = string:to_upper(First),
    lists:append([NewFirst], String).

-spec string_to_print_term(String :: string()) ->
    term().
string_to_print_term(String) ->
    lists:foldl(
        fun
            ($", Str0) ->
                lists:append(Str0, [$\\, $"]);
            (C, Str0) ->
                lists:append(Str0, [C])
        end,
        [],
        String
    ).

-spec datetime_to_ftp_lmtm(tuple()) ->
    string().
datetime_to_ftp_lmtm({{Y, Mon, D}, {H, Min, S}}) ->
    sprintf("~p~2..0w~2..0w~2..0w~2..0w~2..0w", [Y, Mon, D, H, Min, S]).

-spec datetime_to_string(tuple()) ->
    string().
datetime_to_string(DateTime) ->
    {{Y, Mon, D}, {H, Min, S}} = DateTime,
    sprintf("~p-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Y, Mon, D, H, Min, S]).

-spec datetime_to_short_string(tuple()) ->
    string().
datetime_to_short_string(DateTime) ->
    {{Y, Mon, D}, {H, Min, S}} = DateTime,
    sprintf("~p~2..0w~2..0w~2..0w~2..0w~2..0w", [Y, Mon, D, H, Min, S]).

-spec make_node_str(SName :: list() | atom(), Ip :: list()) ->
    string().
make_node_str(SName, Ip) when is_list(SName) andalso is_list(Ip) ->
    lists:concat([SName, "@", Ip]);
make_node_str(SName, Ip) when is_atom(SName) andalso is_list(Ip) ->
    make_node_str(atom_to_list(SName), Ip);
make_node_str(SName, Ip) when is_atom(SName) andalso is_atom(Ip) ->
    make_node_str(atom_to_list(SName), atom_to_list(Ip));
make_node_str(_, _) ->
    [].

-spec make_node(SName :: string() | atom(), Ip :: list()) ->
    atom().
make_node(SName, Ip) when is_list(SName) andalso is_list(Ip) ->
    list_to_atom(make_node_str(SName, Ip));
make_node(SName, Ip) when is_atom(SName) andalso is_list(Ip) ->
    list_to_atom(make_node_str(SName, Ip));
make_node(SName, Ip) when is_atom(SName) andalso is_atom(Ip) ->
    make_node(atom_to_list(SName), atom_to_list(Ip));
make_node(_, _) ->
    ''.

-spec make_socket_str(Socket :: inet:socket()) ->
    string().
make_socket_str(Socket) ->
    case inet:peername(Socket) of
        {error, _} ->
            [];
        {ok, {Address, _Port}} ->
            make_ipaddress_str(Address)
    end.

-spec make_ipaddress_str({integer(), integer(), integer(), integer()}) ->
    string().
make_ipaddress_str({A1, A2, A3, A4}) ->
    string:join([integer_to_list(A1),
        integer_to_list(A2),
        integer_to_list(A3),
        integer_to_list(A4)], ".").

-spec token_str(String :: string(), TokenStr :: string()) ->
    list().
token_str(String, TokenStr) ->
    Len = string:len(TokenStr),
    case string:str(String, TokenStr) of
        0 ->
            [String];
        1 ->
            LeftString = string:sub_string(String, Len + 1),
            token_str(LeftString, TokenStr);
        I ->
            FirstString = string:sub_string(String, 1, I - 1),
            LeftString = string:sub_string(String, I + Len),
            [FirstString | token_str(LeftString, TokenStr)]
    end.

-spec replace(String :: string(), TokenStr :: string(), NewString :: string()) ->
    string().
replace(String, SearchPattern, Replacement) ->
    lists:concat(string:replace(String, SearchPattern, Replacement, all)).

token_between(String, BeginToken, EndToken) ->
    BLen = string:len(BeginToken),
    ELen = string:len(EndToken),
    token_between(String, BeginToken, EndToken, BLen, ELen).

token_between([], _BeginToken, _EndToken, _BLen, _ELen) -> [];
token_between(String, BeginToken, EndToken, BLen, ELen) ->
    case string:str(String, BeginToken) of
        0 ->
            [String];
        I ->
            FirstString = string:sub_string(String, 1, I - 1),
            RightString = string:sub_string(String, I + BLen),
            case RightString of
                [] ->
                    [String];
                _ ->
                    case string:str(RightString, EndToken) of
                        0 ->
                            [String];
                        NI ->
                            TailString = string:sub_string(RightString, NI + ELen),
                            case FirstString of
                                [] ->
                                    lists:append([{}], token_between(TailString, BeginToken, EndToken));
                                _ ->
                                    lists:append([FirstString, {}], token_between(TailString, BeginToken, EndToken))
                            end
                    end
            end
    end.

-spec replace_between(String :: string(), BeginToken :: string(), EndToken :: string(), NewString :: string()) ->
    {integer(), string()}.
replace_between(String, BeginToken, EndToken, NewString) ->
    SplitStrings = token_between(String, BeginToken, EndToken),
    lists:foldl(
        fun(E, {C, L}) ->
            case E of
                {} ->
                    {C + 1, lists:append(L, NewString)};
                O ->
                    {C, lists:append(L, O)}
            end
        end,
        {0, []},
        SplitStrings
    ).

-spec validate_visible(S :: string()) ->
    boolean().
validate_visible(S) ->
    lists:all(
        fun(E) ->
            if
                is_list(E) orelse is_binary(E) orelse is_atom(E) orelse is_tuple(E) ->
                    false;
                true ->
                    true
            end
        end,
        S).

-spec hex_octet(N :: integer()) ->
    string().
hex_octet(N) when N =< 9 ->
    [$0 + N];
hex_octet(N) when N > 15 ->
    lists:append(hex_octet(N bsr 4), hex_octet(N band 15));
hex_octet(N) ->
    [N - 10 + $A].

-spec binary_to_hexstring(Bin :: binary()) ->
    string().
binary_to_hexstring(<<>>) ->
    "";
binary_to_hexstring(Bin) when is_binary(Bin) ->
    <<Byte:8, LefBin/binary>> = Bin,
    lists:append(integer_to_hexstring(Byte), binary_to_hexstring(LefBin));
binary_to_hexstring(_) ->
    "".

-spec hexstring_to_binary(Str :: string()) ->
    binary().
hexstring_to_binary([]) ->
    <<>>;
hexstring_to_binary([H, L | Left] = List) when is_list(List) ->
    CurValue = erlang:list_to_integer([H, L], 16),
    LeftBinary = hexstring_to_binary(Left),
    <<CurValue:8, LeftBinary/binary>>;
hexstring_to_binary(_) ->
    <<>>.

-spec integer_to_hexstring(Byte :: integer()) ->
    string().
integer_to_hexstring(Byte) ->
    Str = erlang:integer_to_list(Byte, 16),
    case length(Str) of
        1 ->
            [$0 | Str];
        _ ->
            Str
    end.

-spec eval_string(S :: string()) ->
    any().
eval_string(S) ->
    {ok, Scanned, _} = erl_scan:string(S),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Value, _} = erl_eval:exprs(Parsed, []),
    Value.

-spec eval_string(S :: string(), Binds :: erl_eval:binding_struct()) ->
    any().
eval_string(S, Binds) ->
    {ok, Scanned, _} = erl_scan:string(S),
    {ok, Parsed} = erl_parse:parse_exprs(Scanned),
    {value, Value, _} = erl_eval:exprs(Parsed, Binds),
    Value.


-spec suffixed_string_len(S :: string(), C :: string(), L :: integer()) ->
    list().
suffixed_string_len(S, C, L) ->
    SL = length(S),
    if
        SL >= L ->
            S;
        true ->
            lists:append(S, lists:duplicate(L - SL, C))
    end.

-spec prefixed_string_len(S :: string(), C :: string(), L :: integer()) ->
    list().
prefixed_string_len(S, C, L) ->
    SL = length(S),
    if
        SL >= L ->
            S;
        true ->
            lists:append(lists:duplicate(L - SL, C), S)
    end.

-spec md5_string(Content :: iodata()) ->
    string().
md5_string(Content) ->
    agb_hex:bin_to_hexlstr(erlang:md5(Content)).

-spec md5_binary_str(Content :: iodata()) ->
    binary().
md5_binary_str(Content) ->
    agb_hex:bin_to_hexlbin(erlang:md5(Content)).

-spec sha256_binary_str(Content :: iodata()) ->
    binary().
sha256_binary_str(Content) ->
    agb_hex:bin_to_hexlbin(crypto:mac(sha256, [], Content)).


-spec decode_unicode_escape(List :: string()) ->
    list().
decode_unicode_escape([]) ->
    [];
decode_unicode_escape([$\\, U, A, B, C, D | Tail]) when U == $u orelse U == $U ->
    AC = uhex(A) bsl 12,
    BC = uhex(B) bsl 8,
    CC = uhex(C) bsl 4,
    DC = uhex(D),
    [AC bor BC bor CC bor DC | decode_unicode_escape(Tail)];
decode_unicode_escape([C | Tail]) ->
    [C | decode_unicode_escape(Tail)].

-spec encode_unicode_escape(List :: list()) ->
    string().
encode_unicode_escape([]) ->
    [];
encode_unicode_escape([Dig | Tail]) when Dig > 16#FF ->
    A = (Dig band 16#F000) bsr 12,
    B = (Dig band 16#0F00) bsr 8,
    C = (Dig band 16#00F0) bsr 4,
    D = (Dig band 16#000F),
    [$\\, $u, hex(A), hex(B), hex(C), hex(D) | encode_unicode_escape(Tail)];
encode_unicode_escape([C | Tail]) ->
    [C | encode_unicode_escape(Tail)].

uhex($0) ->
    0;
uhex($1) ->
    1;
uhex($2) ->
    2;
uhex($3) ->
    3;
uhex($4) ->
    4;
uhex($5) ->
    5;
uhex($6) ->
    6;
uhex($7) ->
    7;
uhex($8) ->
    8;
uhex($9) ->
    9;
uhex($A) ->
    10;
uhex($B) ->
    11;
uhex($C) ->
    12;
uhex($D) ->
    13;
uhex($E) ->
    14;
uhex($F) ->
    15;
uhex($a) ->
    10;
uhex($b) ->
    11;
uhex($c) ->
    12;
uhex($d) ->
    13;
uhex($e) ->
    14;
uhex($f) ->
    15.

hex(0) ->
    $0;
hex(1) ->
    $1;
hex(2) ->
    $2;
hex(3) ->
    $3;
hex(4) ->
    $4;
hex(5) ->
    $5;
hex(6) ->
    $6;
hex(7) ->
    $7;
hex(8) ->
    $8;
hex(9) ->
    $9;
hex(10) ->
    $A;
hex(11) ->
    $B;
hex(12) ->
    $C;
hex(13) ->
    $D;
hex(14) ->
    $E;
hex(15) ->
    $F.

-spec to_string(Value :: list()| integer() | float() | atom() | binary()) ->
    string().
to_string(Value) when is_list(Value) ->
    Value;
to_string(Value) when is_integer(Value) ->
    integer_to_list(Value);
to_string(Value) when is_float(Value) ->
    float_to_list(Value);
to_string(Value) when is_atom(Value) ->
    atom_to_list(Value);
to_string(Value) when is_binary(Value) ->
    binary_to_list(Value);
to_string(Value) when is_pid(Value) ->
    pid_to_list(Value);
to_string(_Value) ->
    throw("Error record value").
