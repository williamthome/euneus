-module(euneus_decoder).

-compile({inline, [
    continue/6, escapeu_1/8, escapeu_2/8, escapeu/6, terminate/6,
    empty_error/2, throw_error/2, throw_error/4, token_error/2,
    token_error/3, value/5, string/6, string/7, key/5, key/6,
    try_parse_float/3, normalize_string/3, parse_opts/1
]}).

-ifdef(EUNEUS_ENABLE_CALENDAR).
-compile({inline, [
    chars_to_integer/2, chars_to_integer/3, chars_to_integer/4
]}).
-endif.

-compile({inline_size, 100}).

-export([ decode/2 ]).

% We use integers instead of atoms to take advantage of the jump table optimization
-define(terminate, 0).
-define(array, 1).
-define(key, 2).
-define(object, 3).

-define(is_number(X), X >= $0, X =< $9).

decode(Data, Opts) when is_binary(Data) andalso is_map(Opts) ->
    try
        {ok, value(Data, parse_opts(Opts), Data, 0, [?terminate])}
    catch
        throw:{position, Position}:_ ->
            case Position == byte_size(Data) of
                true ->
                    {error, unexpected_end_of_input};
                false ->
                    Byte = binary:at(Data, Position),
                    Hex = integer_to_binary(Byte, 16),
                    {error, {unexpected_byte, <<"0x"/utf8,Hex/binary>>, Position}}
            end;
        throw:{token, Token, Position}:_ ->
            {error, {unexpected_sequence, Token, Position}}
    end.

parse_opts(Opts) ->
    Opts#{
        null_term => maps:get(null_term, Opts, undefined)
    }.

value(<<H/integer,Rest/bitstring>>, Opts, Input, Skip, Stack)
  when H =:= 32; H =:= 9; H =:= 10; H =:= 13 ->
    value(Rest, Opts, Input, Skip + 1, Stack);
value(<<34/integer,Rest/bitstring>>, Opts, Input, Skip, Stack) ->
    string(Rest, Opts, Input, Skip + 1, Stack, 0);
value(<<45/integer,Rest/bitstring>>, Opts, Input, Skip, Stack) ->
    number_minus(Rest, Opts, Input, Skip, Stack);
value(<<48/integer,Rest/bitstring>>, Opts, Input, Skip, Stack) ->
    number_zero(Rest, Opts, Input, Skip, Stack, 1);
value(<<H/integer,Rest/bitstring>>, Opts, Input, Skip, Stack)
  when H > 48, H < 58 ->
    number(Rest, Opts, Input, Skip, Stack, 1);
value(<<"true",Rest/bitstring>>, Opts, Input, Skip, Stack) ->
    continue(Rest, Opts, Input, Skip + 4, Stack, true);
value(<<"false",Rest/bitstring>>, Opts, Input, Skip, Stack) ->
    continue(Rest, Opts, Input, Skip + 5, Stack, false);
value(<<"null",Rest/bitstring>>, #{null_term := Null} = Opts, Input, Skip, Stack) ->
    continue(Rest, Opts, Input, Skip + 4, Stack, Null);
value(<<123/integer,Rest/bitstring>>, Opts, Input, Skip, Stack) ->
    key(Rest, Opts, Input, Skip + 1, [[] | Stack]);
value(<<91/integer,Rest/bitstring>>, Opts, Input, Skip, Stack) ->
    value(Rest, Opts, Input, Skip + 1, [?array, [] | Stack]);
value(<<93/integer,Rest/bitstring>>, Opts, Input, Skip, Stack) ->
    empty_array(Rest, Opts, Input, Skip + 1, Stack);
value(<<_/integer,Rest/bitstring>>, _Opts, Input, Skip, Stack) ->
    throw_error(Rest, Input, Skip + 1, Stack);
value(<<_/bitstring>>, _Opts, Input, Skip, _Stack) ->
    throw_error(Input, Skip).

-ifdef(EUNEUS_ENABLE_CALENDAR).

string(<<$"/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len) ->
    String = binary_part(Input, Skip, Len),
    Value = normalize_string(Stack, Opts, String),
    continue(Rest, Opts, Input, Skip + Len + 1, Stack, Value);
string(<<$\\/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len) ->
    Part = binary_part(Input, Skip, Len),
    escape(Rest, Opts, Input, Skip + Len, Stack, Part);
string(<<H/integer,_Rest/bitstring>>, _Opts, Input, Skip, _Stack, _Len)
  when H < 32 ->
    throw_error(Input, Skip);
string(<< Y4/integer, Y3/integer, Y2/integer, Y1/integer, $-/integer
        , M2/integer, M1/integer, $-/integer
        , D2/integer, D1/integer
        , $T/integer
        , H2/integer, H1/integer, $:/integer
        , Min2/integer, Min1/integer, $:/integer
        , S2/integer, S1/integer, $Z/integer, $"
        , Rest/bitstring >>, Opts, Input, Skip, Stack, 0)
  when ?is_number(Y4), ?is_number(Y3), ?is_number(Y2), ?is_number(Y1)
     , ?is_number(M2), ?is_number(M1)
     , ?is_number(D2), ?is_number(D1)
     , ?is_number(H2), ?is_number(H1)
     , ?is_number(Min2), ?is_number(Min1)
     , ?is_number(S2), ?is_number(S1) ->
    Date = {chars_to_integer(Y4, Y3, Y2, Y1), chars_to_integer(M2, M1), chars_to_integer(D2, D1)},
    Time = {chars_to_integer(H2, H1), chars_to_integer(Min2, Min1), chars_to_integer(S2, S1)},
    Value = {Date, Time},
    continue(Rest, Opts, Input, Skip + 21, Stack, Value);
string(<< Y4/integer, Y3/integer, Y2/integer, Y1/integer, $-/integer
        , M2/integer, M1/integer, $-/integer
        , D2/integer, D1/integer
        , $T/integer
        , H2/integer, H1/integer, $:/integer
        , Min2/integer, Min1/integer, $:/integer
        , S2/integer, S1/integer, $./integer
        , Mil3/integer, Mil2/integer, Mil1/integer, $Z/integer, $"
        , Rest/bitstring >>, Opts, Input, Skip, Stack, 0)
  when ?is_number(Y4), ?is_number(Y3), ?is_number(Y2), ?is_number(Y1)
     , ?is_number(M2), ?is_number(M1)
     , ?is_number(D2), ?is_number(D1)
     , ?is_number(H2), ?is_number(H1)
     , ?is_number(Min2), ?is_number(Min1)
     , ?is_number(S2), ?is_number(S1)
     , ?is_number(Mil3), ?is_number(Mil2), ?is_number(Mil1) ->
    Date = {chars_to_integer(Y4, Y3, Y2, Y1), chars_to_integer(M2, M1), chars_to_integer(D2, D1)},
    Time = {chars_to_integer(H2, H1), chars_to_integer(Min2, Min1), chars_to_integer(S2, S1)},
    DateTime = {Date, Time},
    MilliSeconds = chars_to_integer(Mil3, Mil2, Mil1),
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200,
    Value = {Seconds div 1000000, Seconds rem 1000000, MilliSeconds * 1000},
    continue(Rest, Opts, Input, Skip + 25, Stack, Value);
string(<<H/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when H < 128 ->
    string(Rest, Opts, Input, Skip, Stack, Len + 1);
string(<<H/utf8,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when H < 2048 ->
    string(Rest, Opts, Input, Skip, Stack, Len + 2);
string(<<H/utf8,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when H < 65536 ->
    string(Rest, Opts, Input, Skip, Stack, Len + 3);
string(<<_/utf8,Rest/bitstring>>, Opts, Input, Skip, Stack, Len) ->
    string(Rest, Opts, Input, Skip, Stack, Len + 4);
string(<<_/integer,_Rest/bitstring>>, _Opts, Input, Skip, _Stack, _Len) ->
    throw_error(Input, Skip);
string(<<_/bitstring>>, _Opts, Input, Skip, _Stack, Len) ->
    empty_error(Input, Skip + Len).

chars_to_integer(N2, N1) ->
    ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N3, N2, N1) ->
    ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

chars_to_integer(N4, N3, N2, N1) ->
    ((N4 - $0) * 1000) + ((N3 - $0) * 100) + ((N2 - $0) * 10) + (N1 - $0).

-else.

string(<<$"/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len) ->
    String = binary_part(Input, Skip, Len),
    Value = normalize_string(Stack, Opts, String),
    continue(Rest, Opts, Input, Skip + Len + 1, Stack, Value);
string(<<$\\/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len) ->
    Part = binary_part(Input, Skip, Len),
    escape(Rest, Opts, Input, Skip + Len, Stack, Part);
string(<<H/integer,_Rest/bitstring>>, _Opts, Input, Skip, _Stack, _Len)
  when H < 32 ->
    throw_error(Input, Skip);
string(<<H/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when H < 128 ->
    string(Rest, Opts, Input, Skip, Stack, Len + 1);
string(<<H/utf8,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when H < 2048 ->
    string(Rest, Opts, Input, Skip, Stack, Len + 2);
string(<<H/utf8,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when H < 65536 ->
    string(Rest, Opts, Input, Skip, Stack, Len + 3);
string(<<_/utf8,Rest/bitstring>>, Opts, Input, Skip, Stack, Len) ->
    string(Rest, Opts, Input, Skip, Stack, Len + 4);
string(<<_/integer,_Rest/bitstring>>, _Opts, Input, Skip, _Stack, _Len) ->
    throw_error(Input, Skip);
string(<<_/bitstring>>, _Opts, Input, Skip, _Stack, Len) ->
    empty_error(Input, Skip + Len).

-endif.

string(<<$"/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc, Len) ->
    Last = binary_part(Input, Skip, Len),
    String = iolist_to_binary([Acc | Last]),
    continue(Rest, Opts, Input, Skip + Len + 1, Stack, String);
string(<<$\\/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc, Len) ->
    Part = binary_part(Input, Skip, Len),
    escape(Rest, Opts, Input, Skip + Len, Stack, [Acc | Part]);
string(<<H/integer,_/bitstring>>, _Opts, Input, Skip, _Stack, _Acc, _Len)
  when H < 32 ->
    throw_error(Input, Skip);
string(<<H/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc, Len)
  when H < 128 ->
    string(Rest, Opts, Input, Skip, Stack, Acc, Len + 1);
string(<<H/utf8,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc, Len)
  when H < 2048 ->
    string(Rest, Opts, Input, Skip, Stack, Acc, Len + 2);
string(<<H/utf8,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc, Len)
  when H < 65536 ->
    string(Rest, Opts, Input, Skip, Stack, Acc, Len + 3);
string(<<_/utf8,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc, Len) ->
    string(Rest, Opts, Input, Skip, Stack, Acc, Len + 4);
string(<<_/integer,_/bitstring>>, _Opts, Input, Skip, _Stack, _Acc, _Len) ->
    throw_error(Input, Skip);
string(<<_/bitstring>>, _Opts, Input, Skip, _Stack, _Acc, Len) ->
    empty_error(Input, Skip + Len).

normalize_string([?key | _], #{handle_key := Handle}, String) ->
    Handle(String);
normalize_string([?key | _], _Opts, String) ->
    String;
normalize_string(_Stack, #{handle_string := Handle}, String) ->
    Handle(String);
normalize_string(_Stack, _Opts, String) ->
    String.

escape(<<$\"/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc) ->
    string(Rest, Opts, Input, Skip + 2, Stack, [Acc, $\"], 0);
escape(<<$//integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc) ->
    string(Rest, Opts, Input, Skip + 2, Stack, [Acc, $/], 0);
escape(<<$\\/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc) ->
    string(Rest, Opts, Input, Skip + 2, Stack, [Acc, $\\], 0);
escape(<<$b/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc) ->
    string(Rest, Opts, Input, Skip + 2, Stack, [Acc, $\b], 0);
escape(<<$f/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc) ->
    string(Rest, Opts, Input, Skip + 2, Stack, [Acc, $\f], 0);
escape(<<$n/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc) ->
    string(Rest, Opts, Input, Skip + 2, Stack, [Acc, $\n], 0);
escape(<<$r/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc) ->
    string(Rest, Opts, Input, Skip + 2, Stack, [Acc, $\r], 0);
escape(<<$t/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc) ->
    string(Rest, Opts, Input, Skip + 2, Stack, [Acc, $\t], 0);
escape(<<$u/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc) ->
    escapeu(Rest, Opts, Input, Skip, Stack, Acc);
escape(<<_/integer,_/bitstring>>, _Opts, Input, Skip, _Stack, _Acc) ->
    throw_error(Input, Skip + 1);
escape(<<_/bitstring>>, _Opts, Input, Skip, _Stack, _Acc) ->
    empty_error(Input, Skip).

escapeu(<<12336:16/integer,Int2:16/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc0) ->
    Last = escapeu_last(Int2, Input, Skip),
    _@1 = Acc0,
    _@2 = 0,
    _@3 = Last,
    Acc =  case _@3 < 128 of
        false ->
            _@4 = 6 bsl 5 + (_@2 bsl 2) + (_@3 bsr 6),
            _@5 = 2 bsl 6 + _@3 band 63,
            [_@1, _@4, _@5];
        true ->
            [_@1, _@3]
    end,
    string(Rest, Opts, Input, Skip + 6, Stack, Acc, 0);
escapeu(<<Int1:16/integer,Int2:16/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc)
  when Int1 > 12336, Int1 < 12344 ->
    X = case Int1 of
        12337 -> 1; 12338 -> 2; 12339 -> 3; 12340 -> 4; 12341 -> 5;
        12342 -> 6; 12343 -> 7;
        _ -> token_error(Input, Skip, 6)
    end,
    Last = escapeu_last(Int2, Input, Skip),
    escapeu_1(Rest, Opts, Input, Skip, Stack, Acc, Last, X);
escapeu(<<Int1:16/integer,Int2:16/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc)
  when (Int1 > 12343 andalso Int1 < 17464) orelse (Int1 > 17711 andalso Int1 < 25656) orelse (Int1 > 25903 andalso Int1 < 26215) ->
    X = case Int1 of
        12344 -> 8; 12345 -> 9; 12353 -> 10; 12354 -> 11; 12355 -> 12;
        12356 -> 13; 12357 -> 14; 12358 -> 15; 12385 -> 10; 12386 -> 11;
        12387 -> 12; 12388 -> 13; 12389 -> 14; 12390 -> 15; 12592 -> 16;
        12593 -> 17; 12594 -> 18; 12595 -> 19; 12596 -> 20; 12597 -> 21;
        12598 -> 22; 12599 -> 23; 12600 -> 24; 12601 -> 25; 12609 -> 26;
        12610 -> 27; 12611 -> 28; 12612 -> 29; 12613 -> 30; 12614 -> 31;
        12641 -> 26; 12642 -> 27; 12643 -> 28; 12644 -> 29; 12645 -> 30;
        12646 -> 31; 12848 -> 32; 12849 -> 33; 12850 -> 34; 12851 -> 35;
        12852 -> 36; 12853 -> 37; 12854 -> 38; 12855 -> 39; 12856 -> 40;
        12857 -> 41; 12865 -> 42; 12866 -> 43; 12867 -> 44; 12868 -> 45;
        12869 -> 46; 12870 -> 47; 12897 -> 42; 12898 -> 43; 12899 -> 44;
        12900 -> 45; 12901 -> 46; 12902 -> 47; 13104 -> 48; 13105 -> 49;
        13106 -> 50; 13107 -> 51; 13108 -> 52; 13109 -> 53; 13110 -> 54;
        13111 -> 55; 13112 -> 56; 13113 -> 57; 13121 -> 58; 13122 -> 59;
        13123 -> 60; 13124 -> 61; 13125 -> 62; 13126 -> 63; 13153 -> 58;
        13154 -> 59; 13155 -> 60; 13156 -> 61; 13157 -> 62; 13158 -> 63;
        13360 -> 64; 13361 -> 65; 13362 -> 66; 13363 -> 67; 13364 -> 68;
        13365 -> 69; 13366 -> 70; 13367 -> 71; 13368 -> 72; 13369 -> 73;
        13377 -> 74; 13378 -> 75; 13379 -> 76; 13380 -> 77; 13381 -> 78;
        13382 -> 79; 13409 -> 74; 13410 -> 75; 13411 -> 76; 13412 -> 77;
        13413 -> 78; 13414 -> 79; 13616 -> 80; 13617 -> 81; 13618 -> 82;
        13619 -> 83; 13620 -> 84; 13621 -> 85; 13622 -> 86; 13623 -> 87;
        13624 -> 88; 13625 -> 89; 13633 -> 90; 13634 -> 91; 13635 -> 92;
        13636 -> 93; 13637 -> 94; 13638 -> 95; 13665 -> 90; 13666 -> 91;
        13667 -> 92; 13668 -> 93; 13669 -> 94; 13670 -> 95; 13872 -> 96;
        13873 -> 97; 13874 -> 98; 13875 -> 99; 13876 -> 100; 13877 -> 101;
        13878 -> 102; 13879 -> 103; 13880 -> 104; 13881 -> 105; 13889 -> 106;
        13890 -> 107; 13891 -> 108; 13892 -> 109; 13893 -> 110; 13894 -> 111;
        13921 -> 106; 13922 -> 107; 13923 -> 108; 13924 -> 109; 13925 -> 110;
        13926 -> 111; 14128 -> 112; 14129 -> 113; 14130 -> 114; 14131 -> 115;
        14132 -> 116; 14133 -> 117; 14134 -> 118; 14135 -> 119; 14136 -> 120;
        14137 -> 121; 14145 -> 122; 14146 -> 123; 14147 -> 124; 14148 -> 125;
        14149 -> 126; 14150 -> 127; 14177 -> 122; 14178 -> 123; 14179 -> 124;
        14180 -> 125; 14181 -> 126; 14182 -> 127; 14384 -> 128; 14385 -> 129;
        14386 -> 130; 14387 -> 131; 14388 -> 132; 14389 -> 133; 14390 -> 134;
        14391 -> 135; 14392 -> 136; 14393 -> 137; 14401 -> 138; 14402 -> 139;
        14403 -> 140; 14404 -> 141; 14405 -> 142; 14406 -> 143; 14433 -> 138;
        14434 -> 139; 14435 -> 140; 14436 -> 141; 14437 -> 142; 14438 -> 143;
        14640 -> 144; 14641 -> 145; 14642 -> 146; 14643 -> 147; 14644 -> 148;
        14645 -> 149; 14646 -> 150; 14647 -> 151; 14648 -> 152; 14649 -> 153;
        14657 -> 154; 14658 -> 155; 14659 -> 156; 14660 -> 157; 14661 -> 158;
        14662 -> 159; 14689 -> 154; 14690 -> 155; 14691 -> 156; 14692 -> 157;
        14693 -> 158; 14694 -> 159; 16688 -> 160; 16689 -> 161; 16690 -> 162;
        16691 -> 163; 16692 -> 164; 16693 -> 165; 16694 -> 166; 16695 -> 167;
        16696 -> 168; 16697 -> 169; 16705 -> 170; 16706 -> 171; 16707 -> 172;
        16708 -> 173; 16709 -> 174; 16710 -> 175; 16737 -> 170; 16738 -> 171;
        16739 -> 172; 16740 -> 173; 16741 -> 174; 16742 -> 175; 16944 -> 176;
        16945 -> 177; 16946 -> 178; 16947 -> 179; 16948 -> 180; 16949 -> 181;
        16950 -> 182; 16951 -> 183; 16952 -> 184; 16953 -> 185; 16961 -> 186;
        16962 -> 187; 16963 -> 188; 16964 -> 189; 16965 -> 190; 16966 -> 191;
        16993 -> 186; 16994 -> 187; 16995 -> 188; 16996 -> 189; 16997 -> 190;
        16998 -> 191; 17200 -> 192; 17201 -> 193; 17202 -> 194; 17203 -> 195;
        17204 -> 196; 17205 -> 197; 17206 -> 198; 17207 -> 199; 17208 -> 200;
        17209 -> 201; 17217 -> 202; 17218 -> 203; 17219 -> 204; 17220 -> 205;
        17221 -> 206; 17222 -> 207; 17249 -> 202; 17250 -> 203; 17251 -> 204;
        17252 -> 205; 17253 -> 206; 17254 -> 207; 17456 -> 208; 17457 -> 209;
        17458 -> 210; 17459 -> 211; 17460 -> 212; 17461 -> 213; 17462 -> 214;
        17463 -> 215; 17712 -> 224; 17713 -> 225; 17714 -> 226; 17715 -> 227;
        17716 -> 228; 17717 -> 229; 17718 -> 230; 17719 -> 231; 17720 -> 232;
        17721 -> 233; 17729 -> 234; 17730 -> 235; 17731 -> 236; 17732 -> 237;
        17733 -> 238; 17734 -> 239; 17761 -> 234; 17762 -> 235; 17763 -> 236;
        17764 -> 237; 17765 -> 238; 17766 -> 239; 17968 -> 240; 17969 -> 241;
        17970 -> 242; 17971 -> 243; 17972 -> 244; 17973 -> 245; 17974 -> 246;
        17975 -> 247; 17976 -> 248; 17977 -> 249; 17985 -> 250; 17986 -> 251;
        17987 -> 252; 17988 -> 253; 17989 -> 254; 17990 -> 255; 18017 -> 250;
        18018 -> 251; 18019 -> 252; 18020 -> 253; 18021 -> 254; 18022 -> 255;
        24880 -> 160; 24881 -> 161; 24882 -> 162; 24883 -> 163; 24884 -> 164;
        24885 -> 165; 24886 -> 166; 24887 -> 167; 24888 -> 168; 24889 -> 169;
        24897 -> 170; 24898 -> 171; 24899 -> 172; 24900 -> 173; 24901 -> 174;
        24902 -> 175; 24929 -> 170; 24930 -> 171; 24931 -> 172; 24932 -> 173;
        24933 -> 174; 24934 -> 175; 25136 -> 176; 25137 -> 177; 25138 -> 178;
        25139 -> 179; 25140 -> 180; 25141 -> 181; 25142 -> 182; 25143 -> 183;
        25144 -> 184; 25145 -> 185; 25153 -> 186; 25154 -> 187; 25155 -> 188;
        25156 -> 189; 25157 -> 190; 25158 -> 191; 25185 -> 186; 25186 -> 187;
        25187 -> 188; 25188 -> 189; 25189 -> 190; 25190 -> 191; 25392 -> 192;
        25393 -> 193; 25394 -> 194; 25395 -> 195; 25396 -> 196; 25397 -> 197;
        25398 -> 198; 25399 -> 199; 25400 -> 200; 25401 -> 201; 25409 -> 202;
        25410 -> 203; 25411 -> 204; 25412 -> 205; 25413 -> 206; 25414 -> 207;
        25441 -> 202; 25442 -> 203; 25443 -> 204; 25444 -> 205; 25445 -> 206;
        25446 -> 207; 25648 -> 208; 25649 -> 209; 25650 -> 210; 25651 -> 211;
        25652 -> 212; 25653 -> 213; 25654 -> 214; 25655 -> 215; 25904 -> 224;
        25905 -> 225; 25906 -> 226; 25907 -> 227; 25908 -> 228; 25909 -> 229;
        25910 -> 230; 25911 -> 231; 25912 -> 232; 25913 -> 233; 25921 -> 234;
        25922 -> 235; 25923 -> 236; 25924 -> 237; 25925 -> 238; 25926 -> 239;
        25953 -> 234; 25954 -> 235; 25955 -> 236; 25956 -> 237; 25957 -> 238;
        25958 -> 239; 26160 -> 240; 26161 -> 241; 26162 -> 242; 26163 -> 243;
        26164 -> 244; 26165 -> 245; 26166 -> 246; 26167 -> 247; 26168 -> 248;
        26169 -> 249; 26177 -> 250; 26178 -> 251; 26179 -> 252; 26180 -> 253;
        26181 -> 254; 26182 -> 255; 26209 -> 250; 26210 -> 251; 26211 -> 252;
        26212 -> 253; 26213 -> 254; 26214 -> 255;
        _ -> token_error(Input, Skip, 6)
    end,
    Last = escapeu_last(Int2, Input, Skip),
    escapeu_2(Rest, Opts, Input, Skip, Stack, Acc, Last, X);
escapeu(<<Int1:16/integer,Int2:16/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Acc) ->
    X = case Int1 of
        17464 -> 216; 17465 -> 217; 17473 -> 218; 17474 -> 219; 17505 -> 218;
        17506 -> 219; 25656 -> 216; 25657 -> 217; 25665 -> 218; 25666 -> 219;
        25697 -> 218; 25698 -> 219;
        _ -> token_error(Input, Skip, 6)
    end,
    Last = escapeu_last(Int2, Input, Skip),
    Hi = 65536 + (X band 3 bsl 8 + Last bsl 10),
    escape_surrogate(Rest, Opts, Input, Skip, Stack, Acc, Hi);
escapeu(<<_Rest/bitstring>>, _Opts, Input, Skip, _Stack, _Acc) ->
    empty_error(Input, Skip).

escapeu_last(Int, Input, Skip) ->
    case Int of
        12336 -> 0; 12337 -> 1; 12338 -> 2; 12339 -> 3; 12340 -> 4; 12341 -> 5; 12342 -> 6;
        12343 -> 7; 12344 -> 8; 12345 -> 9; 12353 -> 10; 12354 -> 11; 12355 -> 12;
        12356 -> 13; 12357 -> 14; 12358 -> 15; 12385 -> 10; 12386 -> 11; 12387 -> 12;
        12388 -> 13; 12389 -> 14; 12390 -> 15; 12592 -> 16; 12593 -> 17; 12594 -> 18;
        12595 -> 19; 12596 -> 20; 12597 -> 21; 12598 -> 22; 12599 -> 23; 12600 -> 24;
        12601 -> 25; 12609 -> 26; 12610 -> 27; 12611 -> 28; 12612 -> 29; 12613 -> 30;
        12614 -> 31; 12641 -> 26; 12642 -> 27; 12643 -> 28; 12644 -> 29; 12645 -> 30;
        12646 -> 31; 12848 -> 32; 12849 -> 33; 12850 -> 34; 12851 -> 35; 12852 -> 36;
        12853 -> 37; 12854 -> 38; 12855 -> 39; 12856 -> 40; 12857 -> 41; 12865 -> 42;
        12866 -> 43; 12867 -> 44; 12868 -> 45; 12869 -> 46; 12870 -> 47; 12897 -> 42;
        12898 -> 43; 12899 -> 44; 12900 -> 45; 12901 -> 46; 12902 -> 47; 13104 -> 48;
        13105 -> 49; 13106 -> 50; 13107 -> 51; 13108 -> 52; 13109 -> 53; 13110 -> 54;
        13111 -> 55; 13112 -> 56; 13113 -> 57; 13121 -> 58; 13122 -> 59; 13123 -> 60;
        13124 -> 61; 13125 -> 62; 13126 -> 63; 13153 -> 58; 13154 -> 59; 13155 -> 60;
        13156 -> 61; 13157 -> 62; 13158 -> 63; 13360 -> 64; 13361 -> 65; 13362 -> 66;
        13363 -> 67; 13364 -> 68; 13365 -> 69; 13366 -> 70; 13367 -> 71; 13368 -> 72;
        13369 -> 73; 13377 -> 74; 13378 -> 75; 13379 -> 76; 13380 -> 77; 13381 -> 78;
        13382 -> 79; 13409 -> 74; 13410 -> 75; 13411 -> 76; 13412 -> 77; 13413 -> 78;
        13414 -> 79; 13616 -> 80; 13617 -> 81; 13618 -> 82; 13619 -> 83; 13620 -> 84;
        13621 -> 85; 13622 -> 86; 13623 -> 87; 13624 -> 88; 13625 -> 89; 13633 -> 90;
        13634 -> 91; 13635 -> 92; 13636 -> 93; 13637 -> 94; 13638 -> 95; 13665 -> 90;
        13666 -> 91; 13667 -> 92; 13668 -> 93; 13669 -> 94; 13670 -> 95; 13872 -> 96;
        13873 -> 97; 13874 -> 98; 13875 -> 99; 13876 -> 100; 13877 -> 101; 13878 -> 102;
        13879 -> 103; 13880 -> 104; 13881 -> 105; 13889 -> 106; 13890 -> 107; 13891 -> 108;
        13892 -> 109; 13893 -> 110; 13894 -> 111; 13921 -> 106; 13922 -> 107; 13923 -> 108;
        13924 -> 109; 13925 -> 110; 13926 -> 111; 14128 -> 112; 14129 -> 113; 14130 -> 114;
        14131 -> 115; 14132 -> 116; 14133 -> 117; 14134 -> 118; 14135 -> 119; 14136 -> 120;
        14137 -> 121; 14145 -> 122; 14146 -> 123; 14147 -> 124; 14148 -> 125; 14149 -> 126;
        14150 -> 127; 14177 -> 122; 14178 -> 123; 14179 -> 124; 14180 -> 125; 14181 -> 126;
        14182 -> 127; 14384 -> 128; 14385 -> 129; 14386 -> 130; 14387 -> 131; 14388 -> 132;
        14389 -> 133; 14390 -> 134; 14391 -> 135; 14392 -> 136; 14393 -> 137; 14401 -> 138;
        14402 -> 139; 14403 -> 140; 14404 -> 141; 14405 -> 142; 14406 -> 143; 14433 -> 138;
        14434 -> 139; 14435 -> 140; 14436 -> 141; 14437 -> 142; 14438 -> 143; 14640 -> 144;
        14641 -> 145; 14642 -> 146; 14643 -> 147; 14644 -> 148; 14645 -> 149; 14646 -> 150;
        14647 -> 151; 14648 -> 152; 14649 -> 153; 14657 -> 154; 14658 -> 155; 14659 -> 156;
        14660 -> 157; 14661 -> 158; 14662 -> 159; 14689 -> 154; 14690 -> 155; 14691 -> 156;
        14692 -> 157; 14693 -> 158; 14694 -> 159; 16688 -> 160; 16689 -> 161; 16690 -> 162;
        16691 -> 163; 16692 -> 164; 16693 -> 165; 16694 -> 166; 16695 -> 167; 16696 -> 168;
        16697 -> 169; 16705 -> 170; 16706 -> 171; 16707 -> 172; 16708 -> 173; 16709 -> 174;
        16710 -> 175; 16737 -> 170; 16738 -> 171; 16739 -> 172; 16740 -> 173; 16741 -> 174;
        16742 -> 175; 16944 -> 176; 16945 -> 177; 16946 -> 178; 16947 -> 179; 16948 -> 180;
        16949 -> 181; 16950 -> 182; 16951 -> 183; 16952 -> 184; 16953 -> 185; 16961 -> 186;
        16962 -> 187; 16963 -> 188; 16964 -> 189; 16965 -> 190; 16966 -> 191; 16993 -> 186;
        16994 -> 187; 16995 -> 188; 16996 -> 189; 16997 -> 190; 16998 -> 191; 17200 -> 192;
        17201 -> 193; 17202 -> 194; 17203 -> 195; 17204 -> 196; 17205 -> 197; 17206 -> 198;
        17207 -> 199; 17208 -> 200; 17209 -> 201; 17217 -> 202; 17218 -> 203; 17219 -> 204;
        17220 -> 205; 17221 -> 206; 17222 -> 207; 17249 -> 202; 17250 -> 203; 17251 -> 204;
        17252 -> 205; 17253 -> 206; 17254 -> 207; 17456 -> 208; 17457 -> 209; 17458 -> 210;
        17459 -> 211; 17460 -> 212; 17461 -> 213; 17462 -> 214; 17463 -> 215; 17464 -> 216;
        17465 -> 217; 17473 -> 218; 17474 -> 219; 17475 -> 220; 17476 -> 221; 17477 -> 222;
        17478 -> 223; 17505 -> 218; 17506 -> 219; 17507 -> 220; 17508 -> 221; 17509 -> 222;
        17510 -> 223; 17712 -> 224; 17713 -> 225; 17714 -> 226; 17715 -> 227; 17716 -> 228;
        17717 -> 229; 17718 -> 230; 17719 -> 231; 17720 -> 232; 17721 -> 233; 17729 -> 234;
        17730 -> 235; 17731 -> 236; 17732 -> 237; 17733 -> 238; 17734 -> 239; 17761 -> 234;
        17762 -> 235; 17763 -> 236; 17764 -> 237; 17765 -> 238; 17766 -> 239; 17968 -> 240;
        17969 -> 241; 17970 -> 242; 17971 -> 243; 17972 -> 244; 17973 -> 245; 17974 -> 246;
        17975 -> 247; 17976 -> 248; 17977 -> 249; 17985 -> 250; 17986 -> 251; 17987 -> 252;
        17988 -> 253; 17989 -> 254; 17990 -> 255; 18017 -> 250; 18018 -> 251; 18019 -> 252;
        18020 -> 253; 18021 -> 254; 18022 -> 255; 24880 -> 160; 24881 -> 161; 24882 -> 162;
        24883 -> 163; 24884 -> 164; 24885 -> 165; 24886 -> 166; 24887 -> 167; 24888 -> 168;
        24889 -> 169; 24897 -> 170; 24898 -> 171; 24899 -> 172; 24900 -> 173; 24901 -> 174;
        24902 -> 175; 24929 -> 170; 24930 -> 171; 24931 -> 172; 24932 -> 173; 24933 -> 174;
        24934 -> 175; 25136 -> 176; 25137 -> 177; 25138 -> 178; 25139 -> 179; 25140 -> 180;
        25141 -> 181; 25142 -> 182; 25143 -> 183; 25144 -> 184; 25145 -> 185; 25153 -> 186;
        25154 -> 187; 25155 -> 188; 25156 -> 189; 25157 -> 190; 25158 -> 191; 25185 -> 186;
        25186 -> 187; 25187 -> 188; 25188 -> 189; 25189 -> 190; 25190 -> 191; 25392 -> 192;
        25393 -> 193; 25394 -> 194; 25395 -> 195; 25396 -> 196; 25397 -> 197; 25398 -> 198;
        25399 -> 199; 25400 -> 200; 25401 -> 201; 25409 -> 202; 25410 -> 203; 25411 -> 204;
        25412 -> 205; 25413 -> 206; 25414 -> 207; 25441 -> 202; 25442 -> 203; 25443 -> 204;
        25444 -> 205; 25445 -> 206; 25446 -> 207; 25648 -> 208; 25649 -> 209; 25650 -> 210;
        25651 -> 211; 25652 -> 212; 25653 -> 213; 25654 -> 214; 25655 -> 215; 25656 -> 216;
        25657 -> 217; 25665 -> 218; 25666 -> 219; 25667 -> 220; 25668 -> 221; 25669 -> 222;
        25670 -> 223; 25697 -> 218; 25698 -> 219; 25699 -> 220; 25700 -> 221; 25701 -> 222;
        25702 -> 223; 25904 -> 224; 25905 -> 225; 25906 -> 226; 25907 -> 227; 25908 -> 228;
        25909 -> 229; 25910 -> 230; 25911 -> 231; 25912 -> 232; 25913 -> 233; 25921 -> 234;
        25922 -> 235; 25923 -> 236; 25924 -> 237; 25925 -> 238; 25926 -> 239; 25953 -> 234;
        25954 -> 235; 25955 -> 236; 25956 -> 237; 25957 -> 238; 25958 -> 239; 26160 -> 240;
        26161 -> 241; 26162 -> 242; 26163 -> 243; 26164 -> 244; 26165 -> 245; 26166 -> 246;
        26167 -> 247; 26168 -> 248; 26169 -> 249; 26177 -> 250; 26178 -> 251; 26179 -> 252;
        26180 -> 253; 26181 -> 254; 26182 -> 255; 26209 -> 250; 26210 -> 251; 26211 -> 252;
        26212 -> 253; 26213 -> 254; 26214 -> 255;
        _ -> token_error(Input, Skip, 6)
    end.

escape_surrogate(<<92/integer, 117/integer, Int1:16/integer, Int2:16/integer, Rest/bitstring>>,
                 Opts, Input, Skip, Stack, Acc0, Hi) ->
    Last = escapeu_last(Int2, Input, Skip + 6),
    X = case Int1 of
        17475 -> 220; 17476 -> 221; 17477 -> 222; 17478 -> 223; 17507 -> 220;
        17508 -> 221; 17509 -> 222; 17510 -> 223; 25667 -> 220; 25668 -> 221;
        25669 -> 222; 25670 -> 223; 25699 -> 220; 25700 -> 221; 25701 -> 222;
        25702 -> 223;
        _ -> token_error(Input, Skip, 12)
    end,
    Y = X band 3 bsl 8 + Last,
    Acc = [Acc0 | <<(Hi + Y)/utf8>>],
    string(Rest, Opts, Input, Skip + 12, Stack, Acc, 0);
escape_surrogate(<<_Rest/bitstring>>, _Opts, Input, Skip, _Stack, _Acc, _Hi) ->
    throw_error(Input, Skip + 6).

escapeu_1(<<_/bitstring>> = Rest, Opts, Input, Skip, Stack, Acc, Last, X) ->
    A = 6 bsl 5 + (X bsl 2) + (Last bsr 6),
    B = 2 bsl 6 + Last band 63,
    C = [Acc, A, B],
    string(Rest, Opts, Input, Skip + 6, Stack, C, 0).

escapeu_2(<<_/bitstring>> = Rest, Opts, Input, Skip, Stack, Acc, Last, X) ->
    A = 14 bsl 4 + (X bsr 4),
    B = 2 bsl 6 + (X band 15 bsl 2) + (Last bsr 6),
    C = 2 bsl 6 + Last band 63,
    D = [Acc, A, B, C],
    string(Rest, Opts, Input, Skip + 6, Stack, D, 0).

key(<<H/integer,Rest/bitstring>>, Opts, Input, Skip, Stack)
  when H =:= $\s; H =:= $\t; H =:= $\n; H =:= $\r ->
    key(Rest, Opts, Input, Skip + 1, Stack);
key(<<$"/integer,Rest/bitstring>>, Opts, Input, Skip, Stack) ->
    string(Rest, Opts, Input, Skip + 1, [?key | Stack], 0);
key(<<$}/integer,Rest/bitstring>>, Opts, Input, Skip, [[] | Stack]) ->
    continue(Rest, Opts, Input, Skip + 1, Stack, #{});
key(<<_/integer,_/bitstring>>, _Opts, Input, Skip, _Stack) ->
    throw_error(Input, Skip);
key(<<_/bitstring>>, _Opts, Input, Skip, _Stack) ->
    empty_error(Input, Skip).

key(<<H/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Value)
  when H =:= $\s; H =:= $\t; H =:= $\n; H =:= $\r ->
    key(Rest, Opts, Input, Skip + 1, Stack, Value);
key(<<$:/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Value) ->
    value(Rest, Opts, Input, Skip + 1, [?object, Value | Stack]);
key(<<_/integer,_/bitstring>>, _Opts, Input, Skip, _Stack, _Value) ->
    throw_error(Input, Skip);
key(<<_/bitstring>>, _Opts, Input, Skip, _Stack, _Value) ->
    empty_error(Input, Skip).

number(<<Byte/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when Byte >= $0 andalso Byte =< $9 ->
    number(Rest, Opts, Input, Skip, Stack, Len + 1);
number(<<$./integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len) ->
    number_frac(Rest, Opts, Input, Skip, Stack, Len + 1);
number(<<E/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when E =:= $e orelse E =:= $E ->
    Prefix = binary_part(Input, Skip, Len),
    number_exp_copy(Rest, Opts, Input, Skip + Len + 1, Stack, Prefix);
number(<<Rest/bitstring>>, Opts, Input, Skip, Stack, Len) ->
    Int = binary_to_integer(binary_part(Input, Skip, Len)),
    continue(Rest, Opts, Input, Skip + Len, Stack, Int).

number_exp(<<Byte/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when Byte >= $0 andalso Byte =< $9 ->
    number_exp_cont(Rest, Opts, Input, Skip, Stack, Len + 1);
number_exp(<<Byte/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when Byte =:= $+ orelse Byte =:= $- ->
    number_exp_sign(Rest, Opts, Input, Skip, Stack, Len + 1);
number_exp(<<_Rest/bitstring>>, _Opts, Input, Skip, _Stack, Len) ->
    throw_error(Input, Skip + Len).

number_exp_cont(<<Byte/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when Byte >= $0 andalso Byte =< $9 ->
    number_exp_cont(Rest, Opts, Input, Skip, Stack, Len + 1);
number_exp_cont(<<Rest/bitstring>>, Opts, Input, Skip, Stack, Len) ->
    Token = binary_part(Input, Skip, Len),
    Float = try_parse_float(Token, Token, Skip),
    continue(Rest, Opts, Input, Skip + Len, Stack, Float).

number_exp_cont(<<Byte/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Prefix, Len)
  when Byte >= $0 andalso Byte =< $9 ->
    number_exp_cont(Rest, Opts, Input, Skip, Stack, Prefix, Len + 1);
number_exp_cont(<<Rest/bitstring>>, Opts, Input, Skip, Stack, Prefix, Len) ->
    Suffix = binary_part(Input, Skip, Len),
    String = <<Prefix/binary,".0e",Suffix/binary>>,
    PrefixSize = byte_size(Prefix),
    InitialSkip = Skip - PrefixSize - 1,
    FinalSkip = Skip + Len,
    Token = binary_part(Input, InitialSkip, PrefixSize + Len + 1),
    Float = try_parse_float(String, Token, InitialSkip),
    continue(Rest, Opts, Input, FinalSkip, Stack, Float).

number_exp_copy(<<Byte/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Prefix)
  when Byte >= $0 andalso Byte =< $9 ->
    number_exp_cont(Rest, Opts, Input, Skip, Stack, Prefix, 1);
number_exp_copy(<<Byte/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Prefix)
  when Byte =:= $+ orelse Byte =:= $- ->
    number_exp_sign(Rest, Opts, Input, Skip, Stack, Prefix, 1);
number_exp_copy(<<_Rest/bitstring>>, _Opts, Input, Skip, _Stack, _Prefix) ->
    throw_error(Input, Skip).

number_exp_sign(<<Byte/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when Byte >= $0 andalso Byte =< $9 ->
    number_exp_cont(Rest, Opts, Input, Skip, Stack, Len + 1);
number_exp_sign(<<_Rest/bitstring>>, _Opts, Input, Skip, _Stack, Len) ->
    throw_error(Input, Skip + Len).

number_exp_sign(<<Byte/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Prefix, Len)
  when Byte >= $0 andalso Byte =< $9 ->
    number_exp_cont(Rest, Opts, Input, Skip, Stack, Prefix, Len + 1);
number_exp_sign(<<_Rest/bitstring>>, _Opts, Input, Skip, _Stack, _Prefix, Len) ->
    throw_error(Input, Skip + Len).

number_frac(<<Byte/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when Byte >= $0 andalso Byte =< $9 ->
    number_frac_cont(Rest, Opts, Input, Skip, Stack, Len + 1);
number_frac(<<_Rest/bitstring>>, _Opts, Input, Skip, _Stack, Len) ->
    throw_error(Input, Skip + Len).

number_frac_cont(<<Byte/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when Byte >= $0 andalso Byte =< $9 ->
    number_frac_cont(Rest, Opts, Input, Skip, Stack, Len + 1);
number_frac_cont(<<E/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when E =:= $e orelse E =:= $E ->
    number_exp(Rest, Opts, Input, Skip, Stack, Len + 1);
number_frac_cont(<<Rest/bitstring>>, Opts, Input, Skip, Stack, Len) ->
    Token = binary_part(Input, Skip, Len),
    Float = try_parse_float(Token, Token, Skip),
    continue(Rest, Opts, Input, Skip + Len, Stack, Float).

number_minus(<<48/integer,Rest/bitstring>>, Opts, Input, Skip, Stack) ->
    number_zero(Rest, Opts, Input, Skip, Stack, 2);
number_minus(<<Byte/integer,Rest/bitstring>>, Opts, Input, Skip, Stack)
  when Byte >= $0 andalso Byte =< $9 ->
    number(Rest, Opts, Input, Skip, Stack, 2);
number_minus(<<_Rest/bitstring>>, _Opts, Input, Skip, _Stack) ->
    throw_error(Input, Skip + 1).

number_zero(<<46/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len) ->
    number_frac(Rest, Opts, Input, Skip, Stack, Len + 1);
number_zero(<<E/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Len)
  when E =:= $e orelse E =:= $E ->
    number_exp_copy(Rest, Opts, Input, Skip + Len + 1, Stack, <<"0">>);
number_zero(<<Rest/bitstring>>, Opts, Input, Skip, Stack, Len) ->
    continue(Rest, Opts, Input, Skip + Len, Stack, 0).

object(<<H/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Value)
  when H =:= 32; H =:= 9; H =:= 10; H =:= 13 ->
    object(Rest, Opts, Input, Skip + 1, Stack, Value);
object(<<44/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Value) ->
    Skip2 = Skip + 1,
    [Key, Acc | Stack2] = Stack,
    Acc2 = [{Key, Value} | Acc],
    key(Rest, Opts, Input, Skip2, [Acc2 | Stack2]);
object(<<125/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Value) ->
    Skip2 = Skip + 1,
    [Key, Acc2 | Stack2] = Stack,
    Final = [{Key, Value} | Acc2],
    continue(Rest, Opts, Input, Skip2, Stack2, maps:from_list(Final));
object(<<_/integer,_/bitstring>>, _Opts, Input, Skip, _Stack, _Value) ->
    throw_error(Input, Skip);
object(<<_/bitstring>>, _Opts, Input, Skip, _Stack, _Value) ->
    empty_error(Input, Skip).

array(<<H/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Value)
  when H =:= $\s; H =:= $\t; H =:= $\n; H =:= $\r ->
    array(Rest, Opts, Input, Skip + 1, Stack, Value);
array(<<$,/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Value) ->
    [Acc | Stack2] = Stack,
    value(Rest, Opts, Input, Skip + 1, [?array, [Value | Acc] | Stack2]);
array(<<$]/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Value) ->
    [Acc | Stack2] = Stack,
    Value2 = lists:reverse(Acc, [Value]),
    continue(Rest, Opts, Input, Skip + 1, Stack2, Value2);
array(<<_/integer,_/bitstring>>, _Opts, Input, Skip, _Stack, _Value) ->
    throw_error(Input, Skip);
array(<<_/bitstring>>, _Opts, Input, Skip, _Stack, _Value) ->
    empty_error(Input, Skip).

empty_array(<<Rest/bitstring>>, Opts, Input, Skip, [?array, [] | Stack]) ->
    continue(Rest, Opts, Input, Skip, Stack, []);
empty_array(<<_/integer,_/bitstring>>, _Opts, Input, Skip, _Stack) ->
    throw_error(Input, Skip - 1);
empty_array(<<_/bitstring>>, _Opts, Input, Skip, _Stack) ->
    empty_error(Input, Skip).

continue(Rest, Opts, Input, Skip, [?key | Stack], Value) ->
    key(Rest, Opts, Input, Skip, Stack, Value);
continue(Rest, Opts, Input, Skip, [?object | Stack], Value) ->
    object(Rest, Opts, Input, Skip, Stack, Value);
continue(Rest, Opts, Input, Skip, [?array | Stack], Value) ->
    array(Rest, Opts, Input, Skip, Stack, Value);
continue(Rest, Opts, Input, Skip, [?terminate | Stack], Value) ->
    terminate(Rest, Opts, Input, Skip, Stack, Value).

terminate(<<H/integer,Rest/bitstring>>, Opts, Input, Skip, Stack, Value)
  when H =:= 32; H =:= 13; H =:= 10; H =:= 9 ->
    terminate(Rest, Opts, Input, Skip + 1, Stack, Value);
terminate(<<>>, _Opts, _Input, _Skip, _Stack, Value) ->
    Value;
terminate(<<_Rest/bitstring>>, _Opts, Input, Skip, _Stack, _Value) ->
    throw_error(Input, Skip).

empty_error(_Input, Skip) ->
    throw({position, Skip}).

throw_error(_Input, Skip) ->
    throw({position, Skip}).

throw_error(<<_Rest/bitstring>>, _Input, Skip, _Stack) ->
    throw({position, Skip - 1}).

token_error(Token, Position) ->
    throw({token, Token, Position}).

token_error(Token, Position, Len) ->
    throw({token, binary_part(Token, Position, Len), Position}).

try_parse_float(Bin, Token, Skip) ->
    try
        binary_to_float(Bin)
    catch
        error:badarg:_ ->
            token_error(Token, Skip)
    end.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_test() ->
    [ ?assertEqual({ok, Expect}, decode(Input, #{}))
      || {Expect, Input} <- [
        {<<"foo">>, <<"\"foo\"">>},
        {123, <<"123">>},
        {1.234, <<"1.234">>},
        {6.02e23, <<"6.02e+23">>},
        {[<<"foo">>, 123], <<"[\"foo\",123]">>},
        {#{<<"foo">> => 123}, <<"{\"foo\":123}">>},
        {undefined, <<"null">>},
        {<<"ABC">>, <<"\"\\u0041\\u0042\\u0043\"">>},
        {{{1970,1,1},{0,0,0}}, <<"\"1970-01-01T00:00:00Z\"">>},
        {{0,0,0}, <<"\"1970-01-01T00:00:00.000Z\"">>}
    ]].

-endif.
