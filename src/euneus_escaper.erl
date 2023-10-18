-module(euneus_escaper).

-export([ escape/1, invalid_byte_error/2 ]).

-callback escape(Char) -> Result
    when Char :: unicode:chardata()
       , Result :: iolist()
       .

escape(0) -> <<"\\u0000">>;
escape(1) -> <<"\\u0001">>;
escape(2) -> <<"\\u0002">>;
escape(3) -> <<"\\u0003">>;
escape(4) -> <<"\\u0004">>;
escape(5) -> <<"\\u0005">>;
escape(6) -> <<"\\u0006">>;
escape(7) -> <<"\\u0007">>;
escape(8) -> <<"\\b">>;
escape(9) -> <<"\\t">>;
escape(10) -> <<"\\n">>;
escape(11) -> <<"\\u000B">>;
escape(12) -> <<"\\f">>;
escape(13) -> <<"\\r">>;
escape(14) -> <<"\\u000E">>;
escape(15) -> <<"\\u000F">>;
escape(16) -> <<"\\u0010">>;
escape(17) -> <<"\\u0011">>;
escape(18) -> <<"\\u0012">>;
escape(19) -> <<"\\u0013">>;
escape(20) -> <<"\\u0014">>;
escape(21) -> <<"\\u0015">>;
escape(22) -> <<"\\u0016">>;
escape(23) -> <<"\\u0017">>;
escape(24) -> <<"\\u0018">>;
escape(25) -> <<"\\u0019">>;
escape(26) -> <<"\\u001A">>;
escape(27) -> <<"\\u001B">>;
escape(28) -> <<"\\u001C">>;
escape(29) -> <<"\\u001D">>;
escape(30) -> <<"\\u001E">>;
escape(31) -> <<"\\u001F">>;
escape(34) -> <<"\\\"">>;
escape(47) -> <<"\\/">>;
escape(92) -> <<"\\\\">>;
escape(Byte) -> invalid_byte_error(Byte, Byte).

invalid_byte_error(Byte0, Input) ->
    Byte = <<"0x"/utf8, (integer_to_binary(Byte0, 16))/binary>>,
    error({invalid_byte, Byte, Input}).
