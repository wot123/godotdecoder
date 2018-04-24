-module(encode_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("include/godot.hrl").

-export([all/0]).
-export([test_positive_integer/1,
         test_negative_integer/1,
         test_positive_float/1,
         test_negative_float/1,
         test_string/1,
         test_string_dict/1,
         test_negative_large_float/1,
         test_int_dict/1,
         test_float_dict/1,
         test_mixed_dict/1,
         test_int_list/1,
         test_float_list/1,
         test_int_dict_list/1,
         test_float_dict_list/1,
         test_vector2/1,
         test_vector3/1,
         test_rect2/1]).

all() ->
    [test_positive_integer,
     test_negative_integer,
     test_positive_float,
     test_negative_float,
     test_string,
     test_string_dict,
     test_negative_large_float,
     test_int_dict,
     test_float_dict,
     test_mixed_dict,
     test_int_list,
     test_float_list,
     test_int_dict_list,
     test_float_dict_list,
     test_vector2,
     test_vector3,
     test_rect2].

test_positive_integer(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/positive_int.bin"),

    Bin = encoder:encode([12]).

test_negative_integer(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/negative_int.bin"),
    Bin = encoder:encode([-12]).

test_positive_float(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/positive_float.bin"),
    Bin = encoder:encode([1.2222]).

test_negative_float(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/negative_float.bin"),
    Bin = encoder:encode([-1.2222]).

test_string(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/string.bin"),
    Bin = encoder:encode([<<"test string">>]).

test_string_dict(_) ->
    % this currently fails godot isnt counting the padding at the end as part of the length
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/string_dict.bin"),
    Bin2 = encoder:encode([#{<<"key">> => <<"value">>, <<"key2">> => <<"value2">>}]),

    [Decode1] = decoder:decode(Bin),
    [Decode2] = decoder:decode(Bin2),

    lists:sort(maps:to_list(Decode1)) == lists:sort(maps:to_list(Decode2)).


test_negative_large_float(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/negative_large_float.bin"),
    Bin = encoder:encode([-14444444444.2222]).

test_int_dict(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/int_dict.bin"),
    Encoded = encoder:encode([#{1 => 99999999, -2 => -999999999}]),
 
    [Bin1] = decoder:decode(Bin),
    [Bin2] = decoder:decode(Encoded),

    lists:sort(maps:to_list(Bin1)) == lists:sort(maps:to_list(Bin2)).

test_float_dict(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/float_dict.bin"),
    Encoded = encoder:encode([ #{1.2222 => 99999999.9999, -2.2222 => -999999999.9999}]),

    [Bin1] = decoder:decode(Bin),
    [Bin2] = decoder:decode(Encoded),

    lists:sort(maps:to_list(Bin1)) == lists:sort(maps:to_list(Bin2)).

test_mixed_dict(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/mixed_dict.bin"),
    Encoded = encoder:encode([ #{1 => "test string", 0.0001 => 12, "test string" => -999999999, -1 => -1.0001}]),

    [Bin1] = decoder:decode(Bin),
    [Bin2] = decoder:decode(Encoded),

    lists:sort(maps:to_list(Bin1)) == lists:sort(maps:to_list(Bin2)).


test_int_list(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/int_list.bin"),
    Encoded = encoder:encode([[1, 2, 3, 4, 9999999999, -1, -2, -3, -4, -9999999999]]),

    [Bin1] = decoder:decode(Bin),
    [Bin2] = decoder:decode(Encoded),

    lists:sort(Bin1) == lists:sort(Bin2).

test_float_list(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/float_list.bin"),
    Encoded = encoder:encode([[1.0, 2.01, 3.01, 4.444444, 9999999999.9999999999, -1.0, -2.01, -3.01, -4.444444, -9999999999.9999999999]]),

    [Bin1] = decoder:decode(Bin),
    [Bin2] = decoder:decode(Encoded),

    lists:sort(Bin1) == lists:sort(Bin2).

test_int_dict_list(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/int_dict_list.bin"),
    Encoded = encoder:encode([[#{1 => 2}, #{3 => 4, 9999999999 => -1}, #{-2 => -3, -4 => -9999999999}]]),

    [Bin1] = decoder:decode(Bin),
    [Bin2] = decoder:decode(Encoded),

    lists:sort(Bin1) == lists:sort(Bin2).


test_float_dict_list(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/float_dict_list.bin"),
    Encoded = encoder:encode([ [ #{1.01 => 2.2}, #{3.3 => 4.4 , 9999999999.99999999 => -1.1}, #{-2.2 => -3.3, -4.4 => -9999999999.999999999}]]),

    [Bin1] = decoder:decode(Bin),
    [Bin2] = decoder:decode(Encoded),

    lists:sort(Bin1) == lists:sort(Bin2).

test_vector2(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/vector2.bin"),
    Bin = encoder:encode([#gd_vector2{ x = 0.0, y = 1.0}]).
   
test_vector3(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/vector3.bin"),
    Bin = encoder:encode([#gd_vector3{ x = 0.0, y = 1.0, z = 2.0}]).

test_rect2(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/rect2.bin"),
    Bin = encoder:encode([#gd_rect2{x1 = 0.0, y1 = 1.0, x2 = 2.0, y2 = 3.0}]).
