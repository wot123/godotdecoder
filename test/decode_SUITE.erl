-module(decode_SUITE).

-include_lib("common_test/include/ct.hrl").

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
         test_bool/1,
         test_null/1,
         test_mixed_dict_null_bool/1,
         test_null_dict/1,
         test_string_null_dict/1,
         test_vector2/1,
         test_vector3/1,
         test_rect2/1,
         test_mixed_dict_vector/1]).

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
     test_bool,
     test_null,
     test_mixed_dict_null_bool,
     test_null_dict,
     test_string_null_dict,
     test_vector2,
     test_vector3,
     test_rect2,
     test_mixed_dict_vector].


test_positive_integer(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/positive_int.bin"),
    [12] = decoder:decode(Bin).

test_negative_integer(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/negative_int.bin"),
    [-12] = decoder:decode(Bin).

test_positive_float(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/positive_float.bin"),
    [1.2222] = decoder:decode(Bin).

test_negative_float(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/negative_float.bin"),
    [-1.2222] = decoder:decode(Bin).

test_string(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/string.bin"),
    ["test string"] = decoder:decode(Bin).

test_string_dict(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/string_dict.bin"),
    [ #{"key" := "value", "key2" := "value2"} ] = 
        decoder:decode(Bin).

test_negative_large_float(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/negative_large_float.bin"),
    [-14444444444.2222] = decoder:decode(Bin).

test_int_dict(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/int_dict.bin"),
    [ #{1 := 99999999, -2 := -999999999}] = 
        decoder:decode(Bin).

test_float_dict(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/float_dict.bin"),
    [ #{1.2222 := 99999999.9999, -2.2222 := -999999999.9999}] =
        decoder:decode(Bin).

test_mixed_dict(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/mixed_dict.bin"),
    [ #{1 := "test string", 0.0001 := 12, "test string" := -999999999, -1 := -1.0001}] =

        decoder:decode(Bin).

test_int_list(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/int_list.bin"),
    [[1, 2, 3, 4, 9999999999, -1, -2, -3, -4, -9999999999]] = decoder:decode(Bin).

test_float_list(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/float_list.bin"),
    [[1.0, 2.01, 3.01, 4.444444, 9999999999.9999999999, -1.0, -2.01, -3.01, -4.444444, -9999999999.9999999999]] = decoder:decode(Bin).

test_int_dict_list(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/int_dict_list.bin"),
    [[#{1 := 2}, #{3 := 4, 9999999999 := -1}, #{-2 := -3, -4 := -9999999999}]] = decoder:decode(Bin).

test_float_dict_list(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/float_dict_list.bin"),
    [ [ #{1.01 := 2.2}, #{3.3 := 4.4 , 9999999999.99999999 := -1.1}, #{-2.2 := -3.3, -4.4 := -9999999999.999999999}]] = decoder:decode(Bin).

test_bool(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/bool.bin"),
    [false] = decoder:decode(Bin).

test_null(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/null.bin"),
    [null] = decoder:decode(Bin).

test_mixed_dict_null_bool(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/mixed_dict_null_bool.bin"),

    [ #{true := null, 1 := "test string", 0.0001 := 12, "test string" := -999999999, -1 := -1.0001}] = decoder:decode(Bin).

test_null_dict(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/null_dict.bin"),
    [ #{ null := null }] = decoder:decode(Bin).

test_string_null_dict(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/string_null_dict.bin"),
    [ #{ "key1" := null }] = decoder:decode(Bin).

test_vector2(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/vector2.bin"),
    [{vector2, 0.0, 1.0}] = decoder:decode(Bin).

test_vector3(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/vector3.bin"),
    [{vector3, 0.0, 1.0, 2.0}] = decoder:decode(Bin).

test_rect2(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/rect2.bin"),
    [{rect2, 0.0, 1.0, 2.0, 3.0}] = decoder:decode(Bin).

test_mixed_dict_vector(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/mixed_dict_vector.bin"),
    [#{ 1 := {vector2, 0.0, 3.0}, 0.0001 := {vector3, 0.0, 1.0, 2.0}, "test string" := {rect2, 0.0, 0.0, 1.0, 1.0}}] = decoder:decode(Bin).
