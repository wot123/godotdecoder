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
         test_float_dict_list/1]).

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
     test_float_dict_list].

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
    [ {dictionary, [{"key","value"},{"key2", "value2"}]}] = 
        decoder:decode(Bin).

test_negative_large_float(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/negative_large_float.bin"),
    [-14444444444.2222] = decoder:decode(Bin).

test_int_dict(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/int_dict.bin"),
    [ {dictionary, [ {1,99999999}, {-2, -999999999}]}] = 
        decoder:decode(Bin).

test_float_dict(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/float_dict.bin"),
    [ {dictionary, [ {1.2222, 99999999.9999}, {-2.2222, -999999999.9999}]}] =
        decoder:decode(Bin).

test_mixed_dict(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/mixed_dict.bin"),
    [ {dictionary, [ {1,"test string"}, {0.0001, 12}, {"test string", -999999999}, {-1, -1.0001}]}] =
        decoder:decode(Bin).

test_int_list(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/int_list.bin"),
    [{array, [1, 2, 3, 4, 9999999999, -1, -2, -3, -4, -9999999999]}] = decoder:decode(Bin).

test_float_list(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/float_list.bin"),
    [{array, [1.0, 2.01, 3.01, 4.444444, 9999999999.9999999999, -1.0, -2.01, -3.01, -4.444444, -9999999999.9999999999]}] = decoder:decode(Bin).

test_int_dict_list(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/int_dict_list.bin"),
    [{array, [{dictionary, [{1,2}]}, {dictionary, [{3,4},{9999999999,-1}]}, {dictionary,[{-2,-3},{-4, -9999999999}]}]}] = decoder:decode(Bin).

test_float_dict_list(_) ->
    {ok, Bin} = file:read_file(code:priv_dir(godotdecoder) ++ "/gdscript_bin/float_dict_list.bin"),
    [{array, [{dictionary, [{1.01, 2.2}]}, {dictionary,[{3.3, 4.4},{9999999999.99999999, -1.1}]}, {dictionary, [{-2.2, -3.3},{-4.4,-9999999999.999999999}]}]}] = decoder:decode(Bin).
