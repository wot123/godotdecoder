-module(decode_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([test_positive_integer/1,
         test_negative_integer/1,
         test_positive_float/1,
         test_negative_float/1,
         test_string/1,
         test_string_dict/1]).


all() ->
    [test_positive_integer,
     test_negative_integer,
     test_positive_float,
     test_negative_float,
     test_string,
     test_string_dict].

test_positive_integer(_) ->
    io:format("~p", [filename:absname(".")]),
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
