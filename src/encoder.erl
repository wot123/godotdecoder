-module(encoder).

-export([encode/1]).
-include("include/godot.hrl").


encode_element([{int, I}|T])->
    {<<?INTEGER64, I:8/little-signed-integer-unit:8>>,T};

encode_element([{float, F}|T]) ->
    {<<?FLOAT64, F:64/little-float>>,T};

encode_element([{string, S}|T]) ->
    Size = byte_size(S),
    Padding = Size + (4 - (Size rem 4)),
    {<<?STRING, Padding:32/little-integer,S/binary>>, T}.


encode_elements([]) ->
    [];

encode_elements(Data) ->
    {Element, R} = encode_element(Data),
    [Element] ++ encode_elements(R).

encode(Data) ->
    list_to_binary(encode_elements(Data)).

