-module(encoder).

-export([encode/1]).
-include("include/godot.hrl").


encode_element(I) when is_integer(I) ->
    <<?INTEGER64, I:8/little-signed-integer-unit:8>>;

encode_element(F) when is_float(F) ->
    <<?FLOAT64, F:64/little-float>>;

encode_element(S) when is_binary(S) ->
    Size = byte_size(S),
    Pad = Size rem 4,
    % Pad string to 4 bytes
    encode_padded_string(S,Size,Pad);

encode_element(M) when is_map(M) ->
    ML = maps:to_list(M),
    Elements = lists:map( fun({K,V}) -> [encode_element(K), encode_element(V)] end, ML),
    Length = length(Elements),
    Bin = list_to_binary(Elements),
   <<?DICTIONARY, Length:?U_INT, Bin/binary>>;

encode_element(L) when is_list(L) ->
    Elements= lists:map( fun(E) -> [encode_element(E)] end, L),
    Length = length(Elements),
    Bin = list_to_binary(Elements),
    <<?ARRAY, Length:?U_INT, Bin/binary>>;

encode_element(#gd_vector2{x=X, y=Y}) ->
    <<?VECTOR2, X:32/little-float, Y:32/little-float>>;

encode_element(#gd_vector3{x=X, y=Y, z=Z}) ->
    <<?VECTOR3, X:32/little-float, Y:32/little-float, Z:32/little-float>>;

encode_element(#gd_rect2{x1 = X, y1 = Y, x2 = X2, y2 = Y2}) ->
    <<?RECT2, X:32/little-float, Y:32/little-float, X2:32/little-float, Y2:32/little-float>>.

encode_elements([]) ->
    [];

encode_elements([H|T]) ->
    [encode_element(H)] ++ encode_elements(T).

encode(Data) ->
    EncodedData = list_to_binary(encode_elements(Data)),
    Size = byte_size(EncodedData),
    <<Size:?U_INT, EncodedData/binary>>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


encode_padded_string(String, Size, 0) ->
    <<?STRING, Size:32/little-integer, String/binary>>;

encode_padded_string(String, Size, Pad) ->
    <<?STRING, Size:32/little-integer, String/binary, 0:((4-Pad)*8)>>.
