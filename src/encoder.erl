-module(encoder).

-export([encode/1]).
-include("include/godot.hrl").


encode_element(I) when is_integer(I) ->
    <<?INTEGER, I:8/little-signed-integer-unit:4>>;

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
    <<?RECT2, X:32/little-float, Y:32/little-float, X2:32/little-float, Y2:32/little-float>>;

encode_element(#gd_color{r = R, g = G, b = B, a = A}) ->
    <<?COLOR, R:32/little-float, G:32/little-float, B:32/little-float, A:32/little-float>>;

encode_element(#gd_aabb{position = #gd_vector3{x=PX, y=PY, z=PZ},
                        size = #gd_vector3{x=SX, y = SY, z = SZ}}) ->
    <<?AABB, PX:32/little-float, PY:32/little-float, PZ:32/little-float,
             SX:32/little-float, SY:32/little-float, SZ:32/little-float>>;

encode_element(#gd_plane{x = X, y = Y, z = Z, d = Distance}) ->
    <<?PLANE, X:32/little-float, Y:32/little-float, Z:32/little-float, Distance:32/little-float>>;

encode_element(#gd_quat{ x = X, y = Y, z = Z, w = W}) ->
    <<?QUAT, X:32/little-float, Y:32/little-float, Z:32/little-float, W:32/little-float>>;


encode_element(#gd_basis{ x_axis = #gd_vector3{x = X1, y = Y1, z = Z1},
                          y_axis = #gd_vector3{x = X2, y = Y2, z = Z2},
                          z_axis = #gd_vector3{x = X3, y = Y3, z= Z3}}) ->
    <<?BASIS, X1:32/little-float, X2:32/little-float, X3:32/little-float,
              Y1:32/little-float, Y2:32/little-float, Y3:32/little-float,
              Z1:32/little-float, Z2:32/little-float, Z3:32/little-float>>;

encode_element(#gd_transform2d{ x_axis = #gd_vector2{ x = X1, y = Y1},
                                y_axis = #gd_vector2{ x = X2, y = Y2},
                                origin = #gd_vector2{ x = X3, y = Y3}}) ->
    <<?TRANSFORM2D, X1:32/little-float, Y1:32/little-float,
                    X2:32/little-float, Y2:32/little-float,
                    X3:32/little-float, Y3:32/little-float>>;

encode_element(#gd_transform{ 
                  basis = #gd_basis{ x_axis = #gd_vector3{x = X1, y = Y1, z = Z1},
                                     y_axis = #gd_vector3{x = X2, y = Y2, z = Z2},
                                     z_axis = #gd_vector3{x = X3, y = Y3, z = Z3}},
                  origin = #gd_vector3{x = X4, y = Y4, z = Z4}}) ->
    <<?TRANSFORM, X1:32/little-float, X2:32/little-float, X3:32/little-float,
                  Y1:32/little-float, Y2:32/little-float, Y3:32/little-float,
                  Z1:32/little-float, Z2:32/little-float, Z3:32/little-float,
                  X4:32/little-float, Y4:32/little-float, Z4:32/little-float>>.


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
