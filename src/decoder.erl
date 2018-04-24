-module(decoder).

%% API exports
-export([decode/1]).
-include("include/godot.hrl").

%%====================================================================
%% API functions
%%====================================================================

decode_element(<<?NULL, D/binary>>) ->
    {null , D};

decode_element(<<?BOOL, 0:?U_INT, D/binary>>) ->
    {false, D};
decode_element(<<?BOOL, 1:?U_INT, D/binary>>) ->
    {true, D};

decode_element(<<?INTEGER, I:4/little-signed-integer-unit:8, D/binary>>) ->
    {I, D};

decode_element(<<?INTEGER64, I:8/little-signed-integer-unit:8, D/binary>>) ->
    {I, D};

decode_element(<<?FLOAT, F:32/little-float, D/binary>>) ->
    {F, D};

decode_element(<<?FLOAT64, F:64/little-float, D/binary>>) ->
    {F, D};

decode_element(<<?STRING, L:32/little-integer, S/binary>>) ->
    % data is padded to 4 bytes.
    Padding = (4 - (L rem 4)),
    {String, D} = decode_padded_string(L, Padding, S),
    {binary_to_list(String), D};

decode_element(<<?VECTOR2, X:32/little-float, Y:32/little-float, D/binary>>) ->
    {#gd_vector2{x=X, y=Y}, D};

decode_element(<<?RECT2, X:32/little-float, Y:32/little-float, XS:32/little-float, YS:32/little-float, D/binary>>) ->
    {#gd_rect2{x1 = X, y1 = Y, x2 = XS, y2 = YS}, D};

decode_element(<<?VECTOR3, X:32/little-float, Y:32/little-float, Z:32/little-float, D/binary>>) ->
    {#gd_vector3{ x = X, y = Y,z = Z}, D};

decode_element(<<?BASIS, X1:32/little-float, X2:32/little-float, X3:32/little-float, 
                         Y1:32/little-float, Y2:32/little-float, Y3:32/little-float, 
                         Z1:32/little-float, Z2:32/little-float, Z3:32/little-float, D/binary>>) ->

    {#gd_basis{ x_axis = #gd_vector3{x = X1, y = Y1, z = Z1},
                y_axis = #gd_vector3{x = X2, y = Y2, z = Z2},
                z_axis = #gd_vector3{x = X3, y = Y3, z = Z3}}, D};

decode_element(<<?PLANE, X:32/little-float, Y:32/little-float, Z:32/little-float, 
                 Distance:32/little-float, D/binary>>) ->
    {#gd_plane{x = X, y = Y, z = Z, d = Distance}, D};

decode_element(<<?QUAT, X:32/little-float, Y:32/little-float, Z:32/little-float, W:32/little-float, D/binary>>) ->
    {#gd_quat{x = X, y = Y, z = Z, w = W},D};

decode_element(<<?AABB, X:32/little-float, Y:32/little-float, Z:32/little-float, 
                        XS:32/little-float, YS:32/little-float, ZS:32/little-float, D/binary>>) ->
    {#gd_aabb{position=#gd_vector3{x=X, y=Y, z=Z}, size=#gd_vector3{x=XS, y=YS, z=ZS}}, D};


decode_element(<<?TRANSFORM2D, ZEROZERO:32/float, ZEROONE:32/float, ZEROTWO:32/float,
                     ONEZERO:32/float, ONEONE:32/float, ONETWO:32/float,
                     TWOZERO:32/float, TWOONE:32/float, TWOTWO:32/float,
                     THREEZERO:32/float, THREEONE:32/float, THREETWO:32/float, D/binary>>) ->
    {{{ZEROZERO, ZEROONE, ZEROTWO},{ONEZERO,ONEONE,ONETWO}, {TWOZERO,TWOONE,TWOTWO}, {THREEZERO,THREEONE, THREETWO}}, D};

decode_element(<<?COLOR, R:32/little-float, G:32/little-float, B:32/little-float, A:32/little-float, D/binary>>) ->
    {#gd_color{r = R, g = G, b = B, a = A}, D};

decode_element(<<?NODEPATH, L:?S_INT, String/binary>>) ->
    Length = L*8,
    <<String:Length, D/binary>> = String,
    {binary_to_list(String), D};

decode_element(<<?RID, D/binary>>) ->
    {unsupported, D};
decode_element(<<?OBJECT, D/binary>>) ->
    {unsupported, D};

decode_element(<<?DICTIONARY, Elements:?U_INT, Bin/binary>>) ->
    decode_dictionary(Bin, Elements, []);

decode_element(<<?ARRAY, Elements:?U_INT, Bin/binary>>) ->
    decode_array(Bin, Elements, []);

decode_element(E) ->
    io:format("E: ~p", [E]),
    {none, <<>>}.

decode(<<L:?U_INT, Bin/binary>>) when byte_size(Bin) == L ->
    decode_elements(Bin);

decode(_) ->
    error.

decode_elements(<<>>) ->
    [];
decode_elements(Bin) ->
    {Element, R} = decode_element(Bin),
    [Element] ++  decode_elements(R).


decode_dictionary(Bin, 0, Dict) ->
    Map = maps:from_list(Dict),
    {Map, Bin};

decode_dictionary(Bin, Elements, Dict) ->
    {Key, Bin2} = decode_element(Bin),
    {Value, Bin3} = decode_element(Bin2),
    decode_dictionary(Bin3, Elements-1, Dict ++ [{Key, Value}]).

decode_array(Bin, 0, Array) ->
    {Array, Bin};

decode_array(Bin, Elements, Array) ->
    {Value, Bin2} = decode_element(Bin),
    decode_array(Bin2, Elements-1, Array ++ [Value]).


decode_padded_string(Length, 4, Bin) ->
    <<String:Length/binary-unit:8, D/binary>> = Bin,
    {String, D};
decode_padded_string(Length, N, Bin) ->
    <<String:Length/binary-unit:8, _:N/binary-unit:8, D/binary>> = Bin,
    {String, D}.

%%====================================================================
%% Internal functions
%%====================================================================
