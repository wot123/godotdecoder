-module(decoder).

%% API exports
-export([decode/1]).

-define(U_INT, 4/little-unsigned-integer-unit:8).
-define(S_INT, 4/little-signed-integer-unit:8).

%% Godot version 3 encoding
-define(NULL, 0:?U_INT).
-define(BOOL, 1:?U_INT).
-define(INTEGER, 2:?U_INT).
-define(INTEGER64, 65538:?U_INT).


-define(FLOAT, 3:?U_INT).
-define(FLOAT64, 65539:?U_INT).
-define(STRING, 4:?U_INT).

-define(VECTOR2, 5:?U_INT).
-define(RECT2, 6:?U_INT).
-define(VECTOR3, 7:?U_INT).
-define(MATRIX32, 8:?U_INT).
-define(PLANE, 9:?U_INT).
-define(QUATERNION, 10:?U_INT).
-define(AABB, 11:?U_INT).
-define(MATRIX33, 12:?U_INT).
-define(TRANSFORM, 13:?U_INT).

-define(COLOR, 14:?U_INT).
-define(NODEPATH, 15:?U_INT).
-define(RID, 16:?U_INT).
-define(OBJECT, 17:?U_INT).
-define(DICTIONARY, 18:?U_INT).


%% invalid beyond this point
-define(ARRAY, 21:?U_INT).
-define(BYTEARRAY, 22:?U_INT).
-define(INTARRAY, 23:?U_INT).
-define(FLOATARRAY, 24:?U_INT).
-define(STRINGARRAY, 25:?U_INT).
-define(VECTOR2ARRAY, 26:?U_INT).
-define(VECTOR3ARRAY,27:?U_INT).
-define(COLORARRAY, 28:?U_INT).


%%====================================================================
%% API functions
%%====================================================================

decode_element(<<?NULL, 0:32, D/binary>>) ->
    {null , D};

decode_element(<<?BOOL, 0:32, D/binary>>) ->
    {false, D};
decode_element(<<?BOOL, 1:32, D/binary>>) ->
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

decode_element(<<?VECTOR2, X:32/float, Y:32/float, D/binary>>) ->
    {{X, Y}, D};

decode_element(<<?RECT2, X:32/float, Y:32/float, XS:32/float, YS:32/float, D/binary>>) ->
    {{X, Y, XS, YS}, D};

decode_element(<<?VECTOR3, X:32/float, Y:32/float, Z:32/float, D/binary>>) ->
    {{X, Y, Z}, D};

decode_element(<<?MATRIX32, ZEROZERO:32/float, ZEROONE:32/float, 
                    ONEZERO:32/float, ONEONE:32/float, 
                    TWOZERO:32/float, TWOONE:32/float, D/binary>>) ->
    {{{ZEROZERO, ZEROONE},{ONEZERO, ONEONE},{TWOZERO, TWOONE}}, D};

decode_element(<<?PLANE, X:32/float, Y:32/float, Z:32/float, Distance:32/float, D/binary>>) ->
    {{X,Y,Z,Distance}, D};

decode_element(<<?QUATERNION, X:32/float, Y:32/float, Z:32/float, R:32/float, D/binary>>) ->
    {{X,Y,Z,R},D};

decode_element(<<?AABB, X:32/float, Y:32/float, Z:32/float, XS:32/float, YS:32/float, ZS:32/float, D/binary>>) ->
    {{X,Y,Z,XS,YS,ZS}, D};

decode_element(<<?MATRIX33, ZEROZERO:32/float, ZEROONE:32/float, ZEROTWO:32/float,
                    ONEZERO:32/float, ONEONE:32/float, ONETWO:32/float,
                    TWOZERO:32/float, TWOONE:32/float, TWOTWO:32/float, D/binary>>) ->
    {{{ZEROZERO, ZEROONE, ZEROTWO},{ONEZERO, ONEONE, ONETWO},{TWOZERO,TWOONE,TWOTWO}}, D};

decode_element(<<?TRANSFORM, ZEROZERO:32/float, ZEROONE:32/float, ZEROTWO:32/float,
                     ONEZERO:32/float, ONEONE:32/float, ONETWO:32/float,
                     TWOZERO:32/float, TWOONE:32/float, TWOTWO:32/float,
                     THREEZERO:32/float, THREEONE:32/float, THREETWO:32/float, D/binary>>) ->
    {{{ZEROZERO, ZEROONE, ZEROTWO},{ONEZERO,ONEONE,ONETWO}, {TWOZERO,TWOONE,TWOTWO}, {THREEZERO,THREEONE, THREETWO}}, D};

decode_element(<<?COLOR, R:4/float-unit:8, G:32/float, B:32/float, A:32/float, D/binary>>) ->
    {{R,G,B,A}, D};

decode_element(<<?NODEPATH, L:?S_INT, String/binary>>) ->
    Length = L*8,
    <<String:Length, D/binary>> = String,
    {binary_to_list(String), D};

decode_element(<<?RID, D/binary>>) ->
    {unsupported, D};
decode_element(<<?OBJECT, D/binary>>) ->
    {unsupported, D};

decode_element(<<?DICTIONARY, Elements:?U_INT, Bin/binary>>) ->
    {Dict, B} = decode_dictionary(Bin, Elements, []),
    {{dictionary, Dict}, B};

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
    {Dict, Bin};

decode_dictionary(Bin, Elements, Dict) ->
    {Key, B} = decode_element(Bin),
    {Value, B2} = decode_element(B),
    decode_dictionary(B2, Elements-1, Dict ++ [{Key, Value}]).

decode_padded_string(Length, 4, Bin) ->
    <<String:Length/binary-unit:8, D/binary>> = Bin,
    {String, D};
decode_padded_string(Length, N, Bin) ->
    <<String:Length/binary-unit:8, _:N/binary-unit:8, D/binary>> = Bin,
    {String, D}.

%%====================================================================
%% Internal functions
%%====================================================================
