godotdecoder
=====
[![CircleCI](https://circleci.com/gh/wot123/godotdecoder.svg?style=svg)](https://circleci.com/gh/wot123/godotdecoder)


Work in progress Godot v3 binary format decoder for Erlang
  
  
GDScript:  

```
func send_random_data(client):
	client.put_var({'id': 12, 'value':'cheese', 'wibble':1.22222})
```

```
<<92,0,0,0,18,0,0,0,3,0,0,0,4,0,0,0,2,0,0,0,105,100,0,0,2,0,1,0,12,0,0,0,0,0,0,
  0,4,0,0,0,5,0,0,0,118,97,108,117,101,0,0,0,4,0,0,0,6,0,0,0,99,104,101,101,
  115,101,0,0,4,0,0,0,6,0,0,0,119,105,98,98,108,101,0,0,3,0,1,0,32,70,8,143,54,
  142,243,63>>
```

```
Erlang/OTP 20 [erts-9.2] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:0] [hipe] [kernel-poll:false]  
Eshell V9.2  (abort with ^G)  
1> decoder:decode(<<92,0,0,0,18,0,0,0,3,0,0,0,4,0,0,0,2,0,0,0,105,100,0,0,2,0,1,0,12,0,0,0,0,0,0,0,4,0,0,0,5,0,0,0,118,97,108,117,101,0,0,0,4,0,0,0,6,0,0,0,99,104,101,101,115,101,0,0,4,0,0,0,6,0,0,0,119,105,98,98,108,101,0,0,3,0,1,0,32,70,8,143,54,142,243,63>>).
[#{"id" => 12,"value" => "cheese","wibble" => 1.22222}]
```

```
Erlang/OTP 20 [erts-9.2] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:0] [hipe] [kernel-poll:false]  
1> encoder:encode([ #{ <<"key">> => 1, 1 => 0.01}]).
<<56,0,0,0,18,0,0,0,2,0,0,0,2,0,1,0,1,0,0,0,0,0,0,0,3,0,1,
  0,123,...>>
2> 

```
Build
-----

    $ rebar3 compile


Test
-----

    $ rebar3 ct  


Generating new test data from Godot
-----

    $ export GODOT_BINARY=/path/to/godot/binary
    $ ./generate_test_data.sh
