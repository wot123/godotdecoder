# 1 "int_list.gd"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 1 "<command-line>" 2
# 1 "int_list.gd"
extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/float_dict_list.bin", file.WRITE)
    file.store_var([{1.01: 2.2},{ 3.3: 4.4, 9999999999.99999999 :-1.1}, {-2.2: -3.3, -4.4: -9999999999.999999999}])
    file.close()
    quit()
