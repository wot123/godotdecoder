# 1 "int_list.gd"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 1 "<command-line>" 2
# 1 "int_list.gd"
extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/int_dict_list.bin", file.WRITE)
    file.store_var([{1: 2},{ 3: 4, 9999999999 :-1}, {-2: -3, -4: -9999999999}])
    file.close()
    quit()
