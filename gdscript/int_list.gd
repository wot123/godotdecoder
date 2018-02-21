extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/int_list.bin", file.WRITE)
    file.store_var([1, 2, 3, 4, 9999999999, -1, -2, -3, -4, -9999999999])
    file.close()
    quit()

