extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/float_list.bin", file.WRITE)
    file.store_var([1.0, 2.01, 3.01, 4.444444, 9999999999.9999999999, -1.0, -2.01, -3.01, -4.444444, -9999999999.9999999999])
    file.close()
    quit()

