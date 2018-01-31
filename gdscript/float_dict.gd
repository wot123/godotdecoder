extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/float_dict.bin", file.WRITE)
    file.store_var({1.2222 : 99999999.9999, -2.2222: -999999999.9999})
    file.close()
    quit()

