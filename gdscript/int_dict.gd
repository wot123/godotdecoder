extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/int_dict.bin", file.WRITE)
    file.store_var({1:99999999, -2: -999999999})
    file.close()
    quit()

