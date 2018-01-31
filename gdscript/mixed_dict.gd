extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/mixed_dict.bin", file.WRITE)
    file.store_var({1:"test string", 0.0001: 12, "test string": -999999999, -1: -1.0001})
    file.close()
    quit()

