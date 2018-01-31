extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/negative_large_float.bin", file.WRITE)
    file.store_var(-14444444444.2222)
    file.close()
    quit()

