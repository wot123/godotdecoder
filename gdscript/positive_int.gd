extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/positive_int.bin", file.WRITE)
    file.store_var(12)
    file.close()
    quit()

