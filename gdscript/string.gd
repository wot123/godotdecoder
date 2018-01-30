extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/string.bin", file.WRITE)
    file.store_var("test string")
    file.close()
    quit()

