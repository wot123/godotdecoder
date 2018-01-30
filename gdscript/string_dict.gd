extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/string_dict.bin", file.WRITE)
    file.store_var({"key":"value", "key2":"value2"})
    file.close()
    quit()

