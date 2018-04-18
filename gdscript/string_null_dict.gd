extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/string_null_dict.bin", file.WRITE)
    file.store_var({"key1": null})
    file.close()
    quit()

