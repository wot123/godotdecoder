extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/null_dict.bin", file.WRITE)
    file.store_var({null: null})
    file.close()
    quit()

