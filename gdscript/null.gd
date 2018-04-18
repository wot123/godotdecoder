extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/null.bin", file.WRITE)
    file.store_var(null)
    file.close()
    quit()

