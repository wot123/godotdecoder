extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/bool.bin", file.WRITE)
    file.store_var(false)
    file.close()
    quit()

