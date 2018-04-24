extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/color.bin", file.WRITE)
    file.store_var(Color(0.0, 1.0, 0.0, 1.0))
    file.close()
    quit()

