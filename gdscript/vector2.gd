extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/vector2.bin", file.WRITE)
    file.store_var(Vector2(0.0, 1.0))
    file.close()
    quit()

