extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/array_vector2.bin", file.WRITE)
    file.store_var([Vector2(0.0, 1.0), Vector2(9999.0, -120.22200012207031), Vector2(0.0, -1.0)])
    file.close()
    quit()

