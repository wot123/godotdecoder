extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/transform2d.bin", file.WRITE)
    file.store_var(Transform2D(Vector2(0.0, 0.0), Vector2(1.0, 1.0), Vector2(2.0,2.0)))
    file.close()
    quit()

