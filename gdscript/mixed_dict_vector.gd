extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/mixed_dict_vector.bin", file.WRITE)
    file.store_var({1: Vector2(0.0, 3.0), 0.0001: Vector3(0.0, 1.0, 2.0), "test string": Rect2(0.0, 0.0, 1.0, 1.0)})
    file.close()
    quit()

