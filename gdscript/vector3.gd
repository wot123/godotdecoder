extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/vector3.bin", file.WRITE)
    file.store_var(Vector3(0.0, 1.0, 2.0))
    file.close()
    quit()

