extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/aabb.bin", file.WRITE)
    file.store_var(AABB(Vector3(0.0, 1.0, 2.0), Vector3(1.0, 2.0, 3.0)))
    file.close()
    quit()

