extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/transform.bin", file.WRITE)
    file.store_var(Transform(Basis(Vector3(0.0, 0.0, 0.0), Vector3(1.0,1.0,1.0), Vector3(2.0,2.0,2.0)), Vector3(2.0,2.0,5.0)))
    file.close()
    quit()

