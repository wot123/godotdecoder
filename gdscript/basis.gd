extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/basis.bin", file.WRITE)
    file.store_var(Basis(Vector3(1.0, 2.0, 3.0),Vector3(4.0, 5.0, 6.0), Vector3(7.0, 8.0, 9.0)))
    file.close()
    quit()

