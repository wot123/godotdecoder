extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/quat.bin", file.WRITE)
    file.store_var(Quat(0.0, 1.0, 2.0, 3.0))
    file.close()
    quit()

