extends SceneTree

func _init():
    var file = File.new()
    file.open("priv/gdscript_bin/rect2.bin", file.WRITE)
    file.store_var(Rect2(0.0, 1.0, 2.0, 3.0))
    file.close()
    quit()

