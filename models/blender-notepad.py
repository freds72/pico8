for f in bm.faces:
    for l in f.loops: # python Range object with the proper indices already set
        v = obdata.vertices[l.vert.index] # The vertex data that loop entry refers to
        print("	Loop index", l.index, "points to vertex index", l.vert.index, "at position", v.co)
        for j,ul in enumerate(bm.layers):
            print("  UV Map", j, "has coordinates", ul.data[l.index].uv, "for this loop index")

class EmptyUV:
    uv = (0.0, 0.0)
    def __getitem__(self, index): return self

obdata = bpy.context.object.data
obj = bpy.context.object

# get UV layer
uv_act = obdata.uv_layers[0]
uv_layer = uv_act.data if uv_act is not None else EmptyUV()

# create a map loop index -> vertex index (see: https://www.python.org/dev/peps/pep-0274/)
verts = obdata.vertices
loop_vert = {l.index:l.vertex_index for l in obdata.loops}

# todo: iterate over loops
# get vertex index (export as usual)
# get uv for vertex
for face in obdata.polygons:
    print("face: {}".format(face.index))
    for li in face.loop_indices:
        print("vert: {} loop: {}, uv:{}".format(loop_vert[li], li, *uv_layer[li].uv))