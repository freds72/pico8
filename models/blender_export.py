import bpy
import bmesh

s = "\"{}\":{{\n".format(bpy.context.object.name)

obdata = bpy.context.object.data
bm = bmesh.new()
bm.from_mesh(obdata)

s = s + "\"v\":["
first=True
for v in obdata.vertices:
    if first==False:
        s = s + ','
    s = s + "[{},{},{}]".format(round(v.co.x,2), round(v.co.z,2), round(v.co.y,2))
    first=False

s = s + "],\n"

# faces:
# edge count
# edge id
s = s + "\"f\":["
first=True
for f in bm.faces:
    if first==False:
        s = s + ","
    # number of edges
    # point index in face
    s = s + "[{},{}".format(len(f.edges),f.verts[0].index+1)
    # all edges
    for e in f.edges:
        s = s + ",{}".format(e.index+1)
    s = s + "]"
    first=False

s = s + "],\n"

#normals
s = s + "\"n\":["
first=True
for f in obdata.polygons:
    if first==False:
        s = s + ","
    s = s + "[{},{},{}]".format(round(f.normal.x,2), round(f.normal.z,2), round(f.normal.y,2))
    first=False

s = s + "],\n"

# all edges
s = s + "\"e\":["
first=True
for e in bm.edges:
    if first==False:
        s = s + ","
    s = s + "[{},{},{}]".format(e.verts[0].index+1, e.verts[1].index+1,'true' if e.is_wire else '-1')
    first=False

s = s + "]}\n"
print(s)

# copy to clipboard
bpy.context.window_manager.clipboard=s

