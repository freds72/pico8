import bpy
import bmesh
import re

obdata = bpy.context.object.data

# charset
charset="_0123456789abcdefghijklmnopqrstuvwxyz"

# how many models (to be manually edited)
s = ""

# object name
name = bpy.context.object.name.lower()
s = s + "{:02x}".format(len(name))
for c in name:
    s = s + "{:02x}".format(charset.index(c)+1)

bm = bmesh.new()
bm.from_mesh(obdata)

s = s + "{:02x}".format(len(obdata.vertices))
for v in obdata.vertices:
    s = s + "{:02x}{:02x}{:02x}".format(int(round(32*v.co.x+128,0)), int(round(32*v.co.z+128,0)), int(round(32*v.co.y+128,0)))

# faces:
s = s + "{:02x}".format(len(bm.faces))
for f in bm.faces:
    # face point index
    # edge count
    s = s + "{:02x}{:02x}".format(f.verts[0].index+1,len(f.edges))
    # edge id's
    for e in f.edges:
        s = s + "{:02x}".format(e.index+1)

#normals
s = s + "{:02x}".format(len(obdata.polygons))
for f in obdata.polygons:
    s = s + "{:02x}{:02x}{:02x}".format(int(round(32*f.normal.x+128,0)), int(round(32*f.normal.z+128,0)), int(round(32*f.normal.y+128,0)))

# all edges
s = s + "{:02x}".format(len(bm.edges))
for e in bm.edges:
    s = s + "{:02x}{:02x}{:02x}".format(e.verts[0].index+1, e.verts[1].index+1,1 if e.is_wire else 0)

#s = re.sub("(.{256})", "\\1\n", s, 0, re.DOTALL)
print(s)

# copy to clipboard
bpy.context.window_manager.clipboard=s

