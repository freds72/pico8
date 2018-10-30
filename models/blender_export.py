import bpy
import bmesh
import argparse
import sys

argv = sys.argv
if "--" not in argv:
    argv = []
else:
   argv = argv[argv.index("--") + 1:]

try:
    parser = argparse.ArgumentParser(description='Exports Blender model as a byte array for wireframe rendering',prog = "blender -b -P "+__file__+" --")
    parser.add_argument('-o','--out', help='Output file', required=True, dest='out')
    args = parser.parse_args(argv)
except Exception as e:
    sys.exit(repr(e))

obdata = bpy.context.object.data

# charset
charset="_0123456789abcdefghijklmnopqrstuvwxyz"

def pack_float(x):
    h = "{:02x}".format(int(round(32*x+128,0)))
    if len(h)!=2:
        raise Exception('Unable to convert: {} into a byte: {}'.format(x,h))
    return h

# model data
s = ""

# object name
name = bpy.context.object.name.lower()
s = s + "{:02x}".format(len(name))
for c in name:
    s = s + "{:02x}".format(charset.index(c)+1)

# scale (custom model property)
s = s + "{:02x}".format(bpy.context.object.get("scale", 1))

bm = bmesh.new()
bm.from_mesh(obdata)

s = s + "{:02x}".format(len(obdata.vertices))
for v in obdata.vertices:
    s = s + "{}{}{}".format(pack_float(v.co.x), pack_float(v.co.z), pack_float(v.co.y))

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
    s = s + "{}{}{}".format(pack_float(f.normal.x), pack_float(f.normal.z), pack_float(f.normal.y))

# all edges
s = s + "{:02x}".format(len(bm.edges))
for e in bm.edges:
    s = s + "{:02x}{:02x}{:02x}".format(e.verts[0].index+1, e.verts[1].index+1,1 if e.is_wire else 0)

#
with open(args.out, 'w') as f:
    f.write(s)

