import bpy
import bmesh
import argparse
import sys
import mathutils

argv = sys.argv
if "--" not in argv:
    argv = []
else:
   argv = argv[argv.index("--") + 1:]

try:
    parser = argparse.ArgumentParser(description='Exports Blender model as a byte array for trifill rendering',prog = "blender -b -P "+__file__+" --")
    parser.add_argument('-o','--out', help='Output file', required=True, dest='out')
    args = parser.parse_args(argv)
except Exception as e:
    sys.exit(repr(e))

obdata = bpy.context.object.data
obj = bpy.context.object

# charset
charset="_0123456789abcdefghijklmnopqrstuvwxyz"

def pack_float(x):
    h = "{:02x}".format(int(round(32*x+128,0)))
    if len(h)!=2:
        raise Exception('Unable to convert: {} into a byte: {}'.format(x,h))
    return h

p8_colors = ['000000','1D2B53','7E2553','008751','AB5236','5F574F','C2C3C7','FFF1E8','FF004D','FFA300','FFEC27','00E436','29ADFF','83769C','FF77A8','FFCCAA']
def diffuse_to_p8color(rgb):
    h = "{:02X}{:02X}{:02X}".format(int(round(255*rgb.r)),int(round(255*rgb.g)),int(round(255*rgb.b)))
    try:
        #print("diffuse:{} -> {}\n".format(rgb,p8_colors.index(h)))
        return p8_colors.index(h)
    except Exception as e:
        # unknown color: purple!
        return 14

# model data
s = ""

# object name
name = bpy.context.object.name.lower()
s = s + "{:02x}".format(len(name))
for c in name:
    s = s + "{:02x}".format(charset.index(c)+1)

# scale (custom model property)
s = s + "{:02x}".format(bpy.context.object.get("scale", 1))

# get UV layer
uv_act = None
uv_layer = None
try:
    uv_act = obdata.uv_layers[0]
    uv_layer = uv_act.data if uv_act is not None else EmptyUV()
except Exception as e:
    print("No UV map")

# create a map loop index -> vertex index (see: https://www.python.org/dev/peps/pep-0274/)
loop_vert = {l.index:l.vertex_index for l in obdata.loops}

s = s + "{:02x}".format(len(obdata.vertices))
for v in obdata.vertices:
    s = s + "{}{}{}".format(pack_float(v.co.x), pack_float(v.co.z), pack_float(v.co.y))

# faces
s = s + "{:02x}".format(len(obdata.polygons))
for f in obdata.polygons:
    # vertex count
    s = s + "{:02x}".format(len(f.loop_indices))
    # vertex id 
    for li in f.loop_indices:
        s = s + "{:02x}".format(loop_vert[li]+1)
    # UV - assumes a 128x128 image map (e.g. pico sprite sheet)
    if uv_act is None:
        s = s + "00"
    else:
        s = s + "{:02x}".format(len(f.loop_indices))
        for li in f.loop_indices:
            uv = uv_layer[li].uv
            s = s + "{:02x}{:02x}".format(int(round(128*uv[0])), 128-int(round(128*uv[1])))
    # center point
    v = f.center
    s = s + "{}{}{}".format(pack_float(v[0]), pack_float(v[1]), pack_float(v[2]))
    
#normals
s = s + "{:02x}".format(len(obdata.polygons))
for f in obdata.polygons:
    s = s + "{}{}{}".format(pack_float(f.normal.x), pack_float(f.normal.z), pack_float(f.normal.y))

#
with open(args.out, 'w') as f:
    f.write(s)

