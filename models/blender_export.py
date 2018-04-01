import bpy
import bmesh

print('\"{}\"={{\n'.format(bpy.context.object.name))

obdata = bpy.context.object.data
print('\"v\"={')
first=True
for v in obdata.vertices:
    if first==False:
        print(',',end='')
    print('{{{},{},{}}}'.format(round(v.co.x,2), round(v.co.z,2), round(v.co.y,2)),end='')
    first=False

print('},')

#faces
print('\"f\"={')
first=True
for f in obdata.polygons:
    if first==False:
        print(',',end='')
    # print number of vertices    
    print('{}'.format(len(f.vertices)),end='')
    for v in f.vertices:
        print(',{}'.format(v+1), end='')
    first=False

#normals
print('\"n\"={')
first=True
for f in obdata.polygons:
    if first==False:
        print(',',end='')
    print('{{{},{},{}}}'.format(round(f.normal.x,2), round(f.normal.z,2), round(f.normal.y,2)),end='')
    first=False

print('},')

#edges (not connected to a face)
bm = bmesh.new()
bm.from_mesh(obdata)

print('\"e\"={')
first=True
for e in bm.edges:
    if e.is_wire:
        if first==False:
            print(',',end='')        
        print('{},{}'.format(e.verts[0].index, e.verts[1].index),end='')
        first=False

print('},')

print('}')