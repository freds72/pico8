import os
from subprocess import Popen, PIPE
import re
import tempfile

local_dir = os.path.dirname(os.path.realpath(__file__))
blender_dir = os.path.expandvars("%programfiles%/Blender Foundation/Blender")

def call(args):
    proc = Popen(args, stdout=PIPE, stderr=PIPE)
    out, err = proc.communicate()
    exitcode = proc.returncode
    #
    return exitcode, out, err

file_list = ['vtree','ground','plane','piper-cub_simple','piper-cub_bbox','cornell','torus']
s = "{:02x}".format(len(file_list))
for blend_file in file_list:
    print("Exporting: {}.blend".format(blend_file))
    fd, path = tempfile.mkstemp()
    try:
        os.close(fd)
        exitcode, out, err = call([os.path.join(blender_dir,"blender.exe"),os.path.join(local_dir,blend_file + ".blend"),"--background","--python",os.path.join(local_dir,"blender_export_fill.py"),"--","--out",path])
        if err:
            raise Exception('Unable to loadt: {}. Exception: {}'.format(blend_file,err))
        #print("exit: {} \n out:{}\n err: {}\n".format(exitcode,out,err))
        with open(path, 'r') as outfile:
            s = s + outfile.read()
    finally:
        os.remove(path)

# pico-8 map format
# first 4096 bytes -> gfx (shared w/ map)
# second 4096 bytes -> map
if len(s)>=2*8192:
    raise Exception('Data string too long ({})'.format(len(s)))

tmp=s[:8192]
print("__gfx__")
# swap bytes
gfx_data = ""
for i in range(0,len(tmp),2):
    gfx_data = gfx_data + tmp[i+1:i+2] + tmp[i:i+1]
print(re.sub("(.{128})", "\\1\n", gfx_data, 0, re.DOTALL))

map_data=s[8192:]
if len(map_data)>0:
    print("__map__")
    print(re.sub("(.{256})", "\\1\n", map_data, 0, re.DOTALL))

