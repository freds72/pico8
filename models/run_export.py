import os
from subprocess import Popen, PIPE
import re
import tempfile

local_dir = os.path.dirname(os.path.realpath(__file__))
blender_dir = os.path.expandvars("%programfiles%/Blender Foundation/Blender")

def call(args):
    print(args)

    proc = Popen(args, stdout=PIPE, stderr=PIPE)
    out, err = proc.communicate()
    exitcode = proc.returncode
    #
    return exitcode, out, err

s = "01"
fd, path = tempfile.mkstemp()
try:
    os.close(fd)
    exitcode, out, err = call([os.path.join(blender_dir,"blender.exe"),os.path.join(local_dir,"xwing.blend"),"--background","--python",os.path.join(local_dir,"blender_export.py"),"--","--out",path])
    # print("exit: {} \n out:{}\n err: {}\n".format(exitcode,out,err))
    with open(path, 'r') as outfile:
        s = s + outfile.read()
finally:
    os.remove(path)

s = re.sub("(.{256})", "\\1\n", s, 0, re.DOTALL)
print(s)