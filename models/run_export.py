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

# file_list = ['deathstar','junk2','tie','xwing','ywing','title','turret','trench1','vent','mfalcon','generator','tiex1']
# file_list = ['audi','audi_bbox','vtree']
file_list = ['205gti','205gti_bbox']
s = "{:02x}".format(len(file_list))
for blend_file in file_list:
    print("Exporting: {}.blend".format(blend_file))
    fd, path = tempfile.mkstemp()
    try:
        os.close(fd)
        exitcode, out, err = call([os.path.join(blender_dir,"blender.exe"),os.path.join(local_dir,blend_file + ".blend"),"--background","--python",os.path.join(local_dir,"blender_export_uv.py"),"--","--out",path])
        if err:
            raise Exception('Unable to loadt: {}. Exception: {}'.format(blend_file,err))
        print("exit: {} \n out:{}\n err: {}\n".format(exitcode,out,err))
        with open(path, 'r') as outfile:
            s = s + outfile.read()
    finally:
        os.remove(path)

# extra data
s = s + "ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00d00008860948f70008880948c90008840948a000088309118a12510948c700088609489e00088309118c125109c600088c12518509488a0008850911941251c500088e1251850948880008850911961248c3000809ba120948c2008209ba125109c2008209bb1251c2000911bc1248c10011ad120a83090141094a88120948c000ad120a830901820041098812510948bf0088120a86094a96120a8309018a00418912510948bd0008881288094a94120a8309018c004a89125109bc00080988128209018a00418c09019300094a891251bc008209881209018c00418a0901940041094a891248bb008209881201ae0041094a88120948ba0009118812b00041098812510948b900118912b100418912510948b80089120ab2004a89125109b80088120a09b200094a891251b7000888128209b20041094a891248b500080988128209b30041094a88120948b400820988120901b400410988125109b4008209881201b60041891251b400091187120ab8004a891248b3001187120a09b800094a88120948b20088128209b80041098812510948b10088128209b900418912510948af000888120901ba004a89125109ae000809881201bb00094a891251ae0082098812bc0041094a891248ad0082098812bd0041094a88120948ac008209881248bd0041098812510948ab00820988120948bd00418912510948aa00820988125109be004a89125109aa008209891251be00094a891251aa0041094a891248bd0041094a891248aa0041094a88120948bd0041094a88120948aa0041098812510948bd00410988125109ab00418912510948bd0041891251ac004a89125109be004a8912ac00094a891251be00094a8812ac0041094a891248bd008209881248ac0041094a88120948bc00820988120948ac0041098812510948bb00410988128209ad00418912510948bb004188128209ae004a89125109bc0088128209ae00094a891251bc0088128209ae0041094a891248bb004a87125109af0041094a88120948ba00094a871251b00041098812510948b90082098812b100418912510948b80082098812b2004a89125109b8004109881248b100094a891251b9004188120948b00041098a1248b90088128209b100418a120948b80088128209b200418912510948b70088128209b300418912510948b60088128209b4004a8912510948b50088128209b400094a8912510948b40088128209b40041094a89125109b3000888128209b50041094a891251b200080988128209b60041098a1248b100820988120901b700418a120948b0008209881201b900418912510948af0082098812bb00418b1251830948aa0082098812bc004a8b1251830948a900091187120abc00094a8f12510948a6001187120a09bc0041094a8f12510948a50088128209bd0041094a9112510948a20088128209be0041094a9112510948a0000888128209bf0041094a93125109489c00080988128209c00041094a93125109489a0008820988120901c30041094a9312518509489200088309881201c50041094a9312518509489000088309118812c9004183094a99125183094884000809118d12ca004183094a99125183094882000809118d120acf0041094aac120a09d00041094aab120901d30041094aa81201d50041094aa61201d90041094aa0120a0901db0041094a9e120a0901e3004185094a8e120a0901e9004185094a8c120a0901f50041860901f90041840901ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00c6000b0f2009160c09180b2408301a3222302c2a2e202d1427"

# pico-8 map format
# first 4096 bytes -> gfx (shared w/ map)
# second 4096 bytes -> map
if len(s)>=8192:
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

