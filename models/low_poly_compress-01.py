#python image convert to pico-8
#import matplotlib.image as mpimg
#import numpy as np
import sys
from math import *


## Blender v2.72 (sub 0) OBJ File: 'small_block.blend'
## www.blender.org
#mtllib small_block.mtl
#o Cube
#v 2.560000 0.022037 -2.560000
#v 2.560000 0.022037 2.560000
#v -2.560000 0.022037 2.559999
#v -2.559999 0.022037 -2.560001
#v 4.000002 4.022038 -3.999998
#v 3.999998 4.022038 4.000002
#v -4.000001 4.022038 3.999999
#v -4.000000 4.022038 -4.000000
#v 2.560001 8.022038 -2.559999
#v 2.559999 8.022038 2.560002
#v -2.560001 8.022038 2.559999
#v -2.560000 8.022038 -2.560000
#g Cube_Cube_Material
#usemtl Material
#s off
#f 1 5 6
#f 5 9 10
#f 2 1 6
#f 7 8 4
#f 6 5 10
#f 9 12 10
#f 8 7 12
#f 3 7 4
#f 3 2 7
#f 2 6 7
#f 12 11 10
#f 7 11 12
#f 6 10 11
#f 7 6 11

def tohex(val, nbits):
	return hex((val + (1 << nbits)) % (1 << nbits))

def main():
	input_filename = sys.argv[1]
	
	
	#input_file = open(input_filename,'r')
	
	#print(input_filename.strip(".obj"))
	output_filename=input_filename.strip(".obj")+"_converted.txt"
	
	output_file = open(output_filename,'w')
	
	
	
	#read until the firt vector
	
	with open(input_filename) as f:
		output_file.write('model_v="')
		for line in f:
			if(line[:1]=='v'):
				line=line.strip('v ')
				#print(line)
				#output_file.write('{')
				point_text=line.split(' ')
				for num_text in point_text:
					#print("p: "+num_text)
					val=float(num_text)
					val=int(floor(val*256))
					hex_string=tohex(val, 16)
					hex_string=hex_string[2:]
					hex_string = hex_string.zfill(4)
					output_file.write(hex_string)
				#output_file.write('},\n')
		output_file.write('"\n')
	
	with open(input_filename) as f:	
		output_file.write('model_f="')
		for line in f:
			if(line[:1]=='f'):
				line=line.strip('f ')
				#print(line)
				#output_file.write('{')
				point_text=line.split(' ')
				for num_text in point_text:
					#print("p: "+num_text)
					val=int(num_text)
					val=min(val,255)
					hex_string=tohex(val, 8)
					hex_string=hex_string[2:]
					hex_string = hex_string.zfill(2)
					output_file.write(hex_string)
				#output_file.write('},\n')
		output_file.write('"\n')
		print("\nConversion complete!\nSee output: "+output_filename)



	#input_file.close()
	output_file.close()
	
	
main()
	