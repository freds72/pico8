pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
function project(v)
 return {63.5-flr(63.5*v[1]/v[3]),63.5+flr(63.5*v[2]/v[3])}
end

local edges={}
local zedges={}
function _update60()
 local p={}
 local a=time()/4
 local c,s=cos(a),-sin(a)
 for i=-4,4 do
   add(p,{i*c+4*s,0,i*s-4*c})   
   add(p,{i*c-4*s,0,i*s+4*c})   
 end
  
 for i,v in pairs(p) do
  v[2]-=4
  v[3]-=10
  edges[i]=project(v)
 end
 
 p={}
 for i=-4,4 do
   add(p,{-4*c-i*s,0,-4*s+i*c})   
   add(p,{4*c-i*s,0,4*s+i*c})   
 end
 for i,v in pairs(p) do
  v[2]-=4
  v[3]-=10
  zedges[i]=project(v)
 end
 
end

function _draw()
 cls()
 
 for y=74,127 do
 	local xmin,zmin
 	local c=0
	 for i=1,#edges,2 do
 	 local p0=edges[i]
	  local x0,y0=p0[1],p0[2]
		 local p1=edges[i+1]
		 local x1,y1=p1[1],p1[2]
	 	--if (y0>y and y1<=y) or (y1>y and y0<=y) then
    local x=x0+(y-y0)*(x1-x0)/(y1-y0)
    if xmin then
     rectfill(xmin,y,x,y,1+c%2)
     c+=1
    end
    xmin=x
   --end
	 	x0,y0=x1,y1
	 end
	end
	local rows=127-74
 memcpy(0x4300,0x6000+74*64,rows*64)
 cls()
	for y=74,127 do
 	local zmin
	 local c=0
	 for i=1,#zedges,2 do
 	 local p0=zedges[i]
	  local x0,y0=p0[1],p0[2]
		 local p1=zedges[i+1]
		 local x1,y1=p1[1],p1[2]
	 	--if (y0>y and y1<=y) or (y1>y and y0<=y) then
    local x=x0+(y-y0)*(x1-x0)/(y1-y0)
    if zmin then
     rectfill(zmin,y,x,y,1+c%2)
     c+=1
    end
    zmin=x
   --end
	 	x0,y0=x1,y1
	 end
 end
 pal(3,8,1)
 pal(6,8,1)
 pal(5,2,1)
 pal(4,2,1)
 local dst=0x6000+74*64
 local src=0x4300
 for i=0,rows*64-1,2 do
  poke4(dst,peek4(dst)+peek4(src))
  dst+=2
  src+=2 
 end
 
 print(stat(1),2,2,7)
end
__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
