pico-8 cartridge // http://www.pico-8.com
version 18
__lua__

function normalize(u,v,scale)
	scale=scale or 1
	local d=sqrt(u*u+v*v)
	if (d>0) u/=d v/=d
	return u*scale,v*scale
end

function shear(sx,sy,x,y,a,w)
	a=(a%1+1)%1
	local sa=sin(a)
 local hw=w/2
 local t=sin(a/2)/cos(a/2)
 w-=1
 
 -- horiz shear
 for iy=-hw,hw-1 do
  sspr(sx,sy+iy+hw,w,1,-iy*t,hw+iy)
 end
 --if(true) return
 -- copy to spritesheet
 for i=0,w do
	 memcpy(0x0+64*i,0x6000+64*i,hw)
	end
	-- vert shear
 for ix=0,w do
 	sspr(ix,0,1,w,ix,(ix-hw)*sa)
 end 
 -- copy to spritesheet
 for i=0,w do
	 memcpy(0x0+64*i,0x6000+64*i,w/2)
	end
	-- horiz shear
 for iy=0,w do
  sspr(0,iy,w,1,x-(iy-hw)*t,y+(iy-hw))
 end	
end

function rspr(sx,sy,x,y,a,w)
	local ca,sa=cos(a),sin(a)
 local ddx0,ddy0=ca,sa
 local mask=shl(0xfff8,(w-1))
 w*=4	
 ca*=w-0.5
 sa*=w-0.5 
 local dx0,dy0=sa-ca+w,-ca-sa+w
 w=2*w-1
 for ix=0,w do
  local srcx,srcy=dx0,dy0
  for iy=0,w do
   if band(bor(srcx,srcy),mask)==0 then
   	local c=sget(sx+srcx,sy+srcy)
   	if c!=14 then
   		pset(x+ix,y+iy,c)
  		end
  	end
   srcx-=ddy0
  	srcy+=ddx0
  end
  dx0+=ddx0
  dy0+=ddy0
 end
end

local time_t=0
local plyr={
	x=0,y=0,
	dx=0,dy=0,
	acc=1,
	angle=0,
	da=0,
	sx=64,
	sy=16
}

local parts={}
function make_part(x,y)
	add(parts,{
		x=x,y=y,
		t=time_t+rnd(30)
	})
end

function update_parts()
	for _,p in pairs(parts) do
		if p.t<time_t then
			del(parts,p)
		end
	end	
end

function draw_parts()
	for i=1,#parts do
		local p0=parts[i]
		pset(64+p0.x,64+p0.y,7)
	end
end

function _update60()
	time_t+=1

	if(btn(2)) plyr.da=0.01
	if(btn(3)) plyr.da=-0.01
	
	plyr.angle+=plyr.da
	plyr.da*=0.96
	
end

function _draw()
	cls(6)
	palt(14,true)
	palt(0,false)
	
 shear(plyr.sx,plyr.sy,64,64,-plyr.angle,16)	

 --rspr(plyr.sx,plyr.sy,64,64,-plyr.angle,4)	
	
	--spr(36,64,64,2,2)
	
	rectfill(0,120,127,127,1)
	print(stat(1),2,121,7)
end

__gfx__
00000000eeeeeeeeee0000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00000000ee88eeeee0999a0eee0000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00700700e000000e091414a0ee0650eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00077000e088777009444490ee0650eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddddeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00077000e055667004555440ee0990eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeedddddddddddeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00700700e000000e0339bbb0ee0440eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddddddeddddddddeeeeeeeeee00000000000000000000000000000000
00000000ee88eeee003000b0eee00eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddddddddddddddddeeeeeeeee00000000000000000000000000000000
00000000eeeeeeeeee0eee0eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddddddddddddddddeeddddeee00000000000000000000000000000000
00000000eeeeeeee0000000000000000e00eeeeeeeeeeeeeeeeeeeeeeeeeeeeeedddddddddddddddddddddddddddddee00000000000000000000000000000000
00000000eeeeeeee00000000000000000660eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddddddddddddddddddddddee00000000000000000000000000000000
00000000ee00000e00000000000000000bb60eeee00000000eeeeeeeeeeeeeeeeedddddddddddddddddddddddddddddd00000000000000000000000000000000
00000000e049550e00000000000000000bbb60ee0ccccc77700eeeeeeeeeeeeeeeeddddddddddddddddddddddddddddd00000000000000000000000000000000
00000000e049660e00000000000000000bbbb6003cccccccccc00eeeeeeeeeeeeeeeeddddddddddedddddddddddddddd00000000000000000000000000000000
00000000ee00000e00000000000000000b555000000000000cccc000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00000000eeeeeeee00000000000000000500066666666666600116660eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00000000eeeeeeee0000000000000000e0dd00000bbbbbbbb6600dddd0eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00000000000000000000000000000000090d07777000bbbbbbb6600000eeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000090d06666000bbbbbbb6600000000eeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
0000000000000000000000000000000009006bbbb6660000555bbbbb3bb3b0eeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
000000000000000000000000000000000906bbbbbbbb66660005bb33a33a3b0eeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000090b7b77b7bbbbbbbbb05b33a93a930eeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
0000000000000000000000000000000009056366363bbbbb333051333933930eeeeee88eeeeeeeee000000000000000000000000000000000000000000000000
000000000000000000000000000000000901333333331111000111111111110eeeee000000eeeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000e00013333111000055101d00000000eeeeee0887770eeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeee011110000000000000eeeeeeeeeeeeee0556670eeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeee0000eeeeeeeeeeeeeeeeeeeeeeeeeee000000eeeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeee0000eeeeeeeeeeeeeeeeeeeeeeeeeeee88eeeeeeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
