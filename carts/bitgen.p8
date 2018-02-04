pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
local src_colors={0x.0066,0x.00dd,0x.006d,0x.00d6}
local dst_colors={0x.00dd,0x.0066,0x.00d6,0x.006d}
local shades={}

function fillbits(src,dst,index)
	if index>4 then
		shades[src]=dst
		print(src)
		return
	end
	local s,d=src,dst
	for i=1,4 do
		local shift=(index-1)*8
		src=bor(s,shl(src_colors[i],shift))
		dst=bor(d,shl(dst_colors[i],shift))
		fillbits(src,dst,index+1)
	end	
end
-- seed
fillbits(0,0,1)

local x,y=0,0
function _update60()
	if(btn(0)) x-=1
	if(btn(1)) x+=1
	if(btn(2)) y-=1
	if(btn(3)) y+=1
	x=mid(x,0,127)
	y=mid(y,0,127)
end

function _draw()
	cls(6)
	circfill(64,64,24,0xd)

	local mem=0x6000
	for j=0,127 do
		for i=0,15 do
			-- poke4(mem,bnot(peek4(mem)))
		 poke4(mem,shades[peek4(mem)])
			mem+=4
		end
	end
	
	local u,v=flr(x/8)*8,y
	local mem=0x6000+u/2+v*64
	local pix=peek4(mem)
	poke4(mem,0x0)
	pset(x,y,7)
	print(pix,u,v+1,7)
	print(shades[pix],u,v+8,8)
end


