pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
local sprr

function _init()
 local prec=256
 sprr={
  make_rspr(16,0,prec,0),
  make_rspr(32,0,prec,0),
  make_rspr(48,0,prec,0),
  make_rspr(64,0,prec,0)
 }
end

local corner
function _update()
 for _,s in pairs(sprr) do
  corner=s(time()/8)
 end
end

function _draw()
 cls()
 
 local angle=time()/8
 local r=10
 --local dx,dy=corner[1],corner[2]
 local dx,dy=cos(angle),-sin(angle)
 local x0,y0=0,0
 for i=-7.5,7.5 do 
	 x0=-i*r*dy
	 y0=i*r*dx
	 for j=-7.5,7.5 do
		 spr(2+2*(flr(i+j)%4),63.5+x0+j*dx*r-8,63.5-y0-j*dy*r-8,2,2)
  end
 end
 
 -- rspr(0,0,time()/8,2)
 
 print(stat(1).." "..stat(0),2,2,2)
end
-->8
-- rotation cache builder
-- returns a function that copies rotated version in place
-- n: number of cache entries
-- tc: transparent color
function make_rspr(sx,sy,n,tc)

	-- build up cache per angle
	local angles,corners={},{}
	for i=0,n-1 do
		local a=i/n
		local ca,sa=cos(a),sin(a)
		local ddx0,ddy0=ca,sa
		local dx0,dy0=(sa-ca)*7.5+8,-(ca+sa)*7.5+8
		
		local srcx,srcy

  --
	 corners[i]={ddy0,ddx0}

		-- if not provided, transparent color is top/left sprite pixel
		tc=tc or sget(sx,sy)
		local function ssget(dx,dy)
			if band(bor(dx,dy),0xfff0)==0 then
				return sget(sx+dx,sy+dy) 
			end
			return tc
		end

		local cache={}
		-- target=sprite initial location!
		-- must be on 4 byte boundary
		assert(sx%16==0,"sprite x must be a multiple of 16:"..sx)
		-- sprite sheet memory location
		local mem=64*8*flr(sy/8)+flr(sx%128)/2
		for iy=0,15 do
			srcx,srcy=dy0+iy*ddy0,dx0+iy*ddx0
			
			cache[mem]=
				bor(
					bor(
						bor(shr(ssget(srcx,srcy),16),shr(ssget(srcx+ddx0,srcy-ddy0),12)),
						bor(shr(ssget(srcx+2*ddx0,srcy-2*ddy0),8),shr(ssget(srcx+3*ddx0,srcy-3*ddy0),4))
					),
					bor(
						bor(ssget(srcx+4*ddx0,srcy-4*ddy0),shl(ssget(srcx+5*ddx0,srcy-5*ddy0),4)),
						bor(shl(ssget(srcx+6*ddx0,srcy-6*ddy0),8),shl(ssget(srcx+7*ddx0,srcy-7*ddy0),12))
					)
				)
			
			cache[mem+4]=
				bor(
					bor(
						bor(shr(ssget(srcx+8*ddx0,srcy-8*ddy0),16),shr(ssget(srcx+9*ddx0,srcy-9*ddy0),12)),
						bor(shr(ssget(srcx+10*ddx0,srcy-10*ddy0),8),shr(ssget(srcx+11*ddx0,srcy-11*ddy0),4))
					),
					bor(
						bor(ssget(srcx+12*ddx0,srcy-12*ddy0),shl(ssget(srcx+13*ddx0,srcy-13*ddy0),4)),
						bor(shl(ssget(srcx+14*ddx0,srcy-14*ddy0),8),shl(ssget(srcx+15*ddx0,srcy-15*ddy0),12))
					)
				)
		
			-- one line down
			mem+=64  
		end
		angles[i]=cache
	end

	-- update spritesheet with rotated version
	return function(angle)
		angle=flr(n*((angle%1+1)%1))
		for k,v in pairs(angles[angle]) do
			poke4(k,v)
		end
		return corners[angle]
	end
end

-->8
function rspr(sx,sy,a,w)
	local srcx,srcy
	local ca,sa=cos(angle),-sin(angle)
 local ddx0,ddy0=ca,sa
 local mask=shl(0xfff8,(w-1))
 w*=4
 ca*=w-0.5
 sa*=w-0.5
 local dx0,dy0=sa-ca+w,-ca-sa+w
 w=2*w-1
 for ix=0,w do
  srcx,srcy=dx0,dy0
  for iy=0,w do
   if band(bor(srcx,srcy),mask)==0 then
   	local c=sget(sx+srcx,sy+srcy)
   	pset(ix,iy,7)
  	end
   srcx-=ddy0
  	srcy+=ddx0
  end
  dx0+=ddx0
  dy0+=ddy0
 end
end

__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00222222222222000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
002888888888820000088888888880000009999999999000000cccccccccc0000005555555555000000000000000000000000000000000000000000000000000
002800000000820000080000000080000009999999999000000cccccccccc0000005555555555000000000000000000000000000000000000000000000000000
002800000000820000080000000080000009998888999000000cceeeecccc0000005556555555000000000000000000000000000000000000000000000000000
002800700700820000080070070080000009998998999000000cceccecccc0000005556555555000000000000000000000000000000000000000000000000000
002800077000820000080007700080000009999998999000000cccceecccc0000005556555555000000000000000000000000000000000000000000000000000
002800077000820000080007700080000009999889999000000cccccecccc0000005556565555000000000000000000000000000000000000000000000000000
002800700700820000080070070080000009998999999000000cceccecccc0000005556666555000000000000000000000000000000000000000000000000000
002800000000820000080000000080000009998888999000000cceeeecccc0000005555565555000000000000000000000000000000000000000000000000000
002800000000820000080000000080000009999999999000000cccccccccc0000005555555555000000000000000000000000000000000000000000000000000
002888888888820000088888888880000009999999999000000cccccccccc0000005555555555000000000000000000000000000000000000000000000000000
00222222222222000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
