pico-8 cartridge // http://www.pico-8.com
version 16
__lua__

local actors={}

-- main
function smoothstep(t)
	t=mid(t,0,1)
	return t*t*(3-2*t)
end

local plyr
local clouds={}
local cache={}
function _init()
	grayvid()
 
 os2d_noise(5)
 
 plyr=make_plyr()
end

local cam_x,cam_y,cam_z
function cam_track(x,y,z)
	cam_x,cam_y,cam_z=x,y,z
end

local cam_cb,cam_sb=cos(0.64),sin(0.60)
local cam_focal=128
function cam_project(x,y,z)
	local y=y-cam_y
	local z=z-cam_z
	local ze=-(y*cam_cb+z*cam_sb)
	-- invalid projection?
	--if(ze<cam_zfar or ze>=0) return nil,nil,z,nil
	--if(ze<cam_zfar) printh("too far") return nil,nil,z,nil
	if(ze>=0) printh("too close") return nil,nil,z,nil
	
	local w=-cam_focal/ze
	local xe=x-cam_x
	local ye=-y*cam_sb+z*cam_cb
	return 64+xe*w,64-ye*w,ze,w
end

function cross(u,v)
	return 
		u.y * v.z - u.z * v.y,
		u.z * v.x - u.x * v.z,
		u.x * v.y - u.y * v.x
end
function vec(u,v)
	local dx,dy,dz=v.x-u.x,v.y-u.y,v.z-u.z
	local d=dx*dx+dy*dy+dz*dz
	d=sqrt(d)
	return {
		x=dx/d,
		y=dy/d,
		z=dz/d}
end
function shade(v0,v1,v2)
	--if getwinding(v0,v1,v2)>0 then		
 	color(0)
 	--line(v0[1],v0[2],v1[1],v1[2])
  --line(v1[1],v1[2],v2[1],v2[2])
 	aaline(v0[1],v0[2],v1[1],v1[2])
  aaline(v1[1],v1[2],v2[1],v2[2])
 --end
end

function getwinding(v1,v2,v3)
	local a={v2[1]-v1[1],v2[2]-v1[2]}
	local b={v3[1]-v1[1],v3[2]-v1[2]}
	--cross product
	return a[1]*b[2]-a[2]*b[1]
end

function draw_ground(self)
	local v={}
	local scale=4
	local dx,dy=cam_x%scale,cam_y%scale
 local i0,j0=flr(cam_x/scale),flr(cam_y/scale)
	
	local j,sz,sw=j0,1,0
	for jj=-32,-1,scale do
		local cy=(j%128+128)%128
		local i=i0
		sw=0
		for ii=-16,16,scale do
			local cx=(i%128+128)%128
			local f=2*os2d_eval(cx,cy)
			local wx,wy=ii-dx+cam_x,jj-dy+cam_y
			local x,y,z,w=cam_project(wx,wy,f)
			add(v,{x,y,z,w,x=wx,y=wy,z=f})
			i+=1
			sw+=1
		end
		j+=1
		sz+=1
	end
	
	-- strip size
	for j=0,sz-3 do
		for i=0,sw-2 do
			local k=i+sz*j+1
			shade(
				v[k],v[k+1],v[k+sz+1])
		end
	end
	print("h:"..sz.."w:"..sw,2,12,7)
end

function make_plyr()
	return add(actors,{
		x=0,y=0,z=5,
		angle=0,
		dy=0,
		update=update_actor})
end

function update_actor(self)
end

local time_t=0
function _update60()
	time_t+=1
	if(btn(0)) plyr.x-=0.1 plyr.angle-=0.01
	if(btn(1)) plyr.x+=0.1 plyr.angle+=0.01
	if(btn(2)) plyr.y-=0.1
	if(btn(3)) plyr.y+=0.1
	
	--cam_cb,cam_sb=cos(plyr.y/10),sin(plyr.y/10)
	cam_track(plyr.x-8,plyr.y,-12)
	
	for _,a in pairs(actors) do
		a:update()
	end
end

function _draw()
	cls(14)
	draw_ground({})
	
 rectfill(0,0,127,8,1)
 print("mem:"..stat(0).." cpu:"..stat(1).."("..stat(7)..")",2,2,7)
end

-->8
-- opensimplex noise

-- adapted from public-domain
-- code found here:
-- https://gist.github.com/kdotjpg/b1270127455a94ac5d19

--------------------------------

-- opensimplex noise in java.
-- by kurt spencer
-- 
-- v1.1 (october 5, 2014)
-- - added 2d and 4d implementations.
-- - proper gradient sets for all dimensions, from a
--   dimensionally-generalizable scheme with an actual
--   rhyme and reason behind it.
-- - removed default permutation array in favor of
--   default seed.
-- - changed seed-based constructor to be independent
--   of any particular randomization library, so results
--   will be the same when ported to other languages.

-- (1/sqrt(2+1)-1)/2
local _os2d_str=-0.211324865405187
-- (  sqrt(2+1)-1)/2
local _os2d_squ= 0.366025403784439

-- cache some constant invariant
-- expressions that were 
-- probably getting folded by 
-- kurt's compiler, but not in 
-- the pico-8 lua interpreter.
local _os2d_squ_pl1=_os2d_squ+1
local _os2d_squ_tm2=_os2d_squ*2
local _os2d_squ_tm2_pl1=_os2d_squ_tm2+1
local _os2d_squ_tm2_pl2=_os2d_squ_tm2+2

local _os2d_nrm=47

local _os2d_prm={}

-- gradients for 2d. they 
-- approximate the directions to
-- the vertices of an octagon 
-- from the center
local _os2d_grd = 
{[0]=
	 5, 2,  2, 5,
	-5, 2, -2, 5,
	 5,-2,  2,-5,
	-5,-2, -2,-5,
}

-- initializes generator using a 
-- permutation array generated 
-- from a random seed.
-- note: generates a proper 
-- permutation, rather than 
-- performing n pair swaps on a 
-- base array.
function os2d_noise(seed)
	local src={}
	for i=0,255 do
		src[i]=i
		_os2d_prm[i]=0
	end
	srand(seed)
	for i=255,0,-1 do
		local r=flr(rnd(i+1))
		_os2d_prm[i]=src[r]
		src[r]=src[i]
	end
end

-- 2d opensimplex noise.
function os2d_eval(x,y)
	-- put input coords on grid
	local sto=(x+y)*_os2d_str
	local xs=x+sto
	local ys=y+sto
	
	-- flr to get grid 
	-- coordinates of rhombus
	-- (stretched square) super-
	-- cell origin.
	local xsb=flr(xs)
	local ysb=flr(ys)
	
	-- skew out to get actual 
	-- coords of rhombus origin.
	-- we'll need these later.
	local sqo=(xsb+ysb)*_os2d_squ
	local xb=xsb+sqo
	local yb=ysb+sqo

	-- compute grid coords rel.
	-- to rhombus origin.
	local xins=xs-xsb
	local yins=ys-ysb

	-- sum those together to get
	-- a value that determines 
	-- which region we're in.
	local insum=xins+yins

	-- positions relative to 
	-- origin point.
	local dx0=x-xb
	local dy0=y-yb
	
	-- we'll be defining these 
	-- inside the next block and
	-- using them afterwards.
	local dx_ext,dy_ext,xsv_ext,ysv_ext

	local val=0

	-- contribution (1,0)
	local dx1=dx0-_os2d_squ_pl1
	local dy1=dy0-_os2d_squ
	local at1=2-dx1*dx1-dy1*dy1
	if at1>0 then
		at1*=at1
		local i=band(_os2d_prm[(_os2d_prm[(xsb+1)%256]+ysb)%256],0x0e)
		val+=at1*at1*(_os2d_grd[i]*dx1+_os2d_grd[i+1]*dy1)
	end

	-- contribution (0,1)
	local dx2=dx0-_os2d_squ
	local dy2=dy0-_os2d_squ_pl1
	local at2=2-dx2*dx2-dy2*dy2
	if at2>0 then
		at2*=at2
		local i=band(_os2d_prm[(_os2d_prm[xsb%256]+ysb+1)%256],0x0e)
		val+=at2*at2*(_os2d_grd[i]*dx2+_os2d_grd[i+1]*dy2)
	end
	
	if insum<=1 then
		-- we're inside the triangle
		-- (2-simplex) at (0,0)
		local zins=1-insum
		if zins>xins or zins>yins then
			-- (0,0) is one of the 
			-- closest two triangular
			-- vertices
			if xins>yins then
				xsv_ext=xsb+1
				ysv_ext=ysb-1
				dx_ext=dx0-1
				dy_ext=dy0+1
			else
				xsv_ext=xsb-1
				ysv_ext=ysb+1
				dx_ext=dx0+1
				dy_ext=dy0-1
			end
		else
			-- (1,0) and (0,1) are the
			-- closest two vertices.
			xsv_ext=xsb+1
			ysv_ext=ysb+1
			dx_ext=dx0-_os2d_squ_tm2_pl1
			dy_ext=dy0-_os2d_squ_tm2_pl1
		end
	else  //we're inside the triangle (2-simplex) at (1,1)
		local zins = 2-insum
		if zins<xins or zins<yins then
			-- (0,0) is one of the 
			-- closest two triangular
			-- vertices
			if xins>yins then
				xsv_ext=xsb+2
				ysv_ext=ysb
				dx_ext=dx0-_os2d_squ_tm2_pl2
				dy_ext=dy0-_os2d_squ_tm2
			else
				xsv_ext=xsb
				ysv_ext=ysb+2
				dx_ext=dx0-_os2d_squ_tm2
				dy_ext=dy0-_os2d_squ_tm2_pl2
			end
		else
			-- (1,0) and (0,1) are the
			-- closest two vertices.
			dx_ext=dx0
			dy_ext=dy0
			xsv_ext=xsb
			ysv_ext=ysb
		end
		xsb+=1
		ysb+=1
		dx0=dx0-_os2d_squ_tm2_pl1
		dy0=dy0-_os2d_squ_tm2_pl1
	end
	
	-- contribution (0,0) or (1,1)
	local at0=2-dx0*dx0-dy0*dy0
	if at0>0 then
		at0*=at0
		local i=band(_os2d_prm[(_os2d_prm[xsb%256]+ysb)%256],0x0e)
		val+=at0*at0*(_os2d_grd[i]*dx0+_os2d_grd[i+1]*dy0)
	end
	
	-- extra vertex
	local atx=2-dx_ext*dx_ext-dy_ext*dy_ext
	if atx>0 then
		atx*=atx
		local i=band(_os2d_prm[(_os2d_prm[xsv_ext%256]+ysv_ext)%256],0x0e)
		val+=atx*atx*(_os2d_grd[i]*dx_ext+_os2d_grd[i+1]*dy_ext)
	end
	return val/_os2d_nrm
end
-->8
-- aaline
local ramp={[0]=7,7,7,7,7,6,6,6,13,13,13,5,5,1,1,0}
function grayvid()
	for i=0,15 do
		pal(i,i,0)
		pal(i,ramp[i],1)
	 palt(i,false)
	end
end
function normvid()
	for i=0,15 do
		pal(i,i,0)
		pal(i,i,1)
		palt(i,false)
	end
end
function aaline(x0,y0,x1,y1)
	local w,h=abs(x1-x0),abs(y1-y0)
	
	-- to calculate dist properly,
	-- do this, but we'll use an
	-- approximation below instead.
 -- local d=sqrt(w*w+h*h)
 
 if h>w then
 	-- order points on y
 	if y0>y1 then
 		x0,y0,x1,y1=x1,y1,x0,y0
 	end
 
 	local dx=x1-x0
 	
 	-- apply the bias to the 
 	-- line's endpoints:
 	y0+=0.5
 	y1+=0.5
 
 	--x0+=0.5 --nixed by -0.5 in loop
 	--x1+=0.5 --don't need x1 anymore

		-- account for diagonal thickness
		-- thanks to freds72 for neat trick from https://oroboro.com/fast-approximate-distance/
  -- 	local k=h/d
		local k=h/(h*0.9609+w*0.3984)
		 	
 	for y=flr(y0)+0.5-y0,flr(y1)+0.5-y0 do	
 		local x=x0+dx*y/h
 		-- originally flr(x-0.5)+0.5
 		-- but now we don't x0+=0.5 so not needed
 		local px=flr(x)
 		pset(px,  y0+y,pget(px,  y0+y)*k*(x-px  ))
 		pset(px+1,y0+y,pget(px+1,y0+y)*k*(px-x+1))
 	end
 elseif w>0 then
 	-- order points on x
 	if x0>x1 then
 		x0,y0,x1,y1=x1,y1,x0,y0
 	end
 
 	local dy=y1-y0
 	
 	-- apply the bias to the 
 	-- line's endpoints:
 	x0+=0.5
 	x1+=0.5
 
 	--y0+=0.5 --nixed by -0.5 in loop
 	--y1+=0.5 --don't need y1 anymore
	
		-- account for diagonal thickness
		-- thanks to freds72 for neat trick from https://oroboro.com/fast-approximate-distance/
  -- local k=w/d
		local k=w/(w*0.9609+h*0.3984)

 	for x=flr(x0)+0.5-x0,flr(x1)+0.5-x0 do	
 		local y=y0+dy*x/w
 		-- originally flr(y-0.5)+0.5
 		-- but now we don't y0+=0.5 so not needed
 		local py=flr(y)
 		pset(x0+x,py,  pget(x0+x,py  )*k*(y-py  ))
 		pset(x0+x,py+1,pget(x0+x,py+1)*k*(py-y+1))
 	end
	end
end
__gfx__
0000000000000000000000000000000000dddddd0000dddd0000dddd0000ddddddddd000dddd0000dddd0000dddd0000dddddddddddddddddddddddddddddddd
00000000000000000000000000000000000ddddd00000ddd0000dddd000ddddddddd0000dddd0000ddd00000ddddd000dddddddddddddddddddddddddddddddd
00000000d00000000000000d000000000000dddd000000dd0000dddd00ddddddddd00000dddd0000dd000000dddddd00dddddddddddddddddddddddddddddddd
00000000dd000000000000dd0000000000000ddd0000000d0000dddd0ddddddddd000000dddd0000d0000000ddddddd0dddddddddddddddddddddddddddddddd
00000000ddd0000000000ddddddddddd000000ddd00000000000ddddddddddddd0000000dddd00000000000ddddddddd00000000ddddddd00ddddddddddddddd
00000000dddd00000000dddddddddddd0000000ddd0000000000dddddddddddd00000000dddd0000000000dddddddddd00000000dddddd0000dddddddddddddd
00000000ddddd000000ddddddddddddd00000000ddd000000000dddddddddddd00000000dddd000000000ddddddddddd00000000ddddd000000ddddddddddddd
00000000dddddd0000dddddddddddddd00000000dddd00000000dddddddddddd00000000dddd00000000dddddddddddd00000000dddd00000000dddddddddddd
