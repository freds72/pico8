pico-8 cartridge // http://www.pico-8.com
version 16
__lua__

local time_t=0
local actors={}

function lerp(a,b,t)
	return a*(1-t)+b*t
end

-- dither
local dither_pat={0b1111111111111111,0b0111111111111111,0b0111111111011111,0b0101111111011111,0b0101111101011111,0b0101101101011111,0b0101101101011110,0b0101101001011110,0b0101101001011010,0b0001101001011010,0b0001101001001010,0b0000101001001010,0b0000101000001010,0b0000001000001010,0b0000001000001000,0b0000000000000000}

-- main
local plyr
local qmap,hmap={},{}
function _init()
	local noise={}
	local idx_offsets={
		0,1,128,129
	}
	local q_codes={
		{0,0,0,0},
		{0,0,1,0},
		{0,0,0,2},
		{0,0,5,5},
		{0,4,0,0},
		{2,0,0,8},
		{0,5,0,5},
	 {2,5,5,5},
	 {8,0,0,0},
	 {5,0,5,0},
	 {5,1,4,5},
	 {5,1,5,5},
	 {5,5,0,0},
	 {5,5,5,8},
	 {5,5,4,5},
	 {5,5,5,5}	
	}
	local get_noise=function(i,j)
		return noise[band(i,0x3f)+64*band(j,0x3f)+1]
	end
 -- returns whether value is above a given level
 local is_solid=function(i,j,level)
  return get_noise(i,j)>level and 1 or 0
 end
 -- converts four corners into a single sprite lookup index
 -- cf 'marching square' thingy
 local marching_code=function(i,j,level)
  return
   8*is_solid(i,j,level)+
   4*is_solid(i+1,j,level)+
   2*is_solid(i+1,j+1,level)+
   is_solid(i,j+1,level)
 end
 
 os2d_noise(48)

	--[[
 for y=0,63 do
  for x=0,63 do
   local c
   -- base noise is strongest
   c=os2d_eval(x/4,y/4)
   -- next is weaker
   c+=os2d_eval(x/2,y/2)/2
   -- and so on
   c+=os2d_eval(x,y)

   -- convert -0.2..+1 to 14 cols
   -- (sea level at -0.2)
   c=mid(0,(c+0.2)/1.2*14,13)/14

   -- set in stoooone
   add(noise,c)
  end
 end
	]]
	
 for j=0,63 do
  for i=0,63 do
  	local x,y=i%8,j%8
   add(noise,(x==0 or y==0) and 1 or 0)
  end
 end
 
 -- convert into marching quadrants
	for j=0,63 do
  for i=0,63 do
   local q=marching_code(i,j,0)
   local idx=2*i+2*128*j
  	local code=q_codes[q+1]
  	for k=1,4 do
   	qmap[idx+idx_offsets[k]]=code[k]
  	 local h
  	 if k==1 then
  	 	h=get_noise(i,j)
  	 elseif k==2 then
  	 	h=0.5*get_noise(i,j)+0.5*get_noise(i+1,j)
				elseif k==3 then 	 	
  	 	h=0.5*get_noise(i,j)+0.5*get_noise(i,j+1)
				elseif k==4	then
  	 	h=(get_noise(i,j)+get_noise(i,j+1)+get_noise(i+1,j)+get_noise(i+1,j+1))/4
	   end

  	 hmap[idx+idx_offsets[k]]=h
  	end
  end
 end 

 plyr=make_plyr()
end

-- camera
local cam_x,cam_y,cam_z
local xscale,zscale,hscale=4,2,4

function cam_track(v)
	cam_x,cam_y,cam_z=v[1],v[2],v[3]+4*zscale
end

local cam_focal=48

function project(x,y,z)
	local xe=x-cam_x
	local ye=y-cam_y
	local ze=z-cam_z
	
	local w=-cam_focal/ze
	return 64+xe*w,64-ye*w,ze,w
end

-- map helpers
function get_qcode(i,j)
	return qmap[band(i,0x7f)+128*band(j,0x7f)]
end
function get_height(i,j)
	return hmap[band(i,0x7f)+128*band(j,0x7f)]
end

function get_color(h,major)
	return major and 6 or 1--sget(7*h,0)
end

function get_altitude(x,z)
	-- cell
	local dx,dz=(band(x,0x7f)%xscale)/xscale,(band(z,0x7f)%zscale/zscale)
	local i,j=flr(x/xscale),flr(z/zscale)
	local h0,h1=lerp(get_height(i,j),get_height(i,j+1),1-dz),lerp(get_height(i+1,j),get_height(i+1,j+1),1-dz)
	return lerp(h0,h1,1-dx)
end

function draw_ground(self)
	local dx,dz=cam_x%xscale,cam_z%zscale
	local nx,ny=flr(cam_x/xscale),flr(cam_z/zscale)
	-- project anchor points
	local p={}
 local j,sz,sw=j0,1,0
	
	for i=8,1,-1 do
		local x,y,z,w=project(-dx+cam_x,0,-dz+cam_z-i*zscale)
		add(p,{x,y,z,w})
	end

	local nj=ny
	for j=2,#p do
		local v0,v1=p[j-1],p[j]
		local w0,w1=v0[4],v1[4]
		local dw0,dw1=xscale*w0,xscale*w1
		local x0,x1=v0[1]-4*dw0,v1[1]-4*dw1
		local x2,x3=v1[1]-3*dw1,v0[1]-3*dw0
		local ni=nx
		-- get cell at i,j
		for i=-4,4 do
		 local q=get_qcode(ni,nj)
			local h0,h1,h2,h3=get_height(ni,nj),get_height(ni,nj+1),get_height(ni+1,nj+1),get_height(ni+1,nj)
			local y0=v0[2]-hscale*w0*h0
			local y1=v1[2]-hscale*w1*h1
			local y2=v1[2]-hscale*w1*h2
			local y3=v0[2]-hscale*w0*h3

			if q==1 then
				local c2,c3=get_color(h1,true),get_color(h3)
				trifill(x0,y0,x1,y1,x2,y2,c2)		
				trifill(x0,y0,x3,y3,x2,y2,c3)		
			elseif q==2 then
				local c2,c0=get_color(h2,true),get_color(h0)
				trifill(x3,y3,x2,y2,x1,y1,c2)		
				trifill(x0,y0,x3,y3,x1,y1,c0)		
			elseif q==4 then
				local c3,c1=get_color(h3,true),get_color(h1)
				trifill(x0,y0,x3,y3,x2,y2,c3)		
				trifill(x0,y0,x2,y2,x1,y1,c1)		
			elseif q==8 then
				local c0,c2=get_color(h0,true),get_color(h2)
				trifill(x0,y0,x3,y3,x1,y1,c0)
				trifill(x3,y3,x2,y2,x1,y1,c2)
			else
				local c=get_color(h0,q==5)
				trifill(x0,y0,x1,y1,x3,y3,c)		
				trifill(x3,y3,x2,y2,x1,y1,c)		
			end
			x0,x1=x3,x2
			x2+=dw1
			x3+=dw0
			ni+=1
		end
		nj+=1
	end
end


function make_plyr()
	return add(actors,{
		pos={0,10,2},
		update=update_actor,
		draw=function(self)
			local x,y,z,w=project(self.pos[1],self.pos[2],self.pos[3])
			circfill(x,y,2*w,7)
			local h=get_altitude(self.pos[1],self.pos[3])
			local x1,y1,z1,w1=project(self.pos[1],h,self.pos[3])
			line(x,y,x1,y1,8)
			print(h,x+1,y-4,8)
		end
	})
end

function update_actor(self)
end

function _update60()
	time_t+=1
	local dx,dy=0,0
	if(btn(0)) dx=-0.1
	if(btn(1)) dx=0.1
	if(btn(2)) dy=-0.1
	if(btn(3)) dy=0.1
		
	plyr.pos[1]+=dx
	plyr.pos[btn(4) and 2 or 3]+=dy
	
	--cam_cb,cam_sb=cos(plyr.y/10),sin(plyr.y/10)
	cam_track(plyr.pos)
	
	for _,a in pairs(actors) do
		a:update()
	end
end

function _draw()
	cls(0)
	
	if btn(5) then
		for j=0,127 do
		 for i=0,127 do
		 	pset(i,j,sget(8*hmap[i+128*j],0))
		 end
		end
		local nx,ny=flr(cam_x/xscale),flr(cam_z/zscale)
		if time_t%2==0 then
			circfill(band(nx,0x7f),band(ny,0x7f),2,8)
		end
	else
		draw_ground()
	end

	for _,a in pairs(actors) do
		a:draw()
	end
	
			
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
-- trifill
function p01_trapeze_h(l,r,lt,rt,y0,y1)
 lt,rt=(lt-l)/(y1-y0),(rt-r)/(y1-y0)
 if(y0<0)l,r,y0=l-y0*lt,r-y0*rt,0 
	y1=min(y1,128)
	for y0=y0,y1 do
  rectfill(l,y0,r,y0)
  l+=lt
  r+=rt
 end
end
function p01_trapeze_w(t,b,tt,bt,x0,x1)
 tt,bt=(tt-t)/(x1-x0),(bt-b)/(x1-x0)
 if(x0<0)t,b,x0=t-x0*tt,b-x0*bt,0 
 x1=min(x1,128)
 for x0=x0,x1 do
  rectfill(x0,t,x0,b)
  t+=tt
  b+=bt
 end
end
function trifill(x0,y0,x1,y1,x2,y2,col)
 color(col)
 if(y1<y0)x0,x1,y0,y1=x1,x0,y1,y0
 if(y2<y0)x0,x2,y0,y2=x2,x0,y2,y0
 if(y2<y1)x1,x2,y1,y2=x2,x1,y2,y1
 if max(x2,max(x1,x0))-min(x2,min(x1,x0)) > y2-y0 then
  col=x0+(x2-x0)/(y2-y0)*(y1-y0)
  p01_trapeze_h(x0,x0,x1,col,y0,y1)
  p01_trapeze_h(x1,col,x2,x2,y1,y2)
 else
  if(x1<x0)x0,x1,y0,y1=x1,x0,y1,y0
  if(x2<x0)x0,x2,y0,y2=x2,x0,y2,y0
  if(x2<x1)x1,x2,y1,y2=x2,x1,y2,y1
  col=y0+(y2-y0)/(x2-x0)*(x1-x0)
  p01_trapeze_w(y0,y0,y1,col,x0,x1)
  p01_trapeze_w(y1,col,y2,y2,x1,x2)
 end
end

__gfx__
1ca9b34500000000000000000000000000dddddd0000dddd0000dddd0000ddddddddd000dddd0000dddd0000dddd0000dddddddddddddddddddddddddddddddd
00000000000000000000000000000000000ddddd00000ddd0000dddd000ddddddddd0000dddd0000ddd00000ddddd000dddddddddddddddddddddddddddddddd
00000000000000000000000d000000000000dddd000000dd0000dddd00ddddddddd00000dddd0000dd000000dddddd00dddddddddddddddddddddddddddddddd
0000000000000000000000dd0000000000000ddd0000000d0000dddd0ddddddddd000000dddd0000d0000000ddddddd0dddddddddddddddddddddddddddddddd
00000000d000000000000ddddddddddd000000ddd00000000000ddddddddddddd0000000dddd00000000000ddddddddd00000000ddddddd00ddddddddddddddd
00000000dd0000000000dddddddddddd0000000ddd0000000000dddddddddddd00000000dddd0000000000dddddddddd00000000dddddd0000dddddddddddddd
00000000ddd00000000ddddddddddddd00000000ddd000000000dddddddddddd00000000dddd000000000ddddddddddd00000000ddddd000000ddddddddddddd
00000000dddd000000dddddddddddddd00000000dddd00000000dddddddddddd00000000dddd00000000dddddddddddd00000000dddd00000000dddddddddddd
00009000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00619660066666000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01619665017776000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
05619666017776000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01619665017776000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
05199a66061766000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01195a65061766000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0066a660000700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001a1a46000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0414a944000000097000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0194a9a400000009a000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0199a9a400000119a470000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
019999a400001919a4a5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
04199a4400005419a496000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
05444446000014199495000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000054499496000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000014411495000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000054499496000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000011444995000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000001455940000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0f0f0f0d08000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0f0f0d0800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0f0f090021220000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0f0f0b0131320000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0f0f0f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
