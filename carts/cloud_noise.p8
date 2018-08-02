pico-8 cartridge // http://www.pico-8.com
version 16
__lua__


-- main
local cache_hi,cache_lo,cache_stuff={},{},{}
function _init()
  local noise={}
  -- returns whether value is above a given level
  local is_solid=function(i,j,level)
    return noise[band(i,0x7f)+128*band(j,0x7f)+1]>level and 1 or 0
  end
  -- converts four corners into a single sprite lookup index
  -- cf 'marching square' thingy
  local marching_code=function(i,j,level)
    return
    is_solid(i,j,level)+
    shl(is_solid(i+1,j,level),1)+
    shl(is_solid(i,j+1,level),2)+
    shl(is_solid(i+1,j+1,level),3)
  end

  os2d_noise(48)

  -- hack: what we're about to 
  -- do is *very* slow, so let's
  -- warn the user...
  cls(0)
  rectfill(39,59,87,65,1)
  print("computing...",40,60,7)
  flip()
    
  -- fill spr ram with samples
  for y=0,127 do
    for x=0,127 do
      local c
      -- base noise is strongest
      c=os2d_eval(x/32,y/32)
      -- next is weaker
      c+=os2d_eval(x/16,y/16)/2
      -- and so on
      c+=os2d_eval(x/ 8,y/ 8)/4
      -- and so on
      c+=os2d_eval(x/ 4,y/ 4)/8
      -- and so on
      c+=os2d_eval(x/ 2,y/ 2)/16

      -- convert -0.2..+1 to 14 cols
      -- (sea level at -0.2)
      --c=mid(0,(c+0.2)/1.2*14,13)

      -- set in stoooone
      add(noise,c)
    end
  end

 -- note: need full noise map
 -- note: noise map can be discarded after (unless game logic needs it...)
 -- compute marching square caches
	for x=0,127 do
    for y=0,127 do
      -- "shore" layer
      add(cache_lo,marching_code(x,y,0.4))
      -- grass layer
      add(cache_hi,marching_code(x,y,0.5))
      -- terrain feature layer
      add(cache_stuff,marching_code(x,y,0.8))
    end
 end
end

-- camera pos
local x,y=0,0
function _update60()
	if(btn(0)) x-=1
	if(btn(1)) x+=1
	if(btn(2)) y-=1
	if(btn(3)) y+=1
end

function _draw()
	cls(12)
	draw_noise(x,y)

	rectfill(0,0,127,8,1)
 print("mem:"..stat(0).." cpu:"..stat(1).."("..stat(7)..")",2,2,7)
end

-- distance between point samples
local scale=8
function draw_noise(x,y)
 local dx,dy=x%scale,y%scale
 local i0,j0=flr(x/scale),flr(y/scale)
	local i=i0
 for x0=-dx-scale,127+scale,scale do
 	local j=j0
 	for y0=-dy-scale,127+scale,scale do
 		-- get rolling index
 		local idx=band(i,0x7f)+128*band(j,0x7f)+1
			local s_lo,s_hi,s_stuff=cache_lo[idx],cache_hi[idx],cache_stuff[idx]
			-- don't draw if hidden by layer above
			if(s_stuff<15 and s_hi<15 and s_lo!=0) spr(s_lo,x0,y0)
			if(s_stuff<15 and s_hi!=0) spr(s_hi+16,x0,y0)
			if(s_stuff!=0) spr(s_stuff+32,x0,y0)
			j+=1
		end
		i+=1
 end
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
	else  --we're inside the triangle (2-simplex) at (1,1)
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

-- note kurt's original code had
-- an extrapolate() function
-- here, which was called in 
-- four places in eval(), but i
-- found inlining it to produce
-- good performance benefits.

__gfx__
000000009940000000000000999a00000000049999999999000049999999999900000000999a000000000000999a000000004999999999990000a99999999999
000000009400000000000000999a00000000004999999999000049999999999900000000999a0000000000009999a0000000499999999999000a999999999999
000000004000000000000000999a00000000000499999999000049999999999900000000999a00000000000099999a00000049999999999900a9999999999999
000000000000000000000000999a00000000000044444444000049999999999400000000999a000000000000999999a000004999499999990a99999999999999
000000000000000000000000999a00000000000000000000000049999999994000000000999a0000aaaaaaaa9999999a0000499904999999a999999999999999
0000000000000000a0000000999a0000000000000000000000004999999994000000000a999a0000999999999999999900004999004999999999999999999999
00000000000000009a000000999a000000000000000000000000499999994000000000a9999a0000999999999999999900004999000499999999999999999999
000000000000000099a00000999a00000000000000000000000049999994000000000a99999a0000999999999999999900004999000049999999999999999999
000000003310000000000000333b00000000013333333333000013333333333300000000333b000000000000333b000000001333333333330000b33333333333
000000003100000000000000333b00000000001333333333000013333333333300000000333b0000000000003333b0000000133333333333000b333333333333
000000001000000000000000333b00000000000133333333000013333333333300000000333b00000000000033333b00000013333333333300b3333333333333
000000000000000000000000333b00000000000011111111000013333333333100000000333b000000000000333333b000001333133333330b33333333333333
000000000000000000000000333b00000000000000000000000013333333331000000000333b0000bbbbbbbb3333333b0000133301333333b333333333333333
0000000000000000b0000000333b0000000000000000000000001333333331000000000b333b0000333333333333333300001333001333333333333333333333
00000000000000003b000000333b000000000000000000000000133333331000000000b3333b0000333333333333333300001333000133333333333333333333
000000000000000033b00000333b00000000000000000000000013333331000000000b33333b0000333333333333333300001333000013333333333333333333
000000002b220000000000002b220000000222b22b2222220002b2222222b220000000002b220000000000002b2000000000022222b22222000000222b2222b2
00000000b3b2000000000000b3b2000000022b3bb3b22b22002b3b22222b3b2000000000b3b2000000000000b3b20000000002b22b3b2222000002b2b3b22b3b
00000000333200002200000033320000000023333332b3b20023332222233320000000223332000000000b003332220000002b3b233322b200202b3b33322333
00000000242000002b20000024b220000000024224223332000242b22b224200000002b224b220002b22b3b024222b200000233302422b3b02b22333242b2242
0000000021200000b3b200002b3b2000000002122122242000021b3bb3b2120000002b3b2b3b2000b3b233322122b3b200002242021223332b3b224221b3b212
00000000220000003332000023332000000002220222212000002333333220000000233323332000333224222222333200000212002022422333221222333222
00000000000000002422000022420000000000020000222000002242242200000002224222422000242221222222242200000222000002122242222222242222
00000000000000002122000022120000000000000000000000002212212000000022221222120000212222222222212200000222000000222212222222212222
__map__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000080200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000080e0b02000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000040d0701000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000040100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
