pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- simplex noise example
-- by anthony digirolamo

local perms = {
   151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225,
   140, 36, 103, 30, 69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148,
   247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32,
   57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68,   175,
   74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83, 111,   229, 122,
   60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54,
   65, 25, 63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169,
   200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186, 3, 64,
   52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212,
   207, 206, 59, 227, 47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213,
   119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9,
   129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104,
   218, 246, 97, 228, 251, 34, 242, 193, 238, 210, 144, 12, 191, 179, 162, 241,
   81,   51, 145, 235, 249, 14, 239,   107, 49, 192, 214, 31, 181, 199, 106, 157,
   184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93,
   222, 114, 67, 29, 24, 72, 243, 141, 128, 195, 78, 66, 215, 61, 156, 180
}

-- make perms 0 indexed
for i = 0, 255 do
   perms[i]=perms[i+1]
end
-- perms[256]=nil

-- the above, mod 12 for each element --
local perms12 = {}

for i = 0, 255 do
   local x = perms[i] % 12
   perms[i + 256], perms12[i], perms12[i + 256] = perms[i], x, x
end

-- gradients for 2d, 3d case --
local grads3 = {
   { 1, 1, 0 }, { -1, 1, 0 }, { 1, -1, 0 }, { -1, -1, 0 },
   { 1, 0, 1 }, { -1, 0, 1 }, { 1, 0, -1 }, { -1, 0, -1 },
   { 0, 1, 1 }, { 0, -1, 1 }, { 0, 1, -1 }, { 0, -1, -1 }
}

for row in all(grads3) do
   for i=0,2 do
      row[i]=row[i+1]
   end
   -- row[3]=nil
end

for i=0,11 do
   grads3[i]=grads3[i+1]
end
-- grads3[12]=nil

function getn2d (bx, by, x, y)
   local t = .5 - x * x - y * y
   local index = perms12[bx + perms[by]]
   return max(0, (t * t) * (t * t)) * (grads3[index][0] * x + grads3[index][1] * y)
end

---
-- @param x
-- @param y
-- @return noise value in the range [-1, +1]
function simplex2d (x, y)
   -- 2d skew factors:
   -- f = (math.sqrt(3) - 1) / 2
   -- g = (3 - math.sqrt(3)) / 6
   -- g2 = 2 * g - 1
   -- skew the input space to determine which simplex cell we are in.
   local s = (x + y) * 0.366025403 -- f
   local ix, iy = flr(x + s), flr(y + s)
   -- unskew the cell origin back to (x, y) space.
   local t = (ix + iy) * 0.211324865 -- g
   local x0 = x + t - ix
   local y0 = y + t - iy
   -- calculate the contribution from the two fixed corners.
   -- a step of (1,0) in (i,j) means a step of (1-g,-g) in (x,y), and
   -- a step of (0,1) in (i,j) means a step of (-g,1-g) in (x,y).
   ix, iy = band(ix, 255), band(iy, 255)
   local n0 = getn2d(ix, iy, x0, y0)
   local n2 = getn2d(ix + 1, iy + 1, x0 - 0.577350270, y0 - 0.577350270) -- g2
   -- determine other corner based on simplex (equilateral triangle) we are in:
   -- if x0 > y0 then
   --    ix, x1 = ix + 1, x1 - 1
   -- else
   --    iy, y1 = iy + 1, y1 - 1
   -- end
   -- local xi = shr(flr(y0 - x0), 31) -- x0 >= y0
   local xi = 0
   if x0 >= y0 then xi = 1 end
   local n1 = getn2d(ix + xi, iy + (1 - xi), x0 + 0.211324865 - xi, y0 - 0.788675135 + xi) -- x0 + g - xi, y0 + g - (1 - xi)
   -- add contributions from each corner to get the final noise value.
   -- the result is scaled to return values in the interval [-1,1].
   return 70 * (n0 + n1 + n2)
end

-- main

function smoothstep(t)
	t=mid(t,0,1)
	return t*t*(3-2*t)
end


local isdirty=true
local x,y=0,0
local clouds={}
local cache={}
function _init()
  local noisedx = rnd(32)
  local noisedy = rnd(32)
  for x=0,127 do
    for y=0,127 do
    	--[[
      local octaves = 3
      local freq = .007
      local max_amp = 0
      local amp = 1
      local value = 0
      local persistance = .65
      for n=1,octaves do
      	value+=simplex2d(noisedx + freq * x,noisedy + freq * y)
        max_amp+=amp
        amp*=persistance
        freq *= 2
      end
      value/=max_amp
      value=mid(value+1,0,2)/2
      ]]
      local x0,y0=x-8,y-8
      local value=(x0*x0+y0*y0)<64 and 1 or 0
      add(clouds,value)
    end
  end
  
	for x=0,127 do
  	for y=0,127 do
  	add(cache,flags(x,y))
  	end
 	end
end

function _update60()
	if(btn(0)) x-=1 isdirty=true
	if(btn(1)) x+=1 isdirty=true
	if(btn(2)) y-=1 isdirty=true
	if(btn(3)) y+=1 isdirty=true
end

--local dither={0,1,5,6,7}
local dither={
  0b1111111111111111,
  0b0111111111111111,
  0b0111111111011111,
  0b0101111111011111,
  0b0101111101011111,
  0b0101101101011111,
  0b0101101101011110,
  0b0101101001011110,
  0b0101101001011010,
  0b0001101001011010,
  0b0001101001001010,
  0b0000101001001010,
  0b0000101000001010,
  0b0000001000001010,
  0b0000001000001000,
  0b0000000000000000
}
function solid(i,j)
	-- wrap around
	i%=128
	j%=128
	if(i<0) i+=128
	if(j<0) j+=128
	return clouds[i+128*j+1]>0.7 and 1 or 0
end

function flags(i,j)
	return
		solid(i,j)+
		shl(solid(i+1,j),1)+
		shl(solid(i+1,j+1),2)+
		shl(solid(i,j+1),3)
end

function flags_cache(i,j)
	-- wrap around
	i%=128
	j%=128
	if(i<0) i+=128
	if(j<0) j+=128
	return cache[i+128*j+1]
end

function _draw()
	if isdirty then
 	cls()
	 --fillp(dither[8])

  local scale=8
  local dx,dy=x%scale,y%scale
  local i0,j0=flr(x/scale),flr(y/scale)
		local i=i0
		local x0=-dx-24
  while x0<127+24 do
  	local j=j0
	 	local y0=-dy-24
	 	while y0<127+24 do
	 		local f=solid(i,j)
	 		if f!=0 then
	 			--[[
	 			if f==1 or f==8 or f==4 or f==2 then
	 				circfill(x0,y0,scale,7)
	 			elseif f==12 then
	 				circfill(x0,y0-scale/2,scale/2,7)
	 			else
	 				rectfill(x0-scale/2,y0-scale/2,x0+scale/2,y0+scale/2,7)
	 			end
	 			
 			 ]]
 			 --spr(f,x0,y0)
				--print(f,x0+3,y0+2,5)
				rectfill(x0,y0,x0+scale-1,y0+scale-1,(i+j)%2+9)
				if(solid(i,j)==1) pset(x0+1,y0+1,1)
				if(solid(i+1,j)==1) pset(x0+3,y0+1,1)
				if(solid(i+1,j+1)==1) pset(x0+3,y0+3,1)
				if(solid(i,j+1)==1) pset(x0+1,y0+3,1)
 			end
 			
 			j+=1
 			y0+=scale
 		end
 		i+=1
 		x0+=scale
  end
 	isdirty=false
	end
	
	fillp()
	rectfill(0,0,127,8,1)
 print("mem:"..stat(0).." cpu:"..stat(1).."("..stat(7)..")",2,2,7)
	--print(x.."/"..y,2,2,7)
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
