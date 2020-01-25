pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
local aa={
[0x0000]=0x0000,
 [0xb000]=0xb300,
 [0xbb00]=0xbb30,
 [0xbbb0]=0xbbb3,
 [0xbbbb]=0xbbbb,
 [0x0b00]=0x3b30,
 [0x0bb0]=0x3bb3,
 [0x0bbb]=0x3bbb,
 [0x00b0]=0x03b3,
 [0x00bb]=0x03bb,
 [0xb0b0]=0xb3b3,
 [0xb0bb]=0xb3bb,
 [0xb00b]=0xb33b,
 [0x000b]=0x003b,
 [0xbb0b]=0xbb3b,
 [0x0b0b]=0x3b3b,
 [0xb300]=0xb300,
 [0xbb30]=0xbb30,
 [0xbbb3]=0xbbb3,
 [0xbbbb]=0xbbbb,
 [0x3b30]=0x3b30,
 [0x3bb3]=0x3bb3,
 [0x3bbb]=0x3bbb,
 [0x03b3]=0x03b3,
 [0x03bb]=0x03bb,
 [0xb3b3]=0xb3b3,
 [0xb3bb]=0xb3bb,
 [0xb33b]=0xb33b,
 [0x003b]=0x003b,
 [0xbb3b]=0xbb3b,
 [0x3b3b]=0x3b3b,
 [0xb3b0]=0xb3b3,
 [0xbb3b]=0xbb3b,
 [0x3b3b]=0x3b3b,
 [0xb03b]=0xb33b,
 [0x0b3b]=0x3b3b,
 [0x00b3]=0x03b3,
 [0x0003]=0x0003,
 [0x3000]=0x3000,
 [0x30b3]=0x33b3,
 [0x3b00]=0x3b30,
 [0xb303]=0xb303,
 [0x3b03]=0x3b33,
 [0x303b]=0x303b,
 [0x3003]=0x3003,
 [0x30bb]=0x33bb,
 [0xbb03]=0xbb33
}

local x,y,vx,vy=4,6,rnd(),rnd()
function _update()
	x+=vx
	if(x<0 or x>127) vx=-vx
	y+=vy
	if(y<0 or y>127) vy=-vy
end

local cube={
	v={
		{-1.0, -1.0,  1.0},
  {1.0, -1.0,  1.0},
  {1.0,  1.0,  1.0},
  {-1.0,  1.0,  1.0},
  {-1.0, -1.0, -1.0},
  {1.0, -1.0, -1.0},
  {1.0,  1.0, -1.0},
  {-1.0,  1.0, -1.0}
 },
	f={
		{1,2,3,4},
		{2,3,7,6},
		{6,7,8,5},
		{8,5,1,4},
		{1,2,6,5},
		{3,4,8,7}
	}
}

local cx,cy,cz=0,0,-3

function project(v)
	local x,y,z=v[1]-cx,v[2]-cy,v[3]-cz
	local w=64/z
	return 64+x*w,64-y*w	
end

function draw_model(model,m)
 local p={}
 local function v_cache(k)
 	local a=p[k]
 	if not a then
 		a=m_x_v(m,model.v[k])
 	 p[k]=a
 	end
 	return a
 end
 color(11)
	for _,f in pairs(model.f) do
		local x0,y0=project(v_cache(f[#f]))
		for i=1,#f do
			local x1,y1=project(v_cache(f[i]))
			line(x0,y0,x1,y1)
			x0,y0=x1,y1
		end
	end
end

function _draw()
	cls()
 
 circfill(64,64+8*cos(time()),4,11)
 
	local angle=time()/4
	for i=1,1 do	
		local axis={cos(angle),1,sin(angle)}
		v_normz(axis)
		local q=make_q(axis,angle-i/64)
		local m=m_from_q(q)

		draw_model(cube,m)
	end
	
	for mem=0x6000,0x7fc0,4 do
		--poke2(mem,aa[peek2(mem)])
		local b=peek4(mem)
		poke4(mem,
			bor(
				aa[band(0xffff,b)],
				lshr(aa[shl(b,16)],16)))
		poke2(mem+1,aa[shl(band(0xff.ff,b),8)])
	end
	
	for mem=0x6003,0x6003+127*64,64 do
		for j=mem,mem+56,4 do
			poke2(j,aa[peek2(j)])
		end
	end	
 
	rectfill(0,0,127,6,1)
	print(flr(100*stat(1)).."%",2,1,7)
end
-->8
-- math
function v_clone(v)
	return {v[1],v[2],v[3]}
end
function v_dot(a,b)
	return a[1]*b[1]+a[2]*b[2]+a[3]*b[3]
end
function v_normz(v)
	local d=v_dot(v,v)
	if d>0.001 then
		d=d^.5
		v[1]/=d
		v[2]/=d
		v[3]/=d
	end
	return d
end

function make_q(v,angle)
	angle/=2
	-- fix pico sin
	local s=-sin(angle)
	return {v[1]*s,
	        v[2]*s,
	        v[3]*s,
	        cos(angle)}
end

function m_from_q(q)
	local x,y,z,w=q[1],q[2],q[3],q[4]
	local x2,y2,z2=x+x,y+y,z+z
	local xx,xy,xz=x*x2,x*y2,x*z2
	local yy,yz,zz=y*y2,y*z2,z*z2
	local wx,wy,wz=w*x2,w*y2,w*z2

	return {
		1-(yy+zz),xy+wz,xz-wy,
		xy-wz,1-(xx+zz),yz+wx,
		xz+wy,yz-wx,1-(xx+yy)}
end

function m_x_v(m,v)
	local x,y,z=v[1],v[2],v[3]
	return {
		m[1]*x+m[4]*y+m[7]*z,
		m[2]*x+m[5]*y+m[8]*z,
		m[3]*x+m[6]*y+m[9]*z}
end
__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
