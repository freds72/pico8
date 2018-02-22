pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- aaline
-- by freds72
local ramp={
	[0]=1,
	[1]=5,
	[5]=6,
	[6]=7,
	[7]=7}
-- shade the pixel
-- dist indicates how far
-- we are from the exact
-- line center
function shadepix(m,dist)
	local x,y=m%128,flr(m/128)
	local n=3-flr(abs(shl(dist,2)))
	local c
	for i=1,n do
		c=c and ramp[c] or ramp[pget(x,y)]
	end
	if(c) pset(x,y,c)
end

-- algorithm credits:
-- http://jamesarich.weebly.com/uploads/1/4/0/3/14035069/480xprojectreport.pdf
function aaline(x0,y0,x1,y1)
	x0,y0,x1,y1=flr(x0),flr(y0),flr(x1),flr(y1)
	local du=x1-x0
	local dv=y1-y0
	if(du==0 and dv==0) return
	local addr=x0+128*y0
	local u,v,uincr,vincr
	-- x-major
	if abs(du)>=abs(dv) then
		u=x1
		uincr=du<0 and -1 or 1
		vincr=dv<0 and -128 or 128
		du,dv=abs(du),abs(dv)
	else
		u=y1
		uincr=dv<0 and -128 or 128
		vincr=du<0 and -1 or 1
		du,dv=abs(dv),abs(du)
	end

	local uend=u+du
	local d=2*dv-du
	local incrs=2*dv
	local incrd=2*(dv-du)
	local twovdu=0
	-- where is quake fast inverse
	-- sqrt when you need it :) 
	local invd=1/(2*sqrt(du*du+dv*dv))
	local invd2du=2*du*invd
	for i=u,uend do
		shadepix(addr,twovdu*invd)
		shadepix(addr+vincr,invd2du-twovdu*invd)
		shadepix(addr-vincr,invd2du+twovdu*invd)
		if d<0 then
			twovdu=d+du
			d+=incrs
		else
			twovdu=d-du
			d+=incrd			
			addr+=vincr
		end
		addr+=uincr
	end
end

local angle=0
function _update()
	angle+=0.002
end

function _draw()
	cls(0)
	local r,n=48,24
	local a,da=angle,1/n
	for i=1,n do
		local x,y=r*cos(a),r*sin(a)
		aaline(64+x,64+y,64-x,64-y,7)			
		a+=da
	end

	rectfill(0,0,127,8,1)
	print("cpu:"..flr(100*stat(1)).."%",2,2,7)
end
