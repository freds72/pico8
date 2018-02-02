pico-8 cartridge // http://www.pico-8.com
version 16
__lua__

function normalize(u,v,scale)
	scale=scale or 1
	local d=sqrt(u*u+v*v)
	if (d>0) u/=d v/=d
	return u*scale,v*scale
end

function fast_sprr(sx,sy,x,y,a)
 local x0,y0=4,4
	
	local ducol,dvcol=sin(-a),cos(-a)
	local durow=ducol
	local dvrow=-dvcol
	
	local su=sx+4-(4*dvcol+4*ducol)
	local sv=sy+4-(4*dvrow+4*durow)

	local rowu=su
	local rowv=sv
	
	local u,v
	for j=y,y+7 do
		u,v=rowu,rowv
		for i=x,x+7 do
			local c=sget(sx+band(u,7),sy+band(v,7))
			pset(i,j,c)
			u+=durow
			v+=dvrow
		end
		rowu+=ducol
		rowv+=dvcol
	end
end

function rspr(sx,sy,x,y,a,w)
	local ca,sa=cos(a),sin(a)
 local srcx,srcy,addr,pixel_pair
 local ddx0,ddy0=ca,sa
 local mask=shl(0xfff8,(w-1))
 w*=4
 ca*=w
 sa*=w
 local dx0,dy0=sa-ca+w,-ca-sa+w
 w=2*w-1
 for ix=0,w do
  srcx,srcy=dx0,dy0
  for iy=0,w do
   if band(bor(srcx,srcy),mask)==0 then
   	local c=sget(sx+srcx,sy+srcy)
   	--if c!=14 then
   		sset(x+ix,y+iy,c)
  		--end
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
	sx=48,
	sy=0
}

local cam_x,cam_y
function cam_track(x,y)
	cam_x,cam_y=x,y
end

function cam_project(x,y)
	return 64-cam_x+x,64+cam_y-y
end

local clouds={}
function make_cloud(x,y,r)
	add(clouds,{
		x=x,y=y,
		r=r
	})
end
function draw_clouds()
	fillp(0b1010010110100101.1)
	for _,c in pairs(clouds) do
		local x,y=cam_project(c.x,c.y)
		circfill(x,y,c.r,13)
	end
end

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
		local p=parts[i]
		local x,y=cam_project(p.x,p.y)
		pset(x,y,13)
	end
end

function _update60()
	time_t+=1

	if(btn(2)) plyr.da=0.01
	if(btn(3)) plyr.da=-0.01
	
	plyr.angle+=plyr.da
	plyr.da*=0.96
	
	-- simulate air friction
	plyr.dx*=0.95
	plyr.dy*=0.95
	-- apply thrust force
	local dx,dy=cos(plyr.angle),sin(plyr.angle)
	plyr.dx+=dx
	plyr.dy+=dy
	local u,v=normalize(plyr.dx,plyr.dy)
	dx,dy=normalize(dx,dy)
	plyr.x+=u*plyr.acc
	plyr.y+=v*plyr.acc

	-- calculate drift force
	if abs(dx*u+dy*v)<0.90 then
		make_part(plyr.x+rnd(2)-1-8*u,plyr.y+rnd(2)-1-8*v)
	end
	
	update_parts()
	
	cam_track(plyr.x,plyr.y)
end

function _draw()
	cls(6)

	fillp()	
	--ground
	local x,y=cam_project(0,0)
	if y<127 then
		rectfill(0,y,127,127,6)
	end
	
	draw_clouds()
	draw_parts()
	
	palt(0,false)
	palt(14,true)
	
	rspr(plyr.sx,plyr.sy,32,16,plyr.angle,2)	
	
	x,y=cam_project(plyr.x,plyr.y)
	x-=8
	y-=8
	
	--[[
	pal(6,0)
	spr(36,x+1,y,2,2)
	spr(36,x-1,y,2,2)
	spr(36,x,y+1,2,2)
	spr(36,x,y-1,2,2)
	]]
	
	pal(6,13)
	spr(36,x,y,2,2)

	rectfill(0,0,127,8,1)
	print((flr(1000*stat(1))/10).."%",2,2,7)
end

function _init()
	for i=1,10 do
		local x,y=rnd(128)-64,rnd(128)+12
		for j=1,flr(rnd(4)) do
			local r=rnd(8)+4
			make_cloud(x+r/2,y+rnd(2)-1,r)
			x+=r
		end	
	end
end
__gfx__
00000000eeeeeeeeee0000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00000000ee88eeeee0999a0eee0000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00700700e000000e091414a0ee0650eeeeee00eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00077000e088777009444490ee0650eeeeee060eeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddddeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00077000e055667004555440ee0990eeeeee0660eeeeeeeeeee666eeeeeeeeeeeedddddddddddeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00700700e000000e0339bbb0ee0440eeeee0066000000eeeeee6666eeeeeeeeeeddddddddddddeddddddddeeeeeeeeee00000000000000000000000000000000
00000000ee88eeee003000b0eee00eeeee066666666cc0eeeeee66666666eeeeeddddddddddddddddddddddeeeeeeeee00000000000000000000000000000000
00000000eeeeeeeeee0eee0eeeeeeeeeee070000007cc70eeee66666666666eeeddddddddddddddddddddddeeddddeee00000000000000000000000000000000
00000000eeeeeeee0000000000000000ee0655555666650eeee6666666666eeeedddddddddddddddddddddddddddddee00000000000000000000000000000000
00000000eeeeeeee0000000000000000eee00000555550eeeeeeee6666eeeeeeeeddddddddddddddddddddddddddddee00000000000000000000000000000000
00000000ee00000e0000000000000000eeeeeeee00000eeeeeeeeeeeeeeeeeeeeedddddddddddddddddddddddddddddd00000000000000000000000000000000
00000000e049550e0000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddddddddddddddddddddddd00000000000000000000000000000000
00000000e049660e0000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddddedddddddddddddddd00000000000000000000000000000000
00000000ee00000e0000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00000000eeeeeeee0000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00000000eeeeeeee0000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000000000000000000000000000000000000000000000000000
