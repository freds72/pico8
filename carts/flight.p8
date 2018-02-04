pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
local src_colors={0x.006d,0x.00d6}
local dst_colors={0x.00d6,0x.006d}
local inv_shade={}
for i=0,15 do
	local s0,s1=0,0
	for b=1,4 do
		local src=band(0x1,shr(i,b-1))
		local shift=(b-1)*8
		s0=bor(s0,shl(src_colors[src+1],shift))
		s1=bor(s1,shl(dst_colors[src+1],shift))
	end
	inv_shade[s0]=s1
end

function sqr_dist(x0,y0,x1,y1)
	local dx,dy=x1-x0,y1-y0
	if abs(dx)>128 or abs(dy)>128 then
		return 32000
	end
	return dx*dx+dy*dy
end

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
   	sset(x+ix,y+iy,c)
  	end
   srcx-=ddy0
  	srcy+=ddx0
  end
  dx0+=ddx0
  dy0+=ddy0
 end
end

local time_t=0
local plyr,lead
local actors={}

local cam_x,cam_y
function cam_track(x,y)
	cam_x,cam_y=x,y
end

function cam_project(x,y)
	return 64-cam_x+x,64+cam_y-y
end

local orders={}
function make_order(btn,dly,ttl,msg)
	add(orders,{
		btn=btn,
		dly=dly,
		ttl=ttl,
		check=false,
		msg=msg
	})
end

function pop_order()
	if #orders>1 then
		local o=orders[#orders]
		orders[#orders]=nil
		o.dly_t=time_t+o.dly
		o.ttl_t=o.dly_t+o.ttl
		return o
	end
end

local current_order
function update_orders(btn)
	if not current_order or current_order.ttl_t<time_t then
	 -- need new orders!
		current_order=pop_order()
	end
	if current_order and current_order.dly<time_t then
		current_order.check=true
	end
end

function draw_orders()
 if current_order and current_order.dly_t<time_t then
		local t=current_order.ttl_t-time_t
		rectfill(0,8,128*t/current_order.ttl,16,7)
 	if current_order.check then
 		print("yeah!",2,10,0)
 	else
 		print(current_order.msg,2,10,0)
		end
 end
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
function make_part(x,y,dx,dy,grav)
	return add(parts,{
		x=x,y=y,
		dx=dx or 0,
		dy=dy or 0,
		r=1,dr=0,
		inertia=0.98,
		grav=grav or false,
		t=time_t+rnd(30),
		cb=nil
	})
end

function make_blast(x,y,dx,dy)
	p=make_part(x,y)
	p.dr=-0.5
	p.r=10
	p.t=time_t+8
	p.c=7
	for i=1,rnd(10)+20 do
		local angle=rnd()
		local c,s=cos(angle),sin(angle)
		local px,py=x+rnd(8)*c,y+rnd(8)*s
		local pdx,pdy=dx+c,dy+s
		if py<0 then
			pdy=-0.5*pdy
			py=0
		end
		p=make_part(px,py,pdx,pdy,true)		
		p.t=time_t+90
		p.cb=function(self)
			if time_t%2==0 then
				p=make_part(self.x,self.y)
				p.dr=0.1*rnd()
				p.dy=0.2
				p.t=time_t+rnd(30)
			end
		end
	end
end

function update_parts()
	for _,p in pairs(parts) do
		if p.t<time_t then
			del(parts,p)
		else
			p.x+=p.dx
			p.y+=p.dy
			p.r+=p.dr
			p.dx*=p.inertia
			p.dy*=p.inertia
			if p.grav then
				p.dy-=0.01
			end
			if p.cb then
				p.cb(p)
			end
		end
	end
end

function draw_parts()
	for i=1,#parts do
		local p=parts[i]
		local x,y=cam_project(p.x,p.y)
		local c=p.c or 13
		if p.r==1 then
			pset(x,y,c)
		else
			circfill(x,y,p.r,c)
		end
	end
end

local coins={}
function make_coin(x,y)
	add(coins,{
		x=x,y=y,
		ttl=time_t+90
	})
end

function update_coins()
	for _,c in pairs(coins) do
		if c.ttl<time_t then
			del(coins,c)
		else
			if sqr_dist(c.x,c.y,plyr.x,plyr.y)<4 then
				plyr.score+=1
				del(coins,c)
			end
		end
	end
end

function draw_coins()
	for _,c in pairs(coins) do
		local x,y=cam_project(c.x,c.y)
		circfill(x,y,2,7)
	end	
end

function make_actor(x,y,npc)
 return add(actors,{
 	x=x,y=y,
 	dx=0,dy=0,
 	acc=1,
 	angle=0,
 	da=0,
 	sx=48,
 	sy=0,
 	input=npc and control_npc or control_plyr,
 	hit=hit
 })
end

function control_plyr(self)
	if(btn(2)) self.da=0.01
	if(btn(3)) self.da=-0.01
end

function control_npc(self)
	if current_order and current_order.dly_t<time_t then
		if not current_order.done then	
			make_coin(self.x,self.y)
			current_order.done=true
		end
		if(current_order.btn==2) self.da=0.01
		if(current_order.btn==3) self.da=-0.01
	end
end

function hit(self)
	local dx,dy=normalize(self.dx,self.dy)
	make_blast(self.x,self.y,dx*self.acc,dy*self.acc)
	del(actors,self)
end

function update_actor(a)
	a:input()
	
	a.angle+=a.da
	a.da*=0.96
	
	-- simulate air friction
	a.dx*=0.95
	a.dy*=0.95
	-- apply thrust force
	local dx,dy=cos(a.angle),sin(a.angle)
	a.dx+=dx
	a.dy+=dy
	local u,v=normalize(a.dx,a.dy)
	dx,dy=normalize(dx,dy)
	a.x+=u*a.acc
	a.y+=v*a.acc

	-- collision?
	local hit=false
	if a.y<4 then
		hit=true
	end
	for _,other in pairs(actors) do
		if other!=a and sqr_dist(a.x,a.y,other.x,other.y)<64 then
			other:hit()
			hit=true
		end
	end
	if hit then
		a:hit()
		return
	end
	
	-- calculate drift force
	if abs(dx*u+dy*v)<0.90 then
		make_part(a.x+rnd(2)-1-8*u,a.y+rnd(2)-1-8*v)
	end
end

function _update60()
	time_t+=1

	for _,a in pairs(actors) do
		update_actor(a)
	end
	
	update_coins()	
	update_parts()
	update_orders()
		
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
	fillp()
	draw_parts()
	draw_coins()
		
	palt(0,false)
	palt(14,true)
	pal(6,13)
	for _,a in pairs(actors) do
		x,y=cam_project(a.x,a.y)

		a.visible=true
		if x<-16 or x>144 or y<-16 or y>144 then
			a.visible=false
		else		
 		rspr(a.sx,a.sy,32,16,a.angle,2)	
 	
 		x-=8
 		y-=8
 		spr(36,x,y,2,2)
		end
	end
	
	-- draw hud
	if lead and not lead.visible then
		x,y=cam_project(lead.x,lead.y)
		x-=plyr.x
		y-=plyr.y
		local angle=atan2(x,y)
		x,y=cos(angle),sin(angle)

 	line(64+8*x,64+8*y,64+10*x,64+10*y,13)
	end

	fillp()
	rectfill(0,0,127,8,1)
	print((flr(1000*stat(1))/10).."%",2,2,7)
 print(plyr.score,112,2,7)
	--[[
	local mem=0x6000
	for j=0,63 do
		for i=0,31 do
			poke4(mem,inv_shade[peek4(mem)])
			mem+=4
		end
	end
	]]
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

	lead=make_actor(24,4,true)
	plyr=make_actor(0,4)
	plyr.score=0
		
	make_order(3,30,30,"up")
	make_order(3,30,30,"up")
	make_order(2,30,90,"down")
	make_order(3,30,90,"up")
	make_order(3,90,30,"up")
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
00000000eeeeeeeeeeeeeeee00000000ee0655555666650eeee6666666666eeeedddddddddddddddddddddddddddddee00000000000000000000000000000000
00000000eeeeeeeeeeeeeeee00000000eee00000555550eeeeeeee6666eeeeeeeeddddddddddddddddddddddddddddee00000000000000000000000000000000
00000000ee00000eeeeddeee00000000eeeeeeee00000eeeeeeeeeeeeeeeeeeeeedddddddddddddddddddddddddddddd00000000000000000000000000000000
00000000e049550eeeedddee00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddddddddddddddddddddddd00000000000000000000000000000000
00000000e049660eeeedddee00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddddedddddddddddddddd00000000000000000000000000000000
00000000ee00000eeeeddeee00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00000000eeeeeeeeeeeeeeee00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00000000eeeeeeeeeeeeeeee00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
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
