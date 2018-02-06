pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- airshow
-- by freds72
-- futures
local time_t,time_dt=0,0
local dither_pat={
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
  0b0000001000001000
}
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

-- futures
function futures_update(futures)
	futures=futures or before_update
	for _,f in pairs(futures) do
		if not coresume(f) then
			del(futures,f)
		end
	end
end
function futures_add(fn,futures)
	return add(futures or before_update,cocreate(fn))
end
function wait_async(t,fn)
	local i=1
	while i<=t do
		if fn then
			if not fn(i) then
				return
			end
		end
		i+=time_dt
		yield()
	end
end
-- print text helper
local txt_offsets={{-1,0},{0,-1},{0,1},{-1,-1},{1,1},{-1,1},{1,-1}}
local txt_center,txt_shade,txt_border=false,-1,false
function txt_options(c,s,b)
	txt_center=c or false
	txt_shade=s or -1
	txt_border=b or false
end
function txt_print(str,x,y,col)
	if txt_center then
		x-=flr((4*#str)/2+0.5)
	end
	if txt_shade!=-1 then	
		print(str,x+1,y,txt_shade)
		if txt_border then
			for _,v in pairs(txt_offsets) do
				print(str,x+v[1],y+v[2],txt_shade)
			end
		end
	end
	print(str,x,y,col)
end

-- helpers
function pop(a)
	if #a>0 then
		local p=a[#a]
		a[#a]=nil
		return p
	end
end
-- calls 'fn' method on all elements of a[]
-- pairs allows add/remove while iterating
function forall(a,fn)
	fn=fn or "update"
	for _,v in pairs(a) do
		if not v[fn](v) then
			del(a,v)
		end
	end
end
function clone(src,dst)
	-- safety checks
	if(src==dst) assert()
	if(type(src)!="table") assert()
	dst=dst or {}
	for k,v in pairs(src) do
		if(not dst[k]) dst[k]=v
	end
	-- randomize selected values
	if src.rnd then
		for k,v in pairs(src.rnd) do
			-- don't overwrite values
			if not dst[k] then
				dst[k]=v[3] and rndarray(v) or rndlerp(v[1],v[2])
			end
		end
	end
	return dst
end
function amortize(x,dx)
	x*=dx
	return abs(x)<0.001 and 0 or x
end
function lerp(a,b,t)
	return a*(1-t)+b*t
end
function rndlerp(a,b)
	return lerp(b,a,1-rnd())
end
function smoothstep(t)
	t=mid(t,0,1)
	return t*t*(3-2*t)
end
function rndrng(ab)
	return flr(rndlerp(ab[1],ab[2]))
end
function rndarray(a)
	return a[flr(rnd(#a))+1]
end
function smoothstep(t)
	t=mid(t,0,1)
	return t*t*(3-2*t)
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

-- camera
local cam_x,cam_y=0,0
local shkx,shky=0,0
function cam_shake(u,v,pow)
	shkx=min(4,shkx+pow*u)
	shky=min(4,shky+pow*v)
end
function cam_update()
	shkx*=-0.7-rnd(0.2)
	shky*=-0.7-rnd(0.2)
	if abs(shkx)<0.5 and abs(shky)<0.5 then
		shkx,shky=0,0
	end
	camera(shkx,shky)
end
function cam_track(x,y)
	cam_x,cam_y=x,y
end
function cam_project(x,y,z)
	local w=(16-z)/16
	return 64-(cam_x-x)*w,64+(cam_y-y)*w,w
end

-- zbuffer
local zbuf={}
function zbuf_clear()
	zbuf[1]={}
	zbuf[2]={}
	zbuf[3]={}
end
function zbuf_write(obj)
	local zi=obj.zorder or 2
	add(zbuf[zi],{obj=obj})
end
function zbuf_draw()
	local xe,ye
	for _,v in pairs(zbuf[1]) do
		xe,ye,w=cam_project(v.obj.x,v.obj.y,8)
		v.obj:draw(xe,ye,w)
	end
	for _,v in pairs(zbuf[2]) do
		xe,ye=cam_project(v.obj.x,v.obj.y,0)
		v.obj:draw(xe,ye,1)
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

-- particles
local parts,all_parts={}
function make_part(x,y,src,dx,dy)
	src=all_parts[src]
	local p=clone(all_parts[src.base_cls or "part_cls"],
		clone(src,{
			x=x,
			y=y,
			dx=dx or 0,
			dy=dy or 0}))
 if(not p.update) assert()
	if(p.sfx) sfx(p.sfx)
	p.t=time_t+p.ttl
	return add(parts,p)
end


local plyr,lead
local actors={}

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
function make_cloud(x,y,r,z)
	add(clouds,{
		zorder=z or 2,
		x=x,y=y,
		r=r,
		update=function(self)
			zbuf_write(self)
			return true
		end,
		draw=draw_cloud
	})
end
function draw_cloud(self,x,y,w)
	local r=self.r*w
	if self.zorder==1 then
		fillp()
	else
		fillp(0b1010010110100101.1)
	end
	circfill(x,y,r,13)
end

function update_emitter(self)
	if update_part(self) then
		if self.emit_t<time_t then
			make_part(self.x,self.y,self.emit_cls)
			self.emit_t=time_t+self.emit_dly
		end
		return true
	end
	return false
end
function make_blast(x,y,dx,dy)
	local p=make_part(x,y,"blast")
	for i=1,p.debris do
		local angle=rnd()
		local c,s=cos(angle),sin(angle)
		local px,py=x+rnd(8)*c,y+rnd(8)*s
		local pdx,pdy=dx+c,dy+s
		if py<0 then
			pdy=abs(0.8*pdy)
			py=0
		end
		make_part(px,py,"debris",pdx,pdy)
	end
	cam_shake(rnd(),rnd(),5)
end

function update_part(p)
	if(p.t<time_t) return false
	p.x+=p.dx
	p.y+=p.dy
	if p.y<0 then
		p.y=0
		p.dy=-0.9*p.dy
	end
	p.r+=p.dr
	p.dx*=p.inertia
	p.dy*=p.inertia
	if p.g then
		p.dy-=0.01
	end
	zbuf_write(p)
	return true
end

function draw_pixel_part(self,x,y)
	pset(x,y,self.c or 13)
end
function draw_circ_part(self,x,y)
	local f=smoothstep((self.t-time_t)/self.ttl)
	fillp(dither_pat[flr(#dither_pat*f)+1])
	circfill(x,y,self.r,self.c or 13)
end

function update_coin(c)
	if(c.t<time_t) return false
	
	if sqr_dist(c.x,c.y,plyr.x,plyr.y)<4 then
		plyr.score+=1
		-- todo: sound + feedback
		return false
	end
	zbuf_write(c)
	return true
end

function draw_coin_part(self,x,y)
	circfill(x,y,2,7)
end

function draw_ground(self,x,y,w)
	if(y>127) return
	pal()
	rectfill(0,y,127,127,13)
	for j=3,12 do
		x,y,w=cam_project(0,0,j)
 	local dx=w*(plyr.x%16)
		x=-dx
		while x<127 do
			x+=w*16
			pset(64+x,y,6)
			pset(64-x-2*dx,y,6)
		end
	end
end

-- actors
function make_actor(x,y,npc)
	return add(actors,{
	x=x,y=y,
	dx=0,dy=0,
	acc=0.8,
	angle=0,
	da=0,
	sx=48,
	sy=0,
	update=update_actor,
	draw=draw_plane,
	input=npc and control_npc or control_plyr,
	hit=hit
	})
end

function draw_plane(self,x,y)
	palt(0,false)
	palt(14,true)
	pal(6,13)
	rspr(self.sx,self.sy,32,16,self.angle,2)
	
	spr(36,x-8,y-8,2,2)
end

function get_turn_rate(b)
	if(b==2) return 0.005
	if(b==3) return -0.01
end

function control_plyr(self)
	if(btn(2)) self.da=get_turn_rate(2)
	if(btn(3)) self.da=get_turn_rate(3)
end

function control_npc(self)
	if current_order and current_order.dly_t<time_t then
		if not current_order.done then	
			make_part(self.x,self.y,"coin")
			current_order.done=true
		end
		if(current_order.btn==2) self.da=get_turn_rate(2)
		if(current_order.btn==3) self.da=get_turn_rate(3)
	end
end

function hit(self)
	local dx,dy=normalize(self.dx,self.dy)
	make_blast(self.x,self.y,dx*self.acc,dy*self.acc)
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
		return false
	end
	
	-- calculate drift force
	if abs(dx*u+dy*v)<0.90 then
		make_part(a.x+rnd(2)-1-8*u,a.y+rnd(2)-1-8*v,"trail")
	end
	
	zbuf_write(a)
	return true
end

function _update60()
	time_t+=1
	
	zbuf_clear()
	-- known bug: one frame delay
	cam_track(plyr.x,plyr.y)
	
	forall(actors)
	forall(parts)
	forall(clouds)
	update_orders()

	cam_update()
end

function _draw()
	cls(6)

	--ground
	local x,y,w=cam_project(0,0,12)
	draw_ground({},x,y,w)
	
	zbuf_draw()
	
	-- draw hud
	if lead and not lead.visible then
		local x,y=cam_project(lead.x,lead.y,0)
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
end

function _init()
 	all_parts={
 	["part_cls"]={
 		update=update_part,
 		draw=draw_pixel_part,
 		inertia=0.98,
 		r=1,dr=0,
 		ttl=30
 	},
 	["coin"]={
 		update=update_coin,
 		draw=draw_coin_part,
 		ttl=90
 	},
 	["trail"]={
 		c=13,
 		rnd={
 			ttl={24,32},
 		}
 	},
 	["smoke"]={
 		draw=draw_circ_part,
 		c=0xd7,
 		rnd={
 			dr={0.01,0.05},
 			ttl={30,60},
 		},
 		dy=0.2
 	},
 	["blast"]={
 		draw=draw_circ_part,
 		dr=-0.5,
 		r=10,
 		rnd={
 			debris={8,12}
 		},
 		ttl=16,
 		c=0x77
 	},
 	["debris"]={
 		g=true,
 		update=update_emitter,
 		rnd={
 			emit_dly={2,8}
 		},
 		emit_t=0,
 		emit_cls="smoke"
 	}
 }

	for i=1,10 do
		local x,y=rnd(128)-64,rnd(128)+12
		local z=flr(rnd(2))+1
		for j=1,flr(rnd(4)) do
			local r=rnd(8)+4
			make_cloud(x+r/2,y+rnd(2)-1,r,z)
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
000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
007007000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
000770000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
000770000000000000000000000000000000000000000000eee666eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee66eeeeeeeee
007007000000000000000000000000000000000000000000eee6666eeeeeeeeeeee666eeeeeeeeeeeeeeeeeeeeeeeeeeeeeee66eeeeeeeeeeee6e6666eeeeeee
000000000000000000000000000000000000000000000000eeee66666666eeeeeee6666eeeeeeeeeeee6666eeeeeeeeeeee6ee66666eeeeeeee66e66666eeeee
000000000000000000000000000000000000000000000000eee66666666666eeeeee66666666eeeeeeee66666666eeeeeee6666666666eeeeeee666666666eee
0000000000000000eeeeeeee000000000000000000000000eee6666666666eeeeee66666666666eeeee66666666666eeeee66666666666eeeee66666666666ee
0000000000000000eeeeeeee000000000000000000000000eeeeee6666eeeeeeeee6666666666eeeeee6666666666eeeeeee666666666eeeeeee666666666eee
0000000000000000eeeddeee000000000000000000000000eeeeeeeeeeeeeeeeeeeee66eeeeeeeeeeeeee66eeeeeeeeeeee66e66666eeeeeeee66e66666eeeee
0000000000000000eeedddee000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee6e66eeeeeeeeeeee6e6666eeeeeee
0000000000000000eeedddee000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee66eeeeeeeee
0000000000000000eeeddeee000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000eeeeeeee000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000eeeeeeee000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
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
