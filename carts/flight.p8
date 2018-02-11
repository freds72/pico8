pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- airshow
-- by freds72
-- screen mgt
local cur_screen

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

function rspr(s,x,y,a,w)
	local sx,sy=band(s*8,127),8*flr(s/16)
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
function make_order(btn,ttl)
	add(orders,{
		btn=btn,
		ttl=ttl,
		check=false
	})
end

function pop_order()
	if #orders>1 then
		local o=orders[#orders]
		orders[#orders]=nil
		o.t=time_t+o.ttl
		return o
	end
end

local current_order
function update_orders(btn)
	if not current_order or current_order.t<time_t then
	 -- need new orders!
		current_order=pop_order()
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
	for j=-8,8 do
		x,y,w=cam_project(0,0,j)
 	local dx=w*(cam_x%16)
		x=-dx
		while x<127 do
			pset(64+x,y,13)
			pset(64-x-2*dx,y,13)
			x+=w*16
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
 	frames={64,66,68,70,72,74,76},
 	frame=1,
 	df=0,
 	rolling=false,
 	inverted=false,
 	update=update_actor,
 	draw=draw_actor,
 	input=npc and control_npc or control_plyr,
 	hit=hit
 	})
end

function draw_actor(self,x,y)
	local s=self.frames[flr(self.frame)]
	rspr(s,32,16,self.angle,2)
	
	x-=8
	y-=8
	palt(14,true)
	--[[
	palt(0,false)
	pal(7,0)
 pal(6,0)
	spr(36,x+1,y,2,2)
	spr(36,x-1,y,2,2)
	spr(36,x,y+1,2,2)
	spr(36,x,y-1,2,2)
	pal()
	]]
	
	pal(6,13)
	palt(14,true)
	spr(36,x,y,2,2)
end

function draw_actor_shadow(self,x,y,w)
	if time_t%2==0 then
		spr(78,x-8,y-8,2,2)
	end
end

function get_turn_rate(self,b)
	local tr
	if(b==2) tr=0.005
	if(b==3) tr=-0.01
	if(self.inverted) tr=-tr
	return tr
end

function process_input(self)
	local btns={
		[0]=btnp(0),
		[1]=btnp(1),
		[2]=btn(2),
		[3]=btn(3)
	}
	control_actor(self,btns)
end

function control_actor(self,btns)
	if not self.rolling then
		if(btns[2]) self.da=get_turn_rate(self,2)
		if(btns[3]) self.da=get_turn_rate(self,3)
		if btns[0] or btns[1] then
			self.rolling=true
			if self.inverted then
				self.df=-0.1
				self.tf=1
			else
				self.df=0.1
				self.tf=#self.frames
			end
		end
	end
end

function control_npc(self)
	if current_order then
		if not current_order.active then
			make_part(self.x,self.y,"coin")
			current_order.active=true
		end
	self.da=get_turn_rate(self,current_order.btn)
	end
end

function hit(self)
	local dx,dy=normalize(self.dx,self.dy)
	make_blast(self.x,self.y,dx*self.acc,dy*self.acc)
	self.disable=true
end

function move_actor(a)
	a.angle+=a.da
	-- rotation damping
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

	if a.rolling then
		if flr(a.frame)!=a.tf then
			a.frame+=a.df
		else
			a.frame=a.tf
			a.rolling=false
			a.inverted=not a.inverted
		end
	end
end

function update_actor(a)
	if (a.disable) return false
	
	a:input()
	
	move_actor(a)

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
		make_part(a.x+rnd(2)-1-8*u,a.y+rnd(2)-1-10*v,"trail")
	end
	
	zbuf_write(a)
	zbuf_write({
		x=a.x,y=0,
		draw=draw_actor_shadow
	})
	return true
end

-- track editor
local save_screen={}
local edit_screen={}
local edit_actor=make_actor(0,4)
local edit_cmds={}
local all_cmds={}

local track_id=0
local all_save_cmds={
	{spr=1,click=cmd_next_track},
	{spr=2,click=cmd_prev_track},
	{spr=3,click=cmd_save_ok},
	{spr=4,click=cmd_save_exit}
}
function save_screen:draw()
	edit_screen:draw()
	rectfill(0,110,127,118,1)
end
function save_screen:update()
end

function cmd_fly(cmd,btns)
	local cmd_item=add(edit_cmds,{
		cmd=cmd,
		t=0,
		path={}
	})
	control_actor(edit_actor,btns)
	local dt=cmd.ttl
	for j=1,dt do
		move_actor(edit_actor)
		if j%4==0 then
			add(cmd_item.path,{type=1,x=edit_actor.x,y=edit_actor.y})
		end
	end
	cmd_item.t=dt
	cmd_item.actor=clone(edit_actor)
	add(cmd_item.path,{type=1,x=edit_actor.x,y=edit_actor.y})
end
function cmd_roll(cmd,btns)
	local cmd_item=add(edit_cmds,{
		cmd=cmd,
		t=0,
		path={}
	})
	control_actor(edit_actor,btns)
	local dt=0
	while edit_actor.rolling do
		move_actor(edit_actor)  			
		if dt%4==0 then
			add(cmd_item.path,{type=1,x=edit_actor.x,y=edit_actor.y})
		end
		dt+=1
	end 		
	cmd_item.t=dt
	cmd_item.actor=clone(edit_actor)
	add(cmd_item.path,{type=1,x=edit_actor.x,y=edit_actor.y})
end
function cmd_checkpoint(cmd)
	if #edit_cmds>0 then
		if edit_cmds[#edit_cmds].cmd==cmd then
			-- cheap error feedback
			cls(8)
			flip()
			return
		end
	end
	local cmd_item=add(edit_cmds,{
		cmd=cmd,
		t=0,
		path={}
	})
	cmd_item.actor=clone(edit_actor)
	add(cmd_item.path,{type=2,x=edit_actor.x,y=edit_actor.y})
end
function cmd_del()
 pop(edit_cmds)
	if #edit_cmds>0 then
		-- start over from previous command
		edit_actor=clone(edit_cmds[#edit_cmds].actor)
	else
		edit_actor=make_actor(0,4)
	end
end
function cmd_save()
	--cur_screen=save_screen
	save_track(edit_cmds,0)
end
function cmd_load()
	--cur_screen=load_screen
	load_track(0)
end
function cmd_exit()
	-- release memory
	edit_cmds={}
	edit_actor=nil
	cur_screen=start_screen
end
function load_track(id)
	edit_cmds={}
	edit_actor=make_actor(0,4)
	local mem=0x2000+id*256
	local id=peek(mem)
	while id!=0 do
		local cmd=all_cmds[id]
 	local btns={}
 	if cmd.btn then
 		btns[cmd.btn]=true
 	end
		cmd.click(cmd,btns)	
		mem+=1
		id=peek(mem)
	end
end
function save_track(cmds,id)
	if(#cmds>254) assert("track too long")
	
 -- map data
	local mem=0x2000+id*256
	local addr=mem
	for i=1,#cmds do
		local cmd=cmds[i]
		poke(addr,cmd.cmd.id)
		addr+=1
	end
	poke(addr,0)
	cstore(mem,mem,256)
end

all_cmds={
		{spr=1,click=cmd_fly,btn=3,ttl=30},
		{spr=2,click=cmd_fly,btn=2,ttl=30},
		{spr=3,click=cmd_roll,btn=0},
		-- checkpoint. must be collected in order
		{spr=4,click=cmd_fly,ttl=30},
		{spr=5,click=cmd_checkpoint},
		{spr=6,click=cmd_del},
		{spr=8,click=cmd_load},
		{spr=7,click=cmd_save},
		{spr=9,click=cmd_exit}
	}
-- assign cmd index
-- no more than 15 commands!!!
-- id and spr must match for fly commands
for i=1,#all_cmds do
	all_cmds[i].id=i
end
function addat(array,idx,elt)
	local len=#array
	for i=idx,len do
		array[i+1]=array[i]
	end
	array[idx]=elt
end
function pop(array)
	local len=#array
	if(len<=0) return
	local elt=array[len]
	array[len]=nil
	return elt
end

function edit_screen:update()
	if(btnp(0)) edit_cmd_i-=1
	if(btnp(1)) edit_cmd_i+=1
	edit_cmd_i=mid(edit_cmd_i,1,#all_cmds)
		
	if btnp(4) then
		local cmd=all_cmds[edit_cmd_i]
 	local btns={}
 	if cmd.btn then
 		btns[cmd.btn]=true
 	end
		cmd.click(cmd,btns)	
	end
end

function edit_screen:draw()
	cls(0)
	
	local t,count=0,0
	local a=make_actor(0,4)
	for k=1,#edit_cmds do
		local cmd=edit_cmds[k]
 	a=cmd.actor
 	for i=1,#cmd.path do
 		local path=cmd.path[i]
 		local x,y=cam_project(path.x,path.y,8)
 		if path.type==1 then
 			pset(x,y,6)
 		elseif path.type==2 then
	 		circfill(x,y,2,7) 	
			end
 	end
 	
 	t+=cmd.t
 	count+=1
	end
	
	cam_track(a.x,a.y)
	x,y,w=cam_project(0,0,8)
	draw_ground({},x,y,w)
	x,y=cam_project(a.x,a.y,8)
	draw_actor(a,x,y)
	
	-- draw commandbar
	palt(14,true)
	rectfill(0,116,127,127,1)
	local x=1
	for i=1,#all_cmds do
		local cmd=all_cmds[i]
		pal(5,i==edit_cmd_i and 7 or 5)
		spr(cmd.spr,x,118)
		x+=10
	end
	pal()
	
	print((t/60).."s - "..count.."/256",2,2,7)
end

local game_screen={}
function game_screen:update()
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

function game_screen:draw()
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
	
	local mem=0x6000+flr(y)*64
	for j=y,127 do
		for i=0,15 do
		 poke4(mem,shades[peek4(mem)])
			mem+=4
		end
	end
	
	pal()
	palt(14,true)
	spr(34,2,100,2,2)
	local angle=(time_t%128)/128
	angle=lerp(0.2,0.8,angle)-0.2
	x,y=6*cos(angle),-6*sin(angle)
	line(10,108,10+x,110-y,6)
	
	fillp()
	rectfill(0,0,127,8,1)
	print((flr(1000*stat(1))/10).."%",2,2,7)
 print(plyr.acc,112,2,7)
end

function game_screen:init()
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
		
	make_order(3,30)
	make_order(3,30)
	make_order(2,90)
	make_order(3,45)
	make_order(3,60)
end

cur_screen=edit_screen
function _draw()
	cur_screen:draw()
end
function _update60()
	cur_screen:update()
end
function _init()
	if cur_screen.init then
		cur_screen:init()
	end
end

__gfx__
00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
00000000eeeeeeeeeeeeeeeeee5555eee555555eeeeeee5eeeeeeeeeeee55eeeeee55eeeeeeeeeee000000000000000000000000000000000000000000000000
00700700eee55eeee555555ee5eeee5eee5ee5eeeeeee55eee5ee5eeeee55eeeee5555eeee5555ee000000000000000000000000000000000000000000000000
00077000ee5555eeee5555eee5ee5eeeeee55eeee55e55eeeee55eeeee5555eeeee55eeeee5555ee000000000000000000000000000000000000000000000000
00077000e555555eeee55eeee5ee55eeee5555eeee555eeeeee55eeeeee55eeeeee55eeeee5555ee000000000000000000000000000000000000000000000000
00700700eeeeeeeeeeeeeeeeee55555ee555555eeee5eeeeee5ee5eee5eeee5ee5eeee5eee5555ee000000000000000000000000000000000000000000000000
00000000eeeeeeeeeeeeeeeeeeee55eeeeeeeeeeeeeeeeeeeeeeeeeee555555ee555555eeeeeeeee000000000000000000000000000000000000000000000000
00000000eeeeeeeeeeeeeeeeeeee5eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeddddddddeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000eeeeee666eeeeeeeeeeeeeeeeeeeeeeeddddddddeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000eeee66e6e66eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000eee6eeeeeee6eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000ee6e6eeeee6e6eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000ee6eeeeeeeee6eeeeeeeeeeeeeeeeeeeddddddddeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000e6eeeeeeeeeee6eeeeeeeeeeeeeeeeeeddddddddeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000e66eeee6777766eeeeeeeeeeeeeeeeeeddddddddeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000e6eeeeee777776eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000ee6eeeeee7776eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000ee6e6eeeee676eeeeeeeeeeeeeeeeeeeddddddddeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000eee6eeeeeee6eeeeeeeeeeeeeeeeeeeeddddddddeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000eeee66eee66eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000eeeeee666eeeeeeeeeeeeeeeeeeeeeeeddddddddeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee66eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eee666eeeeeeeeeeeeeeeeeeeeeeeeeeeeeee66eeeeeeeeeeee6e6666eeeeeeeeee6e66eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eee6666eeeeeeeeeeee666eeeeeeeeeeeee6e66666eeeeeeeee66e66666eeeeeeee6666666eeeeeeeeeee66eeeeeeeeeeeeee66eeeeeeeeeeeeeeeeeeeeeeeee
eeee66666677eeeeeeee66666677eeeeeee6666666776eeeeeee666666666eeeeeee666666666eeeeee6666666666eeeeee6666666666eeeeeeee666666eeeee
eee66666666666eeeee66666666666eeeee66666667766eeeee66666667766eeeee66666667766eeeee66666666666eeeee66666666666eeeee6666666666eee
eee6666666666eeeeee6666666666eeeeeee666666666eeeeeee666666666eeeeee6666666776eeeeeee66666677eeeeeeee66666677eeeeeeee66666666eeee
eeeee66eeeeeeeeeeeeee66eeeeeeeeeeee6666666eeeeeeeee66e66666eeeeeeee6e66666eeeeeeeee666eeeeeeeeeeeee6666eeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee6e66eeeeeeeeeeee6e6666eeeeeeeeeeee66eeeeeeeeeeeeeeeeeeeeeeeeeeee666eeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee66eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee66eeeeeeeeeeeeee66eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000
eee66eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee66eeeeeeeeeeeee666eeeeeeeeeeeee666eeeeeeeeeeeeee66eeeeeeeeeeeeeeeeeeeeee0000000000000000
eee666ee77eeeeeeeee66eee66eeeeeeeee66ee666eeeeeeeee66ee666eeeeeeeeeeeee666eeeeeeeeeeeee666eeeeeeeeeeeee666eeeeee0000000000000000
ee66666677666eeeee66666677666eeeee66666666666eeeeee6666666666eeeeee66e6666666eeeeeeeee6666666eeeeeeeee6666666eee0000000000000000
eeee6666666666eeeee66666776666eeeee66666776666eeee666666776666eeeee66666776666eeeee66666666666eeeeee6666666666ee0000000000000000
eeeeee6666666eeeeeeeee6666666eeeeee66e6666666eeeeee6666677666eeeee66666677666eeeee66666677666eeeee66666677666eee0000000000000000
eeeeeee666eeeeeeeeeeeee666eeeeeeeeeeeee666eeeeeeeee66ee666eeeeeeeee66ee666eeeeeeeee66eee77eeeeeeeee666ee77eeeeee0000000000000000
eeeeeeeeeeeeeeeeeeeeeeee66eeeeeeeeeeeee666eeeeeeeeeeeee666eeeeeeeeeeeeee66eeeeeeeeeeeeeeeeeeeeeeeee66eeeeeeeeeee0000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee66eeeeeeeeeeeeee66eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000
__map__
0104040504040301040302050404020201010504040301010105040403010403050401010405030403010405040404040000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
