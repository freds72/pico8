pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- airshow
-- by freds72
local good_side,bad_side,any_side=0x1,0x2,0x0
-- register json context here
local _tok={
 ['true']=true,
 ['false']=false}
function nop() end
local _g={
	good_side=good_side,
	bad_side=bad_side,
	any_side=any_side,
	nop=nop}

-- json parser
-- from: https://gist.github.com/tylerneylon/59f4bcf316be525b30ab
local table_delims={['{']="}",['[']="]"}
local function match(s,tokens)
	for i=1,#tokens do
		if(s==sub(tokens,i,i)) return true
	end
	return false
end
local function skip_delim(str, pos, delim, err_if_missing)
 if sub(str,pos,pos)!=delim then
  if(err_if_missing) assert('delimiter missing')
  return pos,false
 end
 return pos+1,true
end
local function parse_str_val(str, pos, val)
	val=val or ''
	if pos>#str then
		assert('end of input found while parsing string.')
	end
	local c=sub(str,pos,pos)
	if(c=='"') return _g[val] or val,pos+1
	return parse_str_val(str,pos+1,val..c)
end
local function parse_num_val(str,pos,val)
	val=val or ''
	if pos>#str then
		assert('end of input found while parsing string.')
	end
	local c=sub(str,pos,pos)
	-- support base 10, 16 and 2 numbers
	if(not match(c,"-xb0123456789abcdef.")) return tonum(val),pos
	return parse_num_val(str,pos+1,val..c)
end
-- public values and functions.

function json_parse(str, pos, end_delim)
	pos=pos or 1
	if(pos>#str) assert('reached unexpected end of input.')
	local first=sub(str,pos,pos)
	if match(first,"{[") then
		local obj,key,delim_found={},true,true
		pos+=1
		while true do
			key,pos=json_parse(str, pos, table_delims[first])
			if(key==nil) return obj,pos
			if not delim_found then assert('comma missing between table items.') end
			if first=="{" then
				pos=skip_delim(str,pos,':',true)  -- true -> error if missing.
				obj[key],pos=json_parse(str,pos)
			else
				add(obj,key)
			end
			pos,delim_found=skip_delim(str, pos, ',')
	end
	elseif first=='"' then
		-- parse a string (or a global object)
		return parse_str_val(str,pos+1)
	elseif match(first,"-0123456789") then
		-- parse a number.
		return parse_num_val(str, pos)
	elseif first==end_delim then  -- end of an object or array.
		return nil,pos+1
	else  -- parse true, false
		for lit_str,lit_val in pairs(_tok) do
			local lit_end=pos+#lit_str-1
			if sub(str,pos,lit_end)==lit_str then return lit_val,lit_end+1 end
		end
		assert('invalid json token')
	end
end

-- screen mgt
local cur_screen
--
local time_t,time_dt=0,0
local dither_pat=json_parse('[0b1111111111111111,0b0111111111111111,0b0111111111011111,0b0101111111011111,0b0101111101011111,0b0101101101011111,0b0101101101011110,0b0101101001011110,0b0101101001011010,0b0001101001011010,0b0001101001001010,0b0000101001001010,0b0000101000001010,0b0000001000001010,0b0000001000001000,0b0000000000000000]')

local colors={0,13,6,7}

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
function filter(a,fn)
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
 local srcx,srcy
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
local parts={}
_g.update_emitter=function(self)
	if _g.update_part(self) then
		if self.emit_t<time_t then
			make_part(self.x,self.y,self.emit_cls)
			self.emit_t=time_t+self.emit_dly
		end
		return true
	end
	return false
end
_g.update_part=function(p)
	if(p.t<time_t or p.r<=0) return false
	p.x+=p.dx
	p.y+=p.dy
	if p.y<0 then
		p.y=0
		p.dy=-0.9*p.dy
	end
	p.r+=p.dr
	p.dx*=p.inertia
	p.dy*=p.inertia
	-- gravity
	if p.g then
		p.dy-=0.01
	end
	zbuf_write(p)
	return true
end

_g.draw_pixel_part=function(self,x,y)
	pset(x,y,self.c or 13)
end
_g.draw_circ_part=function(self,x,y)
	local f=smoothstep((self.t-time_t)/self.ttl)
	fillp(dither_pat[flr(#dither_pat*f)+1])
	circfill(x,y,self.r,self.c or 13)
end


local all_parts=json_parse('{"flash":{"dly":8,"r":0.8,"c":7,"dr":-0.1},"part_cls":{"update":"update_part","draw":"draw_pixel_part","inertia":0.98,"r":1,"dr":0,"ttl":30},"trail":{"c":13,"rnd":{"ttl":[24,32]}},"smoke":{"draw":"draw_circ_part","c":0xd7,"rnd":{"dr":[0.01,0.05],"ttl":[30,60]},"dy":0.2},"blast":{"draw":"draw_circ_part","dr":-0.5,"r":10,"rnd":{"debris":[8,12]},"ttl":16,"c":0x77},"debris":{"g":true,"update":"update_emitter","rnd":{"emit_dly":[2,8]},"emit_t":0,"emit_cls":"smoke"}}')
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

function draw_clouds(z,fp)
	--[[
	local x,y,w=cam_project(0,0,z)
	local dist=16
	local dx,dy=w*(cam_x%dist),w*(cam_y%dist)
	
	fillp(fp)
	y=-dy
	while y<127 do
		x=-dx
		while x<127 do
			circfill(64+x,64-y,w*8,13)
			circfill(64-x-2*dx,64-y,w*8,13)
			x+=w*dist
		end
		y+=w*dist
	end
	]]
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
function draw_actor(self,x,y)
	local s=self.frames[flr(self.frame)]
	rspr(s,32,16,self.angle,2)
	
	palt(14,true)
	palt(0,false)
	pal(6,self.in_sight and 8 or 0)
	palt(14,true)
	spr(36,x-8,y-8,2,2)

--	circ(x,y,	sqrt(256),7)
	for _,f in pairs(self.f) do
		local x1,y1=x+f[2],y-f[3]
		line(x,y,x1,y1,11)
		print(f[1],x1,y1-6,7)
	end
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

function arrive(self,pos)
end
function follow(self,other,dist)
	-- target point
	local x,y=other.x-dist*other.u,other.y-dist*other.v
	--return normalize(x-self.x,y-self.y)
	return x-self.x,y-self.y
end
function evade(self,other,dist)
	self.in_sight=false

	local dx,dy=other.x-self.x,other.y-self.y
	local d=dx*dx+dy*dy
	if d<dist*dist then
		d=sqrt(d)
		if abs(d)>0.001 then
			dx/=d
			dy/=d
		end
		local angle=other.u*dx+other.v*dy
		-- in cone?
		if angle<-0.9 then
			self.in_sight=true
			return -self.v,self.u
		end
	end
	return 0,0
end
function avoid(self,dist)
	local dx,dy=0,0
	for _,other in pairs(actors) do
		if other!=self then
			local ddx,ddy=other.x-self.x,other.y-self.y
			local d=ddx*ddx+ddy*ddy
			local scale=1-smoothstep(d/dist*dist)
			--[[
			d=sqrt(d)
			if abs(d)>0.001 then
				ddx/=d
				ddy/=d
			end
			]]
			dx-=scale*ddx
			dy-=scale*ddy
		end
	end
	-- avoid ground
	if self.y<32 then
		--dy+=-self.y/32
		dy+=abs(self.y)
	end
	return dx,dy
end

function control_npc(self)
	local fx,fy,dx,dy=0,0,0,0
	self.f={}
	fx,fy=evade(self,plyr,32)
	dx+=fx
	dy+=fy
	--add(self.f,{"evade",fx,fy})
	fx,fy=avoid(self,32)
	dx+=fx
	dy+=fy
	add(self.f,{"avoid",fx,fy})
	fx,fy=follow(self,plyr,32)
	dx+=fx
	dy+=fy
	--add(self.f,{"follow",fx,fy})
	
	-- project force into thrust normal
	-- pull/push based on sign
	dx,dy=normalize(dx,dy)
	local d=-self.v*dx+self.u*dy
	self.da=lerp(0.005,-0.01,smoothstep((d+1)/2))
end

function control_plyr(self)
	if not self.rolling then
		if(btn(2)) self.da=get_turn_rate(self,2)
		if(btn(3)) self.da=get_turn_rate(self,3)
		if btnp(0) or btnp(1) then
			self.rolling=true
			if self.inverted then
				self.df=-0.1
				self.target_frame=1
			else
				self.df=0.1
				self.target_frame=#self.frames
			end
		end
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
	a.x+=u*a.acc
	a.y+=v*a.acc

	if a.rolling then
		if flr(a.frame)!=a.target_frame then
			a.frame+=a.df
		else
			a.frame=a.target_frame
			a.rolling=false
			a.inverted=not a.inverted
		end
	end
	a.u=u
	a.v=v
	dx,dy=normalize(dx,dy)
	return u,v,dx,dy
end

function update_actor(a)
	if (a.disable) return false
	
	a:input()
	
	local u,v,dx,dy=move_actor(a)

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

local actor_cls={
 	dx=0,dy=0,
 	acc=0.8,
 	u=1,
 	v=0,
 	angle=0,
 	da=0,
 	frames={64,66,68,70,72,74,76},
 	frame=1,
 	df=0,
 	rolling=false,
 	inverted=false,
 	update=update_actor,
 	draw=draw_actor,
 	hit=hit
}
function make_actor(x,y)
	local a=clone(actor_cls,{x=x,y=y,f={}})
	return add(actors,a)
end


	
local game_screen={}
function game_screen:update()
	time_t+=1
	
	zbuf_clear()
	-- known bug: one frame delay
	cam_track(plyr.x,plyr.y)
	
	filter(actors)
	filter(parts)
	--filter(clouds)

	cam_update()
end

function game_screen:draw()
	cls(6)

	--ground
	local x,y,w=cam_project(0,0,12)
	draw_ground({},x,y,w)
	
	draw_clouds(8)
	zbuf_draw()
	draw_clouds(0,0b1010010110100101.1)
	-- draw hud
	if lead and not lead.visible then
		local x,y=cam_project(lead.x,lead.y,0)
		x-=plyr.x
		y-=plyr.y
		local angle=atan2(x,y)
		x,y=cos(angle),sin(angle)

 	line(64+8*x,64+8*y,64+10*x,64+10*y,13)
	end
	
	--[[
	local mem=0x6000+flr(y)*64
	for j=y,127 do
		for i=0,15 do
		 poke4(mem,shades[peek4(mem)])
			mem+=4
		end
	end
	]]
	
	circ(12+1,117,10,0)
	circ(12,117,10,13)
	
	local angle=(time_t%128)/128
	angle=lerp(0.2,0.8,angle)-0.2
	x,y=6*cos(angle),-6*sin(angle)
	line(12+1,117,12+x+1,117-y,0)
	line(12,117,12+x,117-y,13)
	
	fillp()
	rectfill(0,0,127,8,1)
	print((flr(1000*stat(1))/10).."%",2,2,7)
	print(track_section,100,2,7)
end

function game_screen:init()
	for i=1,10 do
		local x,y=rnd(128)-64,rnd(128)+12
		local z=flr(rnd(2))+1
		for j=1,flr(rnd(4)) do
			local r=rnd(8)+4
			make_cloud(x+r/2,y+rnd(2)-1,r,z)
			x+=r
		end
	end

	lead=make_actor(24,24)
	lead.input=control_npc
	lead.npc=true
	
	lead=make_actor(-24,32)
	lead.input=control_npc
	lead.npc=true
	
	plyr=make_actor(0,24)
	plyr.input=control_plyr
	plyr.score=0
end

cur_screen=game_screen
function _draw()
	cur_screen:draw()
	time_dt=0
end
function _update60()
	time_dt+=1
	cur_screen:update()
end
function _init()
	if cur_screen.init then
		cur_screen:init()
	end
end

__gfx__
00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00000000eeeeeeeeeeeeeeeeee5555eee555555eeeeeee5eeeeeeeeeeee55eeeeee55eeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
00700700eee55eeee555555ee5eeee5eee5ee5eeeeeee55eee5ee5eeeee55eeeee5555eeee5555eeeeee5eeeeeeeeeee00000000000000000000000000000000
00077000ee5555eeee5555eee5ee5eeeeee55eeee55e55eeeee55eeeee5555eeeee55eeeee5555eeeee555eeeee555ee00000000000000000000000000000000
00077000e555555eeee55eeee5ee55eeee5555eeee555eeeeee55eeeeee55eeeeee55eeeee5555eeeeee5eeeeeeeeeee00000000000000000000000000000000
00700700eeeeeeeeeeeeeeeeee55555ee555555eeee5eeeeee5ee5eee5eeee5ee5eeee5eee5555eee5eeee5ee5eeee5e00000000000000000000000000000000
00000000eeeeeeeeeeeeeeeeeeee55eeeeeeeeeeeeeeeeeeeeeeeeeee555555ee555555eeeeeeeeee555555ee555555e00000000000000000000000000000000
00000000eeeeeeeeeeeeeeeeeeee5eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
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
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0101010101040404030504040202020405010101010504040401050301010504040404040301010504040405030101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0101010300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0404040404040105040404040403050101010504040401040504040404040403050101050404040501000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
