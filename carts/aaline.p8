pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- aaline 1.2f
-- by freds72, tweaked by felice
local ramp={[0]=0,1,1,5,5,13,13,13,6,6,6,7,7,7,7,7}

-- shade the pixel
-- abs(dist) indicates how far
-- we are from the exact
-- line center
function shadepix_xy(x,y,dist)
	pset(x,y,0xf.ffff-abs(dist*(0xf.ffff-pget(x,y))))
end

function shadepix_yx(y,x,dist)
	pset(x,y,0xf.ffff-abs(dist*(0xf.ffff-pget(x,y))))
end

-- algorithm credits:
-- http://jamesarich.weebly.com/uploads/1/4/0/3/14035069/480xprojectreport.pdf
function aaline(x0,y0,x1,y1)
	if x1!=x2 or y1!=y2 then
 	local	du,dv=x1-x0,y1-y0
 	local u,v,uincr,vincr,shadepix
 	local	mu,mv=abs(du),abs(dv)
 	if mu>=mv then
 		u,v,
 		uincr,vincr,
 		du,dv,
 		shadepix
 		=
 		x0,y0,
 		sgn(du),sgn(dv),
 		mu,mv,
 		shadepix_xy
 	else
 		u,v,
 		uincr,vincr,
 		du,dv,shadepix
 		=
 		y0,x0,
 		sgn(dv),sgn(du),
 		mv,mu,
 		shadepix_yx
 	end
 
 	local incrs=2*dv
 	local d=incrs-du
 	local incrd=d-du
 	
 	local twovdu=0
 	local twodist=2*sqrt(du*du+dv*dv)
 	local invd2du=2*du/twodist
 	
 	for i=u,u+du do
 		local vdu_over_dist=twovdu/twodist
 		shadepix(u,v,          vdu_over_dist)
 		if vdu_over_dist<0 then
 			shadepix(u,v-vincr,invd2du+vdu_over_dist)
 		else
 			shadepix(u,v+vincr,invd2du-vdu_over_dist)
 		end
 		if d<0 then
 			twovdu=d+du
 			d+=incrs
 		else
 			twovdu=d-du
 			d+=incrd			
 			v+=vincr
 		end
 		u+=uincr
 	end
 end
end

function aacircfill(x0,y0,r)
	if(r==0) return
 local x,y,dx,dy=flr(r),0,1,1
 r*=2
 local err=dx-r

	local j=0
 while x>=y do
		local dist=1+err/r
		rectfill(x0-x+1,y0+y,x0+x-1,y0+y,15)
		rectfill(x0-x+1,y0-y,x0+x-1,y0-y,15)
		rectfill(x0-y,y0-x+1,x0+y,y0-x+1,15)
		rectfill(x0-y,y0+x-1,x0+y,y0+x-1,15)
	 shadepix_xy(x0+x,y0+y,dist)
  shadepix_xy(x0+y,y0+x,dist)
  shadepix_xy(x0-y,y0+x,dist)
  shadepix_xy(x0-x,y0+y,dist)
  shadepix_xy(x0-x,y0-y,dist)
  shadepix_xy(x0-y,y0-x,dist)
  shadepix_xy(x0+y,y0-x,dist)
  shadepix_xy(x0+x,y0-y,dist)
 
	 if err<=0 then
   y+=1
   err+=dy
   dy+=2
		end  
	 if err>0 then
   x-=1
   dx+=2
   err+=dx-r
		end
	end
end

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

function filter(array,fn)
	for _,a in pairs(array) do
		if not a[fn](a) then
			del(array,a)
		end
	end
end
function forall(array,fn)
	for _,a in pairs(array) do
		a[fn](a)
	end
end

function lerp(a,b,t)
	return a*(1-t)+b*t
end

local time_t=0
local fonts={
	["0"]={{0,0,1,0},{1,0,1,1},{1,1,0,1},{0,1,0,0}},
	["1"]={{0.5,0,0.5,1}},
	["2"]={{0,0,1,0},{1,0,1,0.5},{1,0.5,0,0.5},{0,0.5,0,1},{0,1,1,1}},
	["3"]={{0,0,1,0},{1,0,1,1},{1,1,0,1},{0,0.5,1,0.5}},
	["4"]={{0,0,0,0.5},{0,0.5,1,0.5},{1,0,1,1}},
	["5"]={{1,0,0,0},{0,0,0,0.5},{0,0.5,1,0.5},{1,0.5,1,1},{1,1,0,1}},
	["6"]={{1,0,0,0},{0,0,0,1},{0,0.5,1,0.5},{1,0.5,1,1},{1,1,0,1}},
	["7"]={{0,0,1,0},{1,0,1,1}},
	["8"]={{0,0,1,0},{1,0,1,1},{1,1,0,1},{0,1,0,0},{0,0.5,1,0.5}},
	["9"]={{1,0,0,0},{0,0,0,0.5},{0,0.5,1,0.5},{1,0,1,1}}
}
local pixel_part=0
local flash_part=1
local parts={}
function make_part(x,y,u,v,f,typ)
	local ttl,draw,r,dr
	if typ==flash_part then
		draw=draw_circ_part
		ttl=24
		r=4
		dr=-0.5
	else
		ttl=24+rnd(4)-8
		draw=draw_part
	end
	return add(parts,{
		x=x,
		y=y,
		u=u,
		v=v,
		f=f,
		r=r,
		dr=dr,
		inertia=0.98,
		t=time_t+ttl,
		ttl=ttl,
		draw=draw,
		update=update_part
	})
end
function make_blt(x,y,u,v)
	local ttl=60+rnd(12)
	return add(parts,{
		x=x,
		y=y,
		u=u,
		v=v,
		f=1.2,
		inertia=1,
		t=time_t+ttl,
		ttl=ttl,
		draw=draw_blt,
		update=update_part,
		collide=collide_blt
	})	
end
function make_blast(x,y)
	local ttl=12
	add(parts,{
		x=x,
		y=y,
		u=0,
		v=0,
		f=0,
		r=12,
		dr=-1,
		inertia=1,
		t=time_t+ttl,
		ttl=ttl,
		draw=draw_circ_part,
		update=update_part
	})
	for i=0,8 do
		local angle=rnd()
		local u,v=cos(angle),sin(angle)
		make_part(x+8*u,y+8*v,u,v,rnd())		
	end
	cam_shake(rnd(),rnd(),5)
end

function update_part(self)
	if(self.t<time_t) return false
	self.x+=self.f*self.u
	self.y+=self.f*self.v
	self.f*=self.inertia
	
 self.x%=128
	self.y%=128

	if self.r then
		self.r+=self.dr
	end
	--	custom update function?
	if self.collide then
		return self:collide()
	end
	return true
	--return self.collide and  or true
end

function draw_circ_part(self)
	aacircfill(self.x,self.y,self.r)
end

function draw_part(self)
 pset(self.x,self.y,15)
 local d=0.75*self.f
 shadepix_xy(self.x+1,self.y,d)
 shadepix_xy(self.x,self.y+1,d)
 shadepix_xy(self.x-1,self.y,d)
 shadepix_xy(self.x,self.y-1,d)
end

function draw_blt(self)
	local x,y=self.x,self.y
	pset(x,y,15)
	local dx,dy=x-flr(x),y-flr(y)
	-- kind of unit circle dithering
	shadepix_xy(x+1,y,dx)
	shadepix_xy(x-1,y,1-dx)
	shadepix_xy(x,y-1,dx)
	shadepix_xy(x,y+1,1-dx)
end

local actors={}
local plyr
function make_plyr(x,y)
	return add(actors,{
		score=0,
		combo_mult=0,
		combo_t=0,
		r=4,
		x=x,
		y=y,
		a=0.25,
		da=0,
		u=0,
		v=1,		
		f=0,
		acc=0.5,
		emit_t=0,
		fire_t=0,
		update=update_plyr,
		draw=draw_plyr
	})
end

function collide_blt(self)
	for _,a in pairs(actors) do
		-- rock?
		if a!=plyr then
			local dx,dy=a.x-self.x,a.y-self.y
			if dx*dx+dy*dy<a.r*a.r then
				a.hp-=1
				if a.hp<=0 then
					plyr.score+=1
					plyr.combo_t=time_t+30
					
					make_blast(a.x,a.y)
					del(actors,a)
			
					-- spawn mini rocks
					local r=a.r/2
					if r>2 then
 					local angle,da=rnd(),1/3
    		for i=1,3 do
    			local u,v=cos(angle),-sin(angle)
    			make_rock(a.x+r*u,a.y-r*v,u,v,r,6)
    			angle+=da
    		end
					end
				else
					make_part(self.x,self.y,0,0,0,flash_part)
				end
				return false
			end
		end
	end
	return true
end

function make_rock(x,y,u,v,radius,n)
	local angle,da=0,1/n
	local segments={}
	for i=1,n do
		local r=lerp(radius*0.8,radius*1.2,rnd())
		local y,x=r*cos(angle),-r*sin(angle)
		add(segments,{x=x,y=y})
		angle+=da
	end
		
	add(actors,{
		hp=3,
		x=x,
		y=y,
		acc=0.25+0.25*rnd(),
		u=-u,
		v=v,
		a=rnd(),
		da=rnd()/64,
		r=radius, -- keep initial radius
		segments=segments,
		draw=draw_rock,
		update=update_rock
	})
end

function rotate(x,y,c,s)
	return x*c-y*s,x*s+y*c
end
function draw_rock(self)	
	local u,v=cos(self.a),-sin(self.a)
	local r=self.segments[1]
	local rx,ry=rotate(r.x,r.y,u,v)
	local x0,y0,x1,y1=self.x+rx,self.y+ry
	local x2,y2=x0,y0
	for i=1,#self.segments do
		r=self.segments[i%#self.segments+1]
		rx,ry=rotate(r.x,r.y,u,v)
		x1,y1=self.x+rx,self.y+ry
		aaline(x0,y0,x1,y1)
		x0,y0=x1,y1
	end
end
function update_rock(self)
	self.a+=self.da
	self.x+=self.acc*self.u
	self.y+=self.acc*self.v
	
	self.x%=128
	self.y%=128
	return true
end

function control_plyr(self)
	if(btn(0)) self.da=-0.01
	if(btn(1)) self.da=0.01
	local thrust=false
	if(btn(4)) self.f=self.acc thrust=true
	local fire=false
	if(btn(5)) fire=true
	
	if fire and self.fire_t<time_t then
		self.fire_t=time_t+8
		make_blt(self.x,self.y,self.u,self.v)
	end
	
	if thrust and self.emit_t<time_t then
		self.emit_t=time_t+rnd(3)
		local emit_v=0.5+rnd()
		make_part(self.x-0.5*self.u,self.y-0.5*self.v,-self.u,-self.v,emit_v)
		make_part(self.x-4*self.u,self.y-4*self.v,-self.u,-self.v,0,flash_part)
	end
end

function update_plyr(self)
	self.a+=self.da	
	self.da*=0.90
	
	self.u=cos(self.a)
	self.v=-sin(self.a)
	
	self.x+=self.f*self.u
	self.y+=self.f*self.v

	self.x%=128
	self.y%=128
	
	self.f*=0.96
	
	return true
end

function draw_plyr(self)
	local a,r=self.a,self.r
	local x0,y0=self.x+r*cos(a),self.y-r*sin(a)
	a+=1/3
	local x1,y1=self.x+r*cos(a),self.y-r*sin(a)
	a+=1/3
	local x2,y2=self.x+r*cos(a),self.y-r*sin(a)
	
	aaline(x0,y0,x1,y1)			
	aaline(x0,y0,x2,y2)
 aaline(x1,y1,x2,y2)
end

local font_scale=8
function draw_char(x,y,c)
	local font=fonts[c]
	if(not font) assert("unsupported char:"..c)
	for _,seg in pairs(font) do
		aaline(
			x+font_scale*seg[1],
			y+font_scale*seg[2],
			x+font_scale*seg[3],
			y+font_scale*seg[4],15)
	end
end

function _update60()
	time_t+=1
	
	control_plyr(plyr)

	filter(actors,"update")
	filter(parts,"update")	
	
	cam_update()
end

-- pico-8 resets the palette
-- when you stop the app, so
-- just run one of these as
-- needed.
function graypal()
	for i=0,15 do
		pal(i,ramp[i],1)
	end
end
function normpal()
	for i=0,15 do
		pal(i,i,1)
	end
end

local stars={}
function _draw()
	cls(0)
	
	-- scanline effect	
	local mem=0x6000
	for i=0,127 do
		local c=band(i,1)
		memset(mem,bor(shl(c,4),c),64)
		mem+=64
	end
	
	for _,s in pairs(stars) do
		pset(s.x,s.y,15*s.c)
	end
	
	forall(actors,"draw")
	forall(parts,"draw")

	-- score
	local score=tostr(plyr.score)
	local x=2
	for i=1,6-#score do
		draw_char(x,12,"0")
		x+=10
	end
	for i=1,#score do
		local c=sub(score,i,i)
		draw_char(x,12,c)
		x+=10
	end
	-- cheap crt effect
	if rnd()>0.5 then
		-- avoid memcpy overflow
		local i=flr(rnd(126))
		local src,dst=0x6000+i*64,0x6000+i*64+2+flr(rnd(2))
		memcpy(dst,src,64)
	end
	
	rectfill(0,0,127,8,1)
	print("cpu:"..flr(100*stat(1)).."%",2,2,15)
end

function _init()
		graypal()
		
		plyr=make_plyr(64,64)
		
		for i=1,32 do
			add(stars,
				{x=rnd(127),
				 y=rnd(127),
				 c=rnd()})
		end
		
		local angle=rnd()
		for i=1,5 do
			local u,v=cos(angle),-sin(angle)
			make_rock(64+48*u,64-48*v,u,v,8,8)
			angle+=1/5
		end
end
