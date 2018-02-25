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

local time_t=0
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
	shadepix_xy(x+1,y,dx)
	shadepix_xy(x-1,y,1-dx)
	shadepix_xy(x,y-1,1-dx)
	shadepix_xy(x,y+1,dx)
end

local actors={}
local plyr
function make_plyr(x,y)
	return add(actors,{
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
			if dx*dx+dy*dy<64 then
				a.hp-=1
				if a.hp<=0 then
					make_blast(a.x,a.y)
					del(actors,a)
				else
					make_part(self.x,self.y,0,0,0,flash_part)
				end
				return false
			end
		end
	end
	return true
end

function make_rock()
	local radius,n={},2*(2+flr(rnd(2)))
	local angle,da=0,1/n
	for i=1,n do
		local r=4+4*rnd()
		local y,x=r*cos(angle),-r*sin(angle)
		add(radius,{x=x,y=y})
		angle+=da
	end
	local angle,acc=rnd(),rnd()/2
	local u,v=cos(angle),-sin(angle)
		
	add(actors,{
		hp=3,
		x=64+48*u,
		y=64-48*v,
		acc=acc,
		u=-u,
		v=v,
		a=rnd(),
		da=rnd()/64,
		segments=radius,
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
	if self.x>110 or self.x<12 then
		self.x=mid(self.x,12,110)
		self.u=-self.u
	end
	if self.y>110 or self.y<12 then
		self.y=mid(self.y,12,110)
		self.v=-self.v
	end
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

function _draw()
	cls()
	
	forall(actors,"draw")
	forall(parts,"draw")

	rectfill(0,0,127,8,1)
	print("cpu:"..flr(100*stat(1)).."%",2,2,15)
end

function _init()
		graypal()
		
		plyr=make_plyr(64,64)
		
		for i=1,5 do
			make_rock()
		end
end
