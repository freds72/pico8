pico-8 cartridge // http://www.pico-8.com
version 8
__lua__
local cam={}
-- active buildings
local blds={}
local blds_c=0
-- floor constants
flr_n=6
flr_h=8
flr_w=24
flr_zmax=(flr_n-1)*flr_h
flr_ramp={6,5}
-- camera
hh=63
hw=64
-- player "thunderblade" settings
local plyr={
	x=0,
	y=0,
	z=16,
	zi=1,
	vx=0,
	vy=1,
	lives=3,
	hit=false,
	safe_dly=0,
	side=0
}
plyr_zmax=56
plyr_r=12
plyr_r2=plyr_r*plyr_r
plyr_rotor={128,131,134}
plyr_body={64,66,68}
plyr_body3d={204,200,202}
plyr_rotor3d={
	[1]={250,{-10,5,true},{8,-2,false,true}},
	[2]={248,{-10,2,false,false},{8,2,true,false}},
	[3]={250,{-8,-2,true,true},{8,5}}
}
-- global time
local time_t=0
-- bullets
local blts={}
blts_c=0
blts_n=32
blt_w=4
blt_r=blt_w
blt_r2=blt_w*blt_w
-- zbuffer
local zbuf={}
-- tanks
local tks={}
tks_c=0
tk_w=16
tk_h=24
tk_chase_w=16
tk_chase_h=16
-- helos
local helos={}
helos_c=0
helo_w=24
helo_h=32
helo_r=12
helo_r2=helo_r*helo_r
helo_body={64,88}
-- map
local world={
	scaley=96, -- 1 cell=1 half screen
	scalex=32,
	ymax=0,
	cur=0,
	funcs={}, -- entity spawners
	road={}, -- road x-offset
	floor={} -- floor color ramps
}
local map_funcs={}
local road_offset={}

-- special fx
local fxs={}

-- boss
local boss={
	enabled=false
}

-- fade ramp + screen manager
_shex={["0"]=0,["1"]=1,
["2"]=2,["3"]=3,["4"]=4,["5"]=5,
["6"]=6,["7"]=7,["8"]=8,["9"]=9,
["a"]=10,["b"]=11,["c"]=12,
["d"]=13,["e"]=14,["f"]=15}
_pl={[0]="00000015d67",
     [1]="0000015d677",
     [2]="0000024ef77",
     [3]="000013b7777",
     [4]="00009a777",
     [5]="000015d6777",
     [6]="0015d677777",
     [7]="015d6777777",
     [8]="000028ef777",
     [9]="000249a7777",
    [10]="00249a77777",
    [11]="00013b77777",
    [12]="00013c77777",
    [13]="00015d67777",
    [14]="00024ef7777",
    [15]="0024ef77777"}
_pi=0-- -100=>100, remaps spal
_pe=0-- end pi val of pal fade
_pf=0-- frames of fade left
function fade(from,to,f)
 _pi=from _pe=to _pf=f
end
-- screen manager
local sm={
	t=0,cur=nil,next=nil,dly=0}
function sm:push(s)
	if(self.cur) then
		self.dly=self.t+8
		self.next=s
		fade(0,-100,8)
	else
		self.cur=s
		self.cur:init()
	end
end
function sm:update()
	self.t+=1
 if(self.next) then 
 	if (self.dly<self.t) then
			self.cur=self.next
			self.cur:init()
			self.next=nil
			fade(0,0,8)
		end
	else
	 self.cur:update()
	end
	if(_pf>0) then --pal fade
		if(_pf==1) then _pi=_pe
		else _pi+=((_pe-_pi)/_pf) 
		end
 		_pf-=1
	end
end
function sm:draw()
	self.cur:draw()
	local pix=6+flr(_pi/20+0.5)
	if(pix!=6) then
		for x=0,15 do
			pal(x,_shex[sub(_pl[x],pix,pix)],1)
		end
	else 
		pal() 
	end
end

-- decompress pic --
-- written by dw817 (david w)
-- http://writerscafe.org/dw817
set="abcdefghijklmnopqrstuvwxyz()[]{}"
function str2mem_cor(t,m)
	local b1,b2,c,n,p=0,0,0,0,1
	repeat
		if b1==0 then
			n=instr(set,sub(t,p,p))-1
		end
		if band(n,2^b1)>0 then
			c+=2^b2
		end
		b1+=1
		if (b1==5) b1=0 p+=1
		b2+=1
		if b2==8 or p>#t then
			poke(m,c)
			b2=0 c=0 m+=1
		end
		if(band(m,255)==0) then 
			yield(m)
		end
	until p>=#t
end--str2mem

-- decompress dots to string
function dot2str(t)
	local i,c,ch,n1,n2
	local r,l1="",0
	i=1 
	while i<=#t do
		c=sub(t,i,i)
		if c=="." then
			i+=1
			ch=sub(t,i,i)
			repeat
				i+=1
				c=sub(t,i,i)
				if (c!=".") l1=l1..c 
			until c=="." or i==#t
			l2=l1+0 --clumsy!
			for j=1,l2 do
				r=r..ch
			end
			l1=""
		else
			r=r..c
		end
		i+=1 
	end--wend
	return r
end--dots2str

-- return pos # of str b in a
function instr(a,b)
	local r=0,0
	for i=1,#a-#b+1 do
		if (sub(a,i,i+#b-1)==b) r=i
	end
	return r
end--instr

---------------------------
--- collision helpers -----
function circ_coll(a,ra,b,rb,r2)
	local dz=b.z-a.z
	if(abs(dz)>ra+rb) return
	local dx,dy=b.x-a.x,b.y-a.y
	local dist=ra+rb
	if(abs(dx)>dist or abs(dy)>dist) return
	local dist2=r2 or dist*dist
	return (dx*dx+dy*dy<dist2)
end

function circbox_coll(a,ra,b,bw,bh,bz)
	local dz=b.z-a.z
	if(abs(dz)>bz+ra) return
	local dx,dy
	dx=a.x-mid(a.x,b.x-bw,b.x+bw)
	dy=a.y-mid(a.y,b.y-bh,b.y+bh)
	if(abs(dx)>ra or abs(dy)>ra) return
	local d2=dx*dx+dy*dy
	return (d2<ra*ra)
end
function box_coll(a,ah,aw,b,bw,bh)
end

---------------------------
-- gfx functions
function smap(cx,cy,cw,ch,x,y,scale)
	scale=shl(scale,3)
	--x-=cw*shr(scale,1)
	y-=ch*shr(scale,1)
	local s,sx,sy,ixl,ixr
	for i=0,cw-1 do
		ixl=x+(i-cw)*scale
		ixr=x+(cw-1-i+1)*scale
		for j=0,ch-1 do
			s=mget(cx+i,cy+j)
			sx,sy=band(shl(s,3),127),shl(flr(s/16),3)
			sspr(sx,sy,8,8,
				ixl,y+j*scale,scale,scale)
			sspr(sx,sy,8,8,
				ixr,y+j*scale,-scale,scale)
		end
	end
end

-----------------------
-- print text helper
printer={
	center=false,
	shade=-1
}
function printer:print(s,x,y,col)
	if(self.center) then
		x-=flr((4*#s)/2+0.5)
	end
	if(self.shade!=-1) then
		print(s,x+1,y,self.shade)
	end
	print(s,x,y,col)
	self.center=false
	self.shade=-1
end
function printer:centered()
	self.center=true
	return self
end
function printer:shaded(col)
	self.shade=col
	return self
end
-- 
function nop()
end

---------------------------
-- world
function world:init(sx,sy,sw)
	self.road_ymax=(sw+1)*self.scaley
	for i=1,16 do
		self.funcs[i]=nop
	end
	for i=0,sw-1 do
		local noroad=true
		for j=0,7 do
			if(sget(sx+i,sy+j)==5) then
				self.road[i]=(j+1-4)*self.scalex
				noroad=false
				break
			end
		end
		if(noroad) then
			self.road_ymax=(i-1)*self.scaley
			break
		end
	end
	-- floor color ramps
	local r={0x33,0x33,0xb3,0x3b}
	for i=0,flr(sw/8+0.5) do
		local c1=sget(sx+8*i,sy)
		if(c1!=0) then
			local c2=sget(sx+8*i+1,sy)
			r={
				bor(shl(c1,4),c1),
				bor(shl(c1,4),c1),
				bor(shl(c2,4),c1),
				bor(shl(c1,4),c2)}
		end
		self.floor[i+1]=r
	end
	-- debug
	self.print=function(self,x,y)
		palt(0,false)
		sspr(sx,sy,sw,8,x,y)
		line(
			x+self.cur,y,
			x+self.cur,y+8,8)
		palt()
		y+=8
		rectfill(x,y,x+48,y+40,0)
		for ramp in all(self.floor) do
			print(ramp[1]..ramp[2].."/"..ramp[3]..ramp[4],x,y,7)
			y+=6
		end
	end
	return self
end
function world:register(i,fn)
	self.funcs[i]=fn
	return self
end
function world:roadx(y)
	if(y<0 or y>=self.road_ymax) return
	local t=y/self.scaley
	local i=flr(t)
	-- fractional part
	t-=i
	return lerp(
		self.road[i],
		self.road[i+1],
		smoothstep(t))
end
function world:floor_ramp(y)
	y=flr(shr(max(0,y),3)/self.scaley)
	return self.floor[y+1]
end
function world:update()
	-- pick world items
	local plyr_cur=flr((plyr.y+196)/self.scaley)
	if (self.cur<plyr_cur) then
		local i=0
		for i=self.cur,plyr_cur do
			for j=0,7 do
				self.funcs[sget(i,56+j)+1]((j+1-4)*self.scalex,i*self.scaley)
			end
		end
		self.cur=plyr_cur+1
	end
end
---------------------------
-- camera
function cam:init(focal,zfar,beta)
	self.x=0
	self.y=0
	self.z=0
	self.focal=focal
	self.zfar=zfar
	self.beta=beta or 0
		-- pico8 trig is inverted!
	self.alpha=1-atan2(focal,hh)
	self.cb=cos(self.beta)
	self.sb=-sin(self.beta)
end
function cam:rotate(beta)
	cam.beta=beta
	self.cb=cos(self.beta)
	self.sb=-sin(self.beta)
end
function cam:is_top_down()
	return self.beta>0.6 and self.beta<0.9
end
function cam:project(pos)
	local cb,sb=self.cb,self.sb
	local y=pos.y-self.y
	local z=pos.z-self.z
	local ze=-(y*cb+z*sb)
	-- invalid projection?
	if(ze<self.zfar or ze>=0) return nil,nil,z,nil
	local w=-self.focal/ze
	local xe=pos.x-self.x
	local ye=-y*sb+z*cb
	return hw+xe*w,hh-ye*w,ze,w
end
function cam:track(pos)
	self.x=pos.x
	self.y=pos.y-self.cb*self.focal
	self.z=pos.z-self.sb*self.focal
end

--------------------
--- tank -----------
tk_class={}
function tk_draw(self,x,y,z,w)
	local we,he=tk_w*w,tk_h*w
	sspr(0,8,16,24,x-we/2,y-we/2,we,he)
end
function tk_chase_draw(self,x,y,z,w)
	local we,he=tk_chase_w*w,tk_chase_h*w
	sspr(104,16,16,16,x-we/2,y-we/2,we,he)
end
function tk_blt_collision(self,blt)		
 if(circbox_coll(blt,blt_r,self,tk_w,tk_h,2)) then
		self.hit=1
		fxs:make_blast(self.x,self.y,self.z,0,0,4,1)
		sfx(1)
		return true
	end
	return false
end

function spawn_tk(x,y)
	local tk={
		x=x,
		y=y,
		z=0,
		dly=2*30+rnd(2*30),
		hit=0,
		side=1,
		blt_hit=tk_blt_collision,
	}
	if(cam:is_top_down()) then
		tk.draw=tk_draw
	else
		tk.draw=tk_chase_draw
	end
	tks_c+=1
	tks[tks_c]=tk
	return tk
end

---------------------
-- special effects
function blast_draw(self,x,y,z,w)
	local s=(self.t-time_t)/3
	local we=s*self.width*w
	sspr(24,16,16,16,x-we/2,y-we/2,we,we,time_t%2==0,time_t%4==0)
end
function blast_update(self)
			self.y+=self.dy
			self.z+=self.dz
end
function smoke_draw(self,x,y,z,w)
	local t=(self.t-time_t)/self.dly
	local we=flr(t*8*w)+1
	t=min(flr(8-t*8),7)
	circfill(x,y,we,sget(8+t,0))
end
function fxs:update()
	for b in all(fxs) do
		if(b.t<time_t) then
			del(blasts,b)
		else
		 b:update()
			zbuf:write(b)
		end
	end
end
function fxs:make_blast(x,y,z,dy,dz,w,tmax)
	local b={
		x=x,
		y=y,
		z=z,
		dy=dy or 0,
		dz=dz or 0,
		t=time_t+(tmax or 0.2)*30+rnd((tmax or 0.4)/2),
		width=w or 32,
		update=blast_update,
		draw=blast_draw
	}
	add(fxs,b)
	return b
end
function fxs:make_smoke(x,y,z)
	local dly=0.5*30+rnd(10)
	local b={
		x=x,
		y=y,
		z=z,
		dly=dly,
		t=time_t+dly,
		update=nop,
		draw=smoke_draw
	}
	add(fxs,b)
	return b
end

---------------------
--- player ----------
function plyr:init()
	self.x=0
	self.y=0
	self.z=plyr_zmax
	self.lives=3
	self.hit=false
	self.safe_dly=time_t+3*30
	self.fire_dly=0
end
function plyr:draw(x,y,z,w)
	if(self.safe_dly>time_t and band(time_t,1)==0) return
	local idx=mid(flr(self.vx),-1,1)+2
	if(cam:is_top_down()) then
		spr(plyr_body[idx],x-8,y-9,2,3)
		spr(plyr_rotor[time_t%3+1],x-10,y-11,3,3)
	else
		local pos=plyr_rotor3d[idx][band(time_t,1)+2]
		spr(plyr_rotor3d[idx][1],x-8+pos[1],y+pos[2],2,1,pos[3],pos[4])
		spr(plyr_body3d[idx],x-8,y,2,3)
	end
end
function plyr:die()
	sfx(1)
	self.lives-=1
	if(self.lives<=0) then
		sm:push(game_over)
	end
	self.safe_dly=time_t+3*30
end
function plyr:blt_hit(blt)
	if(circ_coll(self,plyr_r,blt,blt_r)) then
		self:die()
		return true
	end
	return false
end
function plyr:update()
	local dx=0
	if (btn(0)) dx=-0.25
	if (btn(1)) dx=0.25
	if(dx==0) then
		self.vx*=0.89
		if(abs(self.vx)<0.3) self.vx=0
	else
		self.vx=mid(self.vx+dx,-4,4)
	end
	self.x=mid(self.x+self.vx,-64,64)
	
	if (btn(2)) self.z-=0.5
	if (btn(3)) self.z+=0.5
	self.z=mid(self.z,0,plyr_zmax)

	if(btn(5)) then
		self.vy-=0.25
	else
		self.vy+=max(0.1, 0.1*self.vy)
	end
	self.vy=mid(self.vy,0,1)
	self.y+=self.vy

	-- debug
	--if(btn(5)) then
	--	cam.beta+=0.01
	--	if(cam.beta>1) cam.beta=0
	--end
	
	-- fire
	if (btn(4) and self.fire_dly<=time_t and blts_c<blts_n) then
		blts:make(self.x,self.y+5,self.z-0.5,3,-3,self.side)
		self.fire_dly=time_t+0.2*30
	end

	-- cam world position
	cam:track({
		x=self.x,
		y=self.y+24,
		z=self.z})
	self.zi=zbuf:write(self,self)
end
function plyr:resolve_collisions()
	-- just (re)spawned
	if (self.safe_dly>=time_t) return
	-- against buildings
	if (self.z<=flr_zmax) then
		local b
		for i=1,blds_c do
			b=blds[i]
			if(circbox_coll(self,plyr_r,b,flr_w,flr_w,flr_zmax)) then
				self:die()
				b.touch=1
				return
			end
		end
	end
	-- against stuff
	local zb=zbuf[plyr.zi]
	for i=1,zb.o do
		local o=zb.objs[i]
		if (o.plyr_hit and o:plyr_hit(self)) then
			self:die()
			return
		end
	end
end

------------
--- helo ---
helo_class={}
function helo_draw(self,x,y,z,w)
	local we,he=helo_w*w,helo_h*w
	sspr(helo_body[band(time_t,1)+1],32,24,32,x-we/2,y-he/2,we,he)
end
function helo_chase_draw(self,x,y,z,w)
	local we,he=helo_w*w,helo_h*w
	sspr(104,0,16,16,x-8*w,y-8*w,16*w,16*w)
	--[[
	if(time_t%2==0) then
		sspr(64,120,16,8,pos.x-8*pos.w,pos.y-4*pos.w,16*pos.w,16*pos.w)
		sspr(64,120,16,8,pos.x-16*pos.w,pos.y-4*pos.w,16*pos.w,16*pos.w,true)	
	end
	]]
end
helo_blt_r2=(blt_r+helo_r)*(blt_r+helo_r)
helo_plyr_r2=(plyr_r+helo_r)*(plyr_r+helo_r)
function helo_die(self)
	self.hit=1
	fxs:make_blast(self.x,self.y,self.z,0,-32/30,4,2)
	sfx(1)
end

function spawn_helo(x,y)
	local h={
		x=x,y=y,z=0,
		dy=0,dz=24/30,
		dly=time_t+1.8*30,
		die=helo_die,
		side=1,
		hit=0}
	if(cam:is_top_down()) then
		h.draw=helo_draw
		h.blt_hit=function(self,blt)
			if(circ_coll(self,helo_r,blt,blt_r,helo_blt_r2)) then
				self:die()
				return true
			end
			return false
		end
		h.plyr_hit=function(self,p)
			if(circ_coll(self,helo_r,plyr,plyr_r,helo_plyr_r2)) then
				self:die()
				return true
			end
			return false
		end
	else
		h.draw=helo_chase_draw
	end
	helos_c+=1
	helos[helos_c]=h
	return h
end

-------------
--- building ---
bld_class={}
function flr_draw(self,xe,ye,z,w)
	local we=flr_w*w
	if(self.i==1) then
		-- shadow
		local se=1.2*we
		rectfill(xe-se,ye-se,xe+se,ye+se,3)
	end
	local x,y=xe-we,ye-we
	local c=flr_ramp[band(self.i,1)+1]
	local k=flr(we/4+0.5)
	we*=2
	if(ye>64) then
		rectfill(x,y,x+we,y+k,c)
	else
		rectfill(x,y+we-k,x+we,y+we,c)
	end
	if(xe<64) then
		rectfill(x+we-k,y,x+we,y+we,c)
	else
		rectfill(x,y,x+k,y+we,c)
	end
end
function roof_draw(self,x,y,z,w,i)
	local we=flr_w*w
	sspr(40,0,32,32,x-we,y-we,2*we,2*we)
end
function flr_chase_draw(self,xe,ye,z,w)
	local we=flr_w*w
	local x,y=xe-we,ye-we
	local c=flr_ramp[band(self.i,1)+1]
	local k=flr(we/4+0.5)
	we*=2
	if(ye>64) then
		rectfill(x,y,x+we,y+k,c)
	else
		rectfill(x,y+we-k,x+we,y+we,c)
	end
	if(xe<64) then
		rectfill(x+we-k,y,x+we,y+we,c)
	else
		rectfill(x,y,x+k,y+we,c)
	end
end
function roof_chase_draw(self,x,y,z,w,i)
	local we=flr_w*w
	sspr(72,0,32,32,x-we,y-we,2*we,2*we)
end
function spawn_building(x,y)
	local b={
		x=x,y=y,z=0,touch=0,
		floors={}
	}
	if(cam:is_top_down()) then
		for i=0,flr_n-1 do
			add(b.floors,{x=x,y=y,z=i*flr_h,i=i,draw=flr_draw})
		end
		add(b.floors,{x=x,y=y,z=flr_n*flr_h,i=8,draw=roof_draw})
	else
		for i=1,flr_n do
			add(b.floors,{x=x,y=y+i*flr_h,z=0,i=i,draw=flr_chase_draw})
		end
		add(b.floors,{x=x,y=y,z=0,i=8,draw=roof_chase_draw})
	end
	blds_c+=1
	blds[blds_c]=b
	return b
end

--------------
--- bullet ---
function blt_update(self)
		self.z+=self.dz
		if(self.z<=0) then
			fxs:make_blast(self.x,self.y,0,0,0,8)
			return false
		elseif(self.t>time_t) then
			self.y+=self.dy
			return true
		end
		return false
end
function blt_draw(self,x,y,z,w)
	local we=flr(blt_w*w)+1
	sspr(48,32,8,8,x-we,y-we,2*we,2*we)
end
function msl_update(self)
		self.z+=self.dz
		local dt=self.t-time_t
		if(dt>0 and self.z>0) then
			--if(flr(dt)%4==0) then
				local dx,dy=plyr.x-self.x,plyr.y-self.y
				local dist=dx*dx+dy*dy
				if(dist>0) then
					dist=sqrt(dist)
					dx/=dist
					dy/=dist
					dx*=2
					dy*=2
				end
			--end	
			self.x+=self.dx
			self.y+=self.dy
			if(time_t%4==0) fxs:make_smoke(self.x,self.y,self.z)
			return true
		end
		fxs:make_blast(self.x,self.y,0,0,0,8)
		return false
end
function msl_draw(self,x,y,z,w)
	local we=flr(blt_w*w)+1
	sspr(48,48,8,8,x-we,y-we,2*we,2*we)
end
function blts:make(x,y,z,dy,dz,side)
	local b={
		x=x,
		y=y,
		z=max(z,0),
		dy=dy,
		dz=dz,
		t=time_t+3*30,
		side=side,
		draw=blt_draw,
		update=blt_update
	}
	blts_c+=1
	self[blts_c]=b
	sfx(3)
	return b
end
function blts:make_msl(x,y,z,dz,side)
	local dx=plyr.x-x
	local dy=plyr.y-y
	local dist=dx*dx+dy*dy
	if(dist>0) then
		dist=sqrt(dist)
		dx/=dist
		dy/=dist
		dx*=2
		dy*=2
	end
	local b={
		x=x,
		y=y,
		z=max(z,0),
		dx=dx,
		dy=dy,
		dz=dz,
		t=time_t+4*30,
		side=side,
		draw=msl_draw,
		update=msl_update
	}
	blts_c+=1
	self[blts_c]=b
	sfx(3)
	return b
end
function blts:update()
	local n=blts_c
	blts_c=0
	local b
	for i=1,n do
		b=blts[i]
		if(b:update()) then
			blts_c+=1
			blts[blts_c]=b
			--insert into zbuffer
			b.zi=zbuf:write(b)
		end
	end
end

function blts:resolve_collisions()
	for j=1,blts_c do
		local blt=self[j]
		-- against buildings
		if (blt.z<=flr_zmax) then
			local b,dx,dy,d2
			for i=1,blds_c do
				b=blds[i]
				dx=blt.x-mid(blt.x,b.x-flr_w,b.x+flr_w)
				dy=blt.y-mid(blt.y,b.y-flr_w,b.y+flr_w)
				dx/=8
				dy/=8
				d2=dx*dx+dy*dy
				if (d2<blt_r2/64) then
					blt.t=0
					b.touch=1
					fxs:make_blast(blt.x,blt.y,blt.z,0,0,8)
					break
				end
			end
		end
		-- against entities
		local zb=zbuf[blt.zi]
		for i=1,zb.o do
			local o=zb.objs[i]
			if (o.side!=blt.side and o:blt_hit(blt)) then
		 		blt.t=0
		 		break
		 	end
		end
	end
end

--------------------------
-- boss helpers
function boss:init(name,x,y)
	boss.enabled=true
	boss.x=x
	boss.y=y
	boss.z=0
	boss.name=name
	boss.msg_dly=time_t+3*30
	boss.parts={}
end
function boss:msg_draw(y)
	printer
		:centered()
		:shaded(5)
		:print("now entering the enemy's zone.",64,y,7)
	y+=6
	line(6,y,124,y,7)
	y+=4
	printer
		:centered()
		:shaded(5)
		:print("target code name:",64,y,7)
	y+=18
	local x=24	
	for i=1,#self.name do
		sprint(sub(self.name,i,i),x,y,10,2)
		x+=10
	end
	y+=16
	printer
		:centered()
		:shaded(5)
		:print(self.mark,64,y,7)
	y+=6
	line(6,y,124,y,7)
end
function boss:update()
	for t in all(self.parts) do
		t:update(self)
	end
	zbuf:write(self)
end

--------
-- battleship boss (level 1)
function spawn_bship(x,y)
	y+=128
	boss:init("mermaster",x,y)
	boss.mark="marine forthress ba-001"
	boss.draw=bship_draw
	local parts={
		{0,0,0,0,0,0},
		{0,0,0,0,0,0},
		{0,1,0,0,1,0},
		{1,1,0,0,1,1},
		{1,1,0,0,1,1},
		{1,0,0,0,0,1},
		{0,0,2,2,0,0},
		{0,0,2,2,0,0}}
		
	-- parts
	local py=128
	for j=1,#parts do
		for i=1,6 do
			local t=nil
			local ptype=parts[j][i]
			local px=128*(i-1)/5-64
			if(ptype==1) then
				t={
					x=px,
					y=py,
					sx=16,
					sy=112,
					fire_dly=time_t+rnd(30)+1.2*30,
					update=turret_update
				}
			elseif(ptype==2) then
				t={
					x=px,
					y=py,
					sx=48,
					sy=112,
					fire_dly=time_t+rnd(30)+1.2*30,
					update=mslturret_update
				}
			end
			if(t) add(boss.parts,t)
		end
	 py-=32
	end
	return boss
end
function bship_draw(self,x,y,z,w)
	smap(2,1,3,12,x,y,4*w)
	palt(4,true)
	palt(3,false)
	local we=24*w
 for i=1,#self.parts do
 	local p=self.parts[i]
		sspr(p.sx,p.sy,16,16,x+p.x*w-we/2,y-p.y*w-we/2,we,we)
	end	
	palt(4,false)
	palt(3,true)
end
function turret_update(self,p)
	if(self.fire_dly<time_t) then
		blts:make(p.x+self.x,p.y+self.y,0.5,-2,1,0)
		self.fire_dly=time_t+rnd(30)+0.5*30
	end
end
function mslturret_update(self,p)
	if(self.fire_dly<time_t) then
		blts:make_msl(p.x+self.x,p.y+self.y,0.5,1,0)
		self.fire_dly=time_t+rnd(30)+0.5*30
	end
end

---------------
--- zbuffer ---
zbuf_class={}
function make_zbuf(n,dist)
	local zb={
		n=n, -- number of layers (+1)
		h=dist  -- dist between layers
	}
	setmetatable(zb,{__index=zbuf_class})
	for i=1,n+1 do
		zb[i]={}
		zb[i].n=0
		zb[i].elts={}
		zb[i].pos={}
		zb[i].o=0
		zb[i].objs={}
	end
	return zb
end
function zbuf_class:clear()
	for i=1,self.n+1 do
		local zb=self[i]
		zb.n=0 
		zb.o=0
	end
end
function zbuf_class:write(obj,phy_obj,i)
	local x,y,z,w=cam:project(obj)
	local zi=i or mid(flr((z-cam.zfar)/self.h+0.5),1,self.n+1)
	local zb=self[zi]
	zb.n+=1
	zb.elts[zb.n]=obj
	zb.pos[zb.n]={x,y,z,w}
	if (phy_obj) then
		zb.o+=1
		zb.objs[zb.o]=phy_obj
	end
	return zi
end
function zbuf_class:draw()
	for i=1,self.n+1 do
		local zb=zbuf[i]
		for j=1,zb.n do
			local pos=zb.pos[j]
			if(pos[4]) zb.elts[j]:draw(pos[1],pos[2],pos[3],pos[4],i)
		end
	end
end
function zbuf_class:print(x,y,c)
	local col=c or 1
	print("plyr:"..plyr.z,x,y,col)
	y+=6
	for i=1,self.n+1 do
		local s="."
		if(plyr.zi==i) s=">"
		print(s..i..":"..zbuf[i].n,x,y+i*8,col)
	end
end

top_down_game={}
function top_down_game:update()
	zbuf:clear()
 
	plyr:update()

	world:update()

	if(boss.enabled) boss:update()

	-- update buildings
	local n=blds_c
	blds_c=0
	for i=1,n do
		local b=blds[i]
		if (b.y-plyr.y>-128) then
			b.touch=0
			for zi=1,#b.floors do
				zbuf:write(b.floors[zi])
			end
			blds_c+=1
			blds[blds_c]=b
		end
	end

	-- update tanks
	n=tks_c
	tks_c=0
	for i=1,n do
		local tk=tks[i]
		if (tk.y-plyr.y>-128 and tk.hit==0) then
			if (tk.dly<=time_t) then
				blts:make(tk.x,tk.y,0.5,-2,1,tk.side)
				tk.dly=time_t+2*30+rnd(30)
			end
			tks_c+=1
			tks[tks_c]=tk
			zbuf:write(tk,tk)
		end
	end

	-- update helos
	n=helos_c
	helos_c=0
	for i=1,n do
		local h=helos[i]
		if (h.dly<=time_t) h.z+=h.dz
		if (h.y-plyr.y>-128 and h.hit==0) then
			helos_c+=1
			helos[helos_c]=h
			--insert into zbuffer
			zbuf:write(h,h)
		end
	end

	blts:update()
	fxs:update()
 
	plyr:resolve_collisions()
	blts:resolve_collisions()
end

function draw_floor()
	local da=cam.alpha/hh
	local a=cam.beta-cam.alpha
	local a_fix=-cam.alpha
	local imax=0
	local z=-cam.z
	for i=-hh,hh do
		local sa=-sin(a)
		if(abs(sa)>0.01) then
		 local ca_fix=cos(a_fix)
			local dist=z*ca_fix/sa
			if(dist>0) then
			 local ca=cos(a)
				-- find v coords 
				local ydist=dist*ca+plyr.y
				local y=band(band(0x7fff,ydist),31)
				local ramp=world:floor_ramp(ydist)
				local c=band(band(flr(shr(y,4)),31),1)
				memset(0x6000+(64-i)*64,ramp[2*c+band(64-i,1)+1],64)
				local xroad=world:roadx(ydist)
				if(xroad) then
					local w=cam.focal/dist
					local ze=flr(32*w+0.5)
					local xe=hw+(xroad-cam.x)*w
					local v=flr(y)
					sspr(24,band(v,15),16,1,xe-ze/2,64-i,ze,1)
				end
				imax=64-i
			end
		end
		a+=da
		a_fix+=da
	end
	if(imax>=0) then
		map(16,0,0,imax-8,16,3)
		rectfill(0,0,127,imax-8,12)
	end
	-- draw heli shadow
	--if (band(flr(t),1)==0) then
	--	ze=flr(ze/2+0.5)
	--	palt(0,false)
	--	palt(3,true)
	--	sspr(56,32,8,24,hw-ze/2,hh+8,ze,2*ze)
	--	palt()
	--end
end

function debug_draw()
 --cls(0)
	for i=1,blds_c do
		local b=blds[i]
		local x,y,ze
		ze=flr_w
		x=b.x-cam.x
		y=b.y-cam.y
		x+=hw
		y=hh-y
		if(b.touch==1) then
			rectfill(x-ze,y-ze,x+ze,y+ze,10)
		else
			rect(x-ze,y-ze,x+ze,y+ze,10)
		end
	end 
	
	local pos=cam:project(plyr)
	if(pos.w) then
		circ(pos.x,pos.y,plyr_r,12)	
		print(pos.w,pos.x+helo_r+1,pos.y,0)
	end
	for i=1,blts_c do
		local b=blts[i]
		local x,y
		x=b.x-cam.x
		y=b.y-cam.y
		x+=hw
		y=hh-y
		local col=10
	 if(b.side!=plyr.side) col=8
		circ(x,y,blt_w,col)
		print(b.z-plyr.z,x+blt_w,y,0)		
	end

	for i=1,helos_c do
		local o=helos[i]
		local dz=plyr.z-o.z
		local pos=cam:project({x=o.x,y=o.y,z=plyr.z})
		if(pos.w) then
			local x,y=o.x-plyr.x,o.y-plyr.y
			if((x*x+y*y)<(helo_r+plyr_r)*(helo_r+plyr_r)) then
				circfill(pos.x,pos.y,helo_r,10)
			else
				circ(pos.x,pos.y,helo_r,10)
			end
			if(abs(dz)<=4) then
				line(hw-1,hh+24,64+x,64-y,8)
			end		
			--print(dz,pos.x+helo_r+1,pos.y,0)
		end
	end
end

function top_down_game:draw_spd(x,y)
	rectfill(x,y,x+12,y+4,1)
	rectfill(x+13,y,x+30,y+4,0)	
	for i=0,4 do
		pset(x+13+4*i,y,8)
		pset(x+13+4*i,y+4,8)		
	end
	rectfill(x+13,y+1,x+13+16*plyr.vy,y+3,11)
	
	print("spd",x+1,y,8)
	
	-- debug (beta angle)
	circ(118,17,8,1)
	line(118,17,118+8*cam.cb,17-8*cam.sb,8)
	print(flr(360*cam.beta),118-6,17+12,1)
end

function top_down_game:draw()
	palt(0,false)
	draw_floor()
	--cls(0)
	palt(3,true)
	zbuf:draw()
	for i=0,plyr.lives-1 do
		spr(2,2+i*9,128-9)
	end
	palt()
	
	rectfill(0,0,127,7,1)
	print("\150:"..flr(100*stat(1)+0.5).."% \152:"..flr(stat(0)+0.5).."kb",1,1,7)
	self:draw_spd(95,120)
	--print("blds:"..blds_c,0,16,12)
	--print("tks :"..tks_c,0,24,12)
	--print_map(0,32)
	--zbuf:print(0,16,1)
	--debug_draw()
	--world:print(0,12)

	if(boss.enabled and boss.msg_dly>time_t) then
		boss:msg_draw(36)
	end
end

function top_down_game:init()
	time_t=0
	cam:init(96,-96-plyr_zmax,0.75)
	-- reset entities
	helos_c=0
	tks_c=0
	blts_c=0
	for b in all(blasts) do
		del(blasts, b)
	end
	--zbuf=make_zbuf(flr_n,flr_h)
	zbuf=make_zbuf(8,flr_h)
	
	world
		:init(0,56,6*8)
				:register(4+1,spawn_tk)
				:register(6+1,spawn_building)
				:register(7+1,spawn_building)
				:register(8+1,spawn_helo)
				:register(9+1,spawn_bship)
				
	--[[
	spawn_building(0,0)
	spawn_building(-48,56)
	spawn_building(48,56)
	]]
	
	-- sounds
	sfx(4)

	plyr:init()
end

-------------------------
---- chase view game ----
-------------------------
chase_game={}
function chase_game:draw()
	cls(12)
	palt(0,false)
	draw_floor()
	palt(3,true)
	zbuf:draw()
	for i=0,plyr.lives-1 do
		spr(2,2+i*9,128-9)
	end
	palt()
	
	zbuf:print(0,8)
	
	rectfill(0,0,127,7,1)
	print("�:"..flr(100*stat(1)+0.5).."% �:"..flr(stat(0)+0.5).."mb",1,1,7)
end
function chase_game:update()
 -- todo: rotate camera based on time
	if(cam.beta<0.75) cam.beta+=0.005
	
	-- clear zbuf
	zbuf:clear()

	heli:update()

	-- update bullets
	local n=blts_c
	blts_c=0
	for i=1,n do
		local b=blts[i]
		b.z+=b.dz
		if (b.t>t and b.z>=flr_z) then
			b.y+=b.dy
			blts_c+=1
			blts[blts_c]=b
			--insert into zbuffer
			b.zi=zbuf:write(nil,b.y,b)
		end
	end
end

function chase_game:init()
	time_t=0
	map_i=0
	map_t=8
	cam.x=0
	cam.y=0
	cam.z=0
	beta=0.5
	zbuf=make_zbuf(16,4)

	for i=1,15 do
		map_funcs[i]=spawn_nop
	end
	--map_funcs[4+1]=spawn_tk
	--map_funcs[6+1]=spawn_building
	--map_funcs[7+1]=spawn_building
	--map_funcs[8+1]=spawn_helo

	-- sounds
	sfx(4)

	plyr:init()
end

-- title_screen
title_screen={}
title_pic=".a163.cecriekbafcr.a13.fcrk.a12.qqkvkvkvkvklnvkd.a24.kv(.a18.ijvkljv(givkn.a12.uukfb.a12.cvkvkkvklvkuvkn.a24.ivkf.a18.fvk)fvkvbvkvb.a11.rskve.a15.rijvknvkvwkvvkb.a22.vkvf.a17.uukvwvkvwvkvg.a10.iymkvs.a15.emgvkvgtzukvwnn.a22.ukvwb.a16.qskvuwkv(wkv(.a11.bllvkc.a14.qiuukv(m]ajv(wvb.a8.qskve.a7.rqkv(gacriecbiukfaukaaakkvskkvk)kvkdqivkvkvaaivmkvkjaivkvkvk.a8.qqkvktvdevkjpvskpvskpvskkvsikvsabskjkvk)askjkvkbev{jkvcaaijvkcjvkklvknuukfjvknuukfrukfbikvsukvc.a8.ckvknwoquknlvkkjvkkjvkkjvkvkvkcquknivkntuknqukfqukvsieaaaevkvkvklnvkvbskjfskvfskvntkvuskvuskvk.a8.iivkvz(bskvnvkvnvkjnvkvnvkvevkvnvkvgvkvnskvnvkvfskvuwvdaaquknsukvwvkvwiecvvkvsukvw)kvk)kvs(kvkb.a9.vkvgldikvwvkvwvkvwvkv(ukv(ukv(ukv(ukvwjkvwtkvwikvwnglaaqskv(wkv(wkv(wkv(ukv(wkv({lvknlvknlvkf.a9.ukv(mnajv(wkv(wkvzwkv(wkvktkvktkvktkvzgjv(gjv(cjv(ovkaaaikvg)kvk)kvkljvk)evk)jvk)evkjnvkjnvkt.a9.ikvjtvbevk)jvg)kvs(kvg)kvsmjvsmjvgnjvg)evgjjvglevkl.a6.jvjkft(mhvknhvzuru(mhv(mtszevtzmsszmc.a9.zmgnwgqszmtsjkhtjkhtjkhtjsftjsftjsftjktszeriekrszmb.a5.umgjvmgt]mgt]mgtgkgtnkgtnkgtekgtecrif.a9.etjsz(akgtnkgj]mgj]mgj]mgjwmgjgkgjwmgjnkgt]crifkgtf.a5.qszevszevtzmwjzezizmwjzmwjzmsizmsmwg.a11.kgjgldizmwjzevtzevtzevtzezizezizezizevjzevjzevizmw.a6.kgtukgtugftugftedftugftugftjcftjkxlqqiec.a6.ieczmnariugriugriecriugriedriedriecriugriugriucrieb.a5.eecriecr(ecr(ecriecr(ecriecriecriaaaeecr.a7.rietvbccr(ecr(ecriecr(ecrmecrmccriccr(ecriecrkccrk.a5.iriecriejtiejtiebriejtiebriebriecriecriec.a6.crqmwgaabtz(ftzkgtz(gtv)gtvk]wf(gtvnglh(]ox(]ovmglb.a5.ntzmgtzkntzkgtzmgtzkgtzmgtvlwoxlvkv)]ofc.a8.btz(aae(]ov(]ov)]ox)]kv)]k)mwiv)]ox)fqzmgtzm]ix)f.a5.u(]ox)]ou(]ou(]ox)]ku)]ox)]kzmgtzmgtzmf.a9.ekvkb.a21.vkv.a9.vkvkvkf.a37.qiecriecri.a312"
start_ramp={8,2,1,2}
scores={
	--name/score/last?
	{"aaa",1000,true},
	{"bbb",900,false},
	{"ccc",800,false},
	{"ddd",600,false},
	{"eee",500,false},
}
ranks={"1st","2nd","3rd","4th","5th"}
starting=false
rooster={
	rows={}
}
chars=" abcdefghijklmnopqrstuvwxyz-0123456789\131\132\133\134\135\136\137\138\139\140"
chars_mem={}
function rooster:clear()
	self.rows={}
end
function rooster:update()
	for row in all(self.rows) do
		if(row.dly<time_t) then
			for c in all(row.chars) do
				if (c.dly<time_t) c:update()
			end
		end
	end
end
function rooster:draw()
	for row in all(self.rows) do
		if(row.dly<time_t) then
			for c in all(row.chars) do
				if (c.dly<time_t) c:draw()
			end
		end
	end
end
function rooster:add(s,col)
	local n=#self.rows
	local row={
		dly=time_t+n*0.5*30,
		chars={}
	}
	local x,z=-48,12-8*n
	local dt=0
	for i=1,#s do
		local c=sub(s,i,i)
		-- no need to display space
		if(c!=" ") then
			local char={
				c=c,
				col=col or 7,
				dly=row.dly+n*2*30+dt*0.25*30,
				src={x=x,y=-64,z=-24},
				dst={x=x,y=0,z=z},
				draw=char_draw,
				update=char_update
			}
			add(row.chars,char)
			dt+=1
		end
		x+=8
	end
	add(self.rows,row)
end
function lerp(a,b,t)
	return a*(1-t)+b*t
end
function lerpn(a,b,t)
	local r={}
	for k,v in pairs(a) do
		r[k]=lerp(v,b[k],t)
	end
	return r
end
function smoothstep(t)
	t=mid(t,0,1)
	return t*t*(3-2*t)
end
function char_update(self)
	local t=(time_t-self.dly)/(0.8*30)
	self.cur=lerpn(self.src,self.dst,smoothstep(t))
end
function char_draw(self)
	local x,y,z,w=cam:project(self.cur)
	if(w) sprint(self.c,x,y,self.col,w)
end

sprint_lastm=-1
function sprint_init(chars)
	for i=1,#chars do
		local c=sub(chars,i,i)
		cls(0)
		print(c,0,0,7)
		local mem=0x4300+(i-1)*32
		for y=0,7 do
			memcpy(mem+4*y,0x6000+64*y,4)
		end
		chars_mem[c]=mem
	end
	cls(0)
end
function sprint(c,x,y,col,size)
	if(abs(size-1)<0.01) then
		print(c,x-4,y-4,col)
	else
		local mem=chars_mem[c]
		if(mem!=sprint_lastm) then
			for m=0,7 do
				memcpy(0x0+64*m,mem+4*m,4)
			end
			sprint_lastm=mem
		end
		pal(7,col)
		sspr(0,0,8,8,x-4*size,y-4*size,8*size,8*size)
		pal(7,7)
	end
end
	
function title_screen:update()
	if(btnp(4) or btnp(5)) then
		starting=true
		-- clear last score
		for s in all(scores) do
			s[3]=false
		end
		sm:push(top_down_game)
	end
	rooster:update()
end
function title_screen:draw()	
	local ret,res=coresume(self.str2mem_cor)
	if(ret and res) then
		res=res-0x6000
		local y=flr(res/64+0.5)
		line(0,y,127,y,7)
	end

	rectfill(0,24,127,127,0)

	print("rank",24,36,5)
	print("score",48,36,5)
	print("name",76,36,5)

	rooster:draw()
	
	local s="\151 or \145 to play"
	local rs=8
	if (starting) rs=2
	print(s,64-#s*5/2,128-8,start_ramp[flr(time_t/rs)%#start_ramp+1])
end
function title_screen:init()
	time_t=0
	starting=false
	cam:init(96,-96)
	cam:track({x=0,y=0,z=0})
	rooster:clear()
	for i=1,#ranks do
		local s=ranks[i].."  "..scores[i][2].."  "..scores[i][1]
		local col=7
		if(scores[i][3]) col=10
		rooster:add(s,col)
	end	
	self.str2mem_cor=cocreate(function()
			local s=dot2str(title_pic)
			str2mem_cor(s,0x6000)
		end)
end
function title_screen:score(name,s)
	local c=1
	for i=1,#ranks do
		if(s>=scores[i][2]) then
		 local j=#ranks
			while(j!=i) do
				scores[j]=scores[j-1]
				j-=1
			end
			scores[i]={name,s,true}
			break
		end
	end
end

game_over={
	done=false,
	cur_i=1,
	next_i=1,
	next_dly=0
}
name={}
name_i=1
i2c={}
for i=1,#chars do
 local c=sub(chars,i,i)
 i2c[i]=c
end
function game_over:get_name()
	local s=""
	for i=1,#name do
		s=s..i2c[name[i]]
	end
	return s
end
function game_over:update()
	if(time_t>30*30 or self.done) then
		title_screen:score(self.get_name(),1450)
		sm:push(title_screen)
	end
	if(btnp(0)) then
		self.next_i-=1
		self.next_dly=time_t+0.25*30
	elseif(btnp(1)) then
		self.next_i+=1
		self.next_dly=time_t+0.9*30
	end
	if(self.next_i>#chars) self.next_i=1
	if(self.next_i<=0) self.next_i=#chars
	if(self.next_dly<time_t) then
		self.next_dly=0
		self.cur_i=self.next_i
	end
end
function game_over:draw()
	cls(0)
	local s="game over"
	print(s,64-(#s*5)/2,24,8)
	s="enter your name"
	print(s,64-(#s*5)/2,48,8)
	print(self.get_name(),48,64,10)
	
	-- carousel
	local t=(self.next_dly-time_t)/(0.9*30)
	t=smoothstep(1-t)
	local da=1/#chars
	local a=-0.25-da*lerp(self.cur_i,self.next_i,t)
	local col=7
	for i=1,#chars do
		local c=sub(chars,i,i)
		local x,y,z,w=cam:project({x=56*cos(a),y=-48*sin(a),z=-16})
		if (i==self.cur_i) col=10 else col=7
		if(w) sprint(c,x,y,col,w)
		a+=da
	end
	print(30-flr(time_t/30),128-8,128-12,1)
end
function game_over:init()
	time_t=0
	self.done=false
	self.sel=1
	name={1,1,1}
	name_i=1
	name_c=1
	cam:init(72,-256)
	cam:track({x=0,y=0,z=0})
end

-- game loop
function _update()
	time_t+=1
	sm:update()
end
function _draw()
	sm:draw()
end
function _init()
	cls(0)
	print("dip switch testing...")
	sprint_init(chars)
	sm:push(top_down_game)
end

__gfx__
88888888a95566673333333305555555555555507777777777777777777777777777777777777777777777777777777777777777333333304333333300000000
88000088000000003777655505555555555555507555557777755555555555777775555766666666666666666666666666666666333334304373333300000000
80800808000000001333133305555555555555507566665555566666666666555556665766666666666666666666666666666666333330004463333300000000
8008800800000000151111c305555555555555507566666666666666666666666666665755555555555555555555555555555555333330355353333300000000
80088008000000003331111105555557755555507566666666666666666666666666665755555555555555555555555555555555333333066433333300000000
80800808000000003335555305555557755555507566666666666666666666666666657755555555555555555555555555555555333330400943333300000000
88000088000000003333333305555557755555507756666666666666666677777776657766666666666666666666666666666666333334c99793333300000000
8888888800000000333333330555555775555550775666666666666666667557557665776666666666666666666666666666666633330cc99c79333300000000
0040444444490440cccccccc0555555775555550775666666666666666667777777665776666666666666666666666666666666634440cc99cc9999300000000
00404444444904407ccc7ccc05555557755555507756666666666666666675575576665755555555555555555555555555555555300004c94c99000300000000
0040440000490440cccccccc0555555555555550775666666666666666667777777666575555555555555555555555555555555534030c4999c4349300000000
0400449999440040cccccccc05555555555555507756666666666666666675575576665755555555555555555555555555555555333331c94c13333300000000
0400440000440040c7c7c7c705555555555555507566677777777777666677777776665766666666666666666666666666666666333303044430333300000000
0400400000040040cccccccc05555555555555507566675555555557666655555556665766666666666666666666666666666666333303333330333300000000
00000090040004407c7c7c7c05555555555555507566675666666657666655555556665766666666666666666666666666666666333333333333333300000000
0090090000400440c7c7c7c705555555555555507566675677766657666666666666665755555555555555555555555555555555333333333333333300000000
04409000000004407b7b7b7b33338888333995137566675677777657666666666666665755555555555555555555555555555555333333333333333300000000
0440900000000440b7b7b7b733388899899999517566675677755657666666666666665755555555555555555555555555555555333333333333333300000000
9440900000000440bbbbbbbb33888999aaaaa9957566675677777657666666666666665766666666666666666666666666666666333333304333333300000000
94400900000004407b7b7b7b331889aaaa77a9957566675677755657666666666666665766666666666666666666666666666666333333304333333300000000
9444009000004440bbbbbbbb33199aa7777aa9887566675677766657666666666666657766666666666666666666666666666666333333304333333300000000
90004000000400403b3b3b3b38119a77777a99557756675655566657666666666666657755555555555555555555555555555555333300304393333300000000
9999440900444440b3b3b3b338899aaa9a7a998177566756666666576666666666666577555555555555555555555555555555553334444049c9a33300000000
00004409004400003b3b3b3b3501889aa777aa1377566777777777776666666666666577555555555555555555555555555555553344440099999a3300000000
0000440900440000030303033558899777777a937756655555555555666666666666657766666666666666666666666666666666334440044449993300000000
000000090000000030303030158999a777aa99997756655555555555666666666666665766666666666666666666666666666666333000000000033300000000
044044090044044003030303518999aaa999888875666666666666666666666666666657666666666666666666666666666666663300000049499a3300000000
0440440900440440303030305511888899988888756666666666666666666666666666575555555000000000000000000055555500404040404049aa00000000
09904409004409900303030315511858899818887566666666666666666666666666665755555550c77cc0c77cc0c77cc0555555444494949494949900000000
33333309003333333030303031555855588115517566655555666666666666555556665755555550cc77c0cc77c0cc77c0555555444040404040444400000000
33333309003333330303030333311158888551137555577777555555555555777775555755555550ccc770ccc770ccc770555555000000000000000000000000
33333309003333333030303033333331111113337777777777777777777777777777777755555550cccc70cccc70cccc70555555000333333333300000000000
33333333633333333333333363333333333333336333333333888833333303333333333333335333333333333333333333337333333333333333333333333333
33333330133333333333333013333333333333301333333338aaaa83333000333333333330935333333333333333333330937333333333333333333555333333
3333333161333333333333316333333333333331733333338aaaaaa8333000333333339440956333333333333333339440956333333333333333300090443333
3333331016333333333333101633333333333310173333338aa77aa8330000033333339440937333333333333333339440935333333333333333300094443333
3333350001633333333336000163333333333600017333338aa77aa8330000033333333330937333333333333333333330935333333333333333330074433333
3333350001603333333336000163333333330600017333338aaaaaa8330000033333333330933333333333333333333330000033333333333333333079333333
33333506116033333333360061633333333306001673333338aaaa83330000033333300330933333333333333333333330000033333333333333333049333333
33333500116033333333360011633333333306001173333333888833330000033333000330933333333333333333333330000333333333333333333049333333
33333306113033333333330061333333333303001633333300000000330000033330000330933333303333333333333330000333333333333533445049444335
3333330013003333333330001003333333330030103333330000000033300033333300003093333300033333333333333000033333333333354444509a944445
3333330013303333333330301303333333330330103333337770707033300033333330003093333000003333333333333000333333333333354444509aa44445
3333333013333333333333301333333333333330133333337070707033330333333333000094330000003333333333330000333333333333354444009aa44445
3333333013333333333333301333333333333330133333337070770033330333333333300044300003333333003333330000333333333333353333005aa33335
333333301333333333333330133333333333333013333333777070703333033333333333009400033333333300000333040433333333333333333300c5a33333
333333301333333333333330133333333333333013333333000000003333033333333330000000333333333300000000000043333333333333333300c5a33333
333333301333333333333330133333333333333013333333000000003333033333333330006043333333333300000000006000000000003333333300c5a33333
33336330133633333336333013336333333363301336333333388333303303303333333000004333333333333333333000004000000000333333333059333333
3333666066663333333666601666633333336665166633333300003330000000333333000cc0033333333333333333310c04c333000000333333333049333333
3333633113363333333633301333633333336330033633333005650330330330333300000c70003333333333333333310004c333333000333333333049333333
33333311133333333333333013333333333333300033333380567608333303333000000010070003333333333333333010074333333333333333333049333333
33333311133333333333333013333333333333300033333380566608333303333000003301143000333333333333333300043333333333333333333597333333
33333333333333333333333333333333333333333333333330056503333333333300033335533000033333333333333300033333333333333333333597333333
33333333333333333333333333333333333333333333333333000033333333333330333333333300003333333333333300033333333333333333333353333333
33333333333333333333333333333333333333333333333333388333333333333333333333333300033333333333333300033333333333333333333353333333
00000000c10000000000000000000000000000000000000000000000000000003333333333333330333333333333333000033333333333330000000000000000
00740800000000000000000000000000000000000000000000000000000000003333333333333333333333333333333000033333333333330000000000000000
80855555000000000000000000000000000000000000000000000000000000003333333333333333333333333333333333333333333333330000000000000000
55500080900000000000000000000000000000000000000000000000000000003333333333333333333333333333333333333333333333330000000000000000
80008087000000000000000000000000000000000000000000000000000000003333333333333333333333333333333333333333333333330000000000000000
00004000000000000000000000000000000000000000000000000000000000003333333333333333333333333333333333333333333333330000000000000000
00000700000000000000000000000000000000000000000000000000000000003333333333333333333333333333333333333333333333330000000000000000
70000000000000000000000000000000000000000000000000000000000000003333333333333333333333333333333333333333333333330000000000000000
33333333300033333333333333333033333300033333333333333330003333333333333333333301333333333333333331333333333333033333333300000000
33333333300033333333333333300003333300033333333333333330003333000333333333333301333333333333333301333333333333013333333300000000
33003333300033333003333333330003333300333333333333333333003333000033333333333301333333333333333301333333333333013333333300000000
30000333300033330000333333333000333300333333333333333333003330000333333333333301333333333333333301333333333333013333333300000000
33000033330333300003333333333300333003333333333330003333003330003333333333333301333333333333333301333333333333013333333300000000
33330003330333000333333333333330033003333333333300000033303300033333333333333301333333333336336301333333333333013633633300000000
33333300330330033333333333333333033033333300033300000000303300333333333333336301363333333336336011333333333333001633633300000000
33333333000003333333333333333333300033300000003333333000000003333333333336336101163363333336301013633333333336301013633300000000
33333333303033333333333300000000003000000000003333333333303033333333333336330101113363333366600011633333333336001116663300000000
33333333000003333333333300000033300033333333333333333333300000000333333336330001113363333360066011133333333330001660063300000000
33333300330330033333333300033333303303333333333333333333003033000000003336666601666663333363300011133633336330001003363300000000
33330003330333000333333333333333003300333333333333333330033033330000003336000001000063333333000116633633336336600111333300000000
33000033300033300003333333333333003330033333333333333300033003333300003336330001113363333333000110066633336660000111333300000000
00000333300033330000033333333330003333003333333333333000333000333333033336300001110363333333300011106333333600001113333300000000
00003333300033333000033333333330003333000333333333330000333000333333333336300000110363333333000011036333333630001100333300000000
30033333300033333300333333333300003333000033333333300003333300033333333333330000113333333300301111036333333630000003003300000000
33333333000003333333333333333300003333300003333333000033333300033333333333330011103333333303300013333333333333310003303300000000
33333333000003333333333333333000033333300000333333000033333300003333333333303000030333333333300000333333333333000003333300000000
33333333000003333333333333333000033333330003333333300333333300003333333333033000033033333333333003033333333330300333333300000000
33333333333333333333333333330000033333330033333333330333333300033333333333033333333033333333333333303333333303333333333300000000
33333333333333333333333333333000033333333333333333333333333333333333333333333333333333333333333333303333333303333333333300000000
33333333333333333333333333333330333333333333333333333333333333333333333333333333333333333333333333333333333333333333333300000000
33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333300000000
33333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333300000000
00000000000000000000000000000000000000000000000000000000000000000000000033333333333333330033333333333333333333333333333300000000
00000000000000000000000000000000000000000000000000000000000000000000000033333333333333330000333333333333333333333333333300000000
00000000000000000000000000000000000000000000000000000000000000000000000033333333333333333300003333333333333333333333333300000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000003300330033333333333000003333333300000000
00000000000000000000000000000000000000000000000000000000000000000000000033333333333300003333003300333333300000003333333300000000
00000000000000000000000000000000000000000000000000000000000000000000000033333333000000333333330033003333000000003333333300000000
00000000000000000000000000000000000000000000000000000000000000000000000033333333333333333333333333330033300000003333333300000000
00000000000000000000000000000000000000000000000000000000000000000000000033333333333333333333333333333333333000003333333300000000
33333333333366666666666666666666444444444444444444444444444444440000000000000000000000000000000000000000000000000000000000000000
33333333333655555555555555555555444444444444444444443333333344440000000000000000000000000000000000000000000000000000000000000000
33333333366500000000000000000000444444444444444444430303303034440000000000000000000000000000000000000000000000000000000000000000
33333333655055550505050505050505444444444444444444330303303033440000000000000000000000000000000000000000000000000000000000000000
33333336500555555555555555555555666666666666666644333333333333440000000000000000000000000000000000000000000000000000000000000000
33333365055555555555555555555555555555555555555544333336733333440000000000000000000000000000000000000000000000000000000000000000
3333365055555555559999555555555555555555555555554433b060070b33440000000000000000000000000000000000000000000000000000000000000000
33336505555555555595555555555555556000000000000544bb00600600bb440000000000000000000000000000000000000000000000000000000000000000
33365055555555555595555555555555556555555555500544770056650077440000000000000000000000000000000000000000000000000000000000000000
333650555555555555999955555555555565005dd500500544bb06555570bb440000000000000000000000000000000000000000000000000000000000000000
336505500555555555955955555555555565555dd555500544bb06555560bb440000000000000000000000000000000000000000000000000000000000000000
336505555555555555955955555555555565005dd500500544bb05655650bb440000000000000000000000000000000000000000000000000000000000000000
36505550055555555599995555555555556555555555500544bb00566500bb440000000000000000000000000000000000000000000000000000000000000000
36505555555555555555555555555555000000000000000044000005500000440000000000000000000000000000000000000000000000000000000000000000
65077777777777777777777777777777555555555555555544400000000004440000000000000000000000000000000000000000000000000000000000000000
65000000000000000000000000000000666666666666666644444444444444440000000000000000000000000000000000000000000000000000000000000000
65044444000000004444444444444444650444444444444444443030303444440000000000000000000000000000000000000000000000000000000000000000
65044444000000004444333333334444650444444444444444443030303444440000000000000000000000000000000000000000000000000000000000000000
65044444000000004443030330303444650444444444444444403333333004440000000000000000000000000000000000000000000000000000000000000000
65044444000000004433030330303344650444444444444444007777777004440000000000000000000000000000000000000000000000000000000000000000
6504444400000000443333333333334465066666666666664400b05b05b004440000000000000000000000000000000000000000000000000000000000000000
6504444400000000443333367333334465055555555555554400b00b00b004440000000000000000000000000000000000000000000000000000000000000000
65044444000000004433b060070b334465055555555555554400bbbbbbb004440000000000000000000000000000000000000000000000000000000000000000
650444440000000044bb00600600bb4465050050777555554400b05b05b004440000000000000000000000000000000000000000000000000000000000000000
4444444400000000447700566500774465055550677555554400b00b00b004440000000000000000000000000000000000000000000000000000000000000000
444444440000000044bb06555570bb4465050050667555554400bbbbbbb004440000000000000000000000000000000000000000000000000000000000000000
444444440000000044bb06555560bb44650555550655555544400000000044440000000000000000000000000000000000000000000000000000000000000000
444444440000000044bb05655650bb44065055550655555544440000000444440000000000000000000000000000000000000000000000000000000000000000
444444440000000044bb00566500bb44065055550655555544444000004444440000000000000000000000000000000000000000000000000000000000000000
44444444000000004400000550000044006500000000000044444444444444440000000000000000000000000000000000000000000000000000000000000000
44444444000000004440000000000444000655555555555544444444444444440000000000000000000000000000000000000000000000000000000000000000
44444444000000004444444444444444000066666666666644444444444444440000000000000000000000000000000000000000000000000000000000000000
__label__
88888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
88888888888888888888888888888888888888888888888888888888888888888888888888888888888ff8ff8888228822888222822888888822888888228888
8888888888888888888888888888888888888888888888888888888888888888888888888888888888ff888ff888222222888222822888882282888888222888
8888888888888888888888888888888888888888888888888888888888888888888888888888888888ff888ff888282282888222888888228882888888288888
8888888888888888888888888888888888888888888888888888888888888888888888888888888888ff888ff888222222888888222888228882888822288888
8888888888888888888888888888888888888888888888888888888888888888888888888888888888ff888ff888822228888228222888882282888222288888
88888888888888888888888888888888888888888888888888888888888888888888888888888888888ff8ff8888828828888228222888888822888222888888
88888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
55555e555e5e5e5e5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555ee55e5e5e5e5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555e555e5e5e5e5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555eee5e5e5eee5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555d5d5ddd5dd55ddd5ddd5ddd55555ddd5d5d5ddd5d555dd55ddd5dd555dd55dd555555555555555555555555555555555555555555555555
55555555555555555d5d5d5d5d5d5d5d55d55d5555555d5d5d5d55d55d555d5d55d55d5d5d555d55555555555555555555555555555555555555555555555555
55555ddd5ddd55555d5d5ddd5d5d5ddd55d55dd555555dd55d5d55d55d555d5d55d55d5d5d555ddd555555555555555555555555555555555555555555555555
55555555555555555d5d5d555d5d5d5d55d55d5555555d5d5d5d55d55d555d5d55d55d5d5d5d555d555555555555555555555555555555555555555555555555
555555555555555555dd5d555ddd5d5d55d55ddd55555ddd55dd5ddd5ddd5ddd5ddd5d5d5ddd5dd5555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555665555556665655566555665555556655555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555656577756565655565656555555565555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555656555556655655565656665555565555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555656577756565655565655565555565555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555656555556665666566656655666556655555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555556665655566555665555556655555ccc55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555556565655565656555555565557775c5c55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555556655655565656665555565555555c5c55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555556565655565655565555565557775c5c55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555556665666566656655666556655555ccc55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555eee55ee5eee5555566655555cc55555566555555ee555ee5555555555555555555555555555555555555555555555555555555555555555555555555555
55555e555e5e5e5e55555565577755c55555565655555e5e5e5e5555555555555555555555555555555555555555555555555555555555555555555555555555
55555ee55e5e5ee555555565555555c55555565655555e5e5e5e5555555555555555555555555555555555555555555555555555555555555555555555555555
55555e555e5e5e5e55555565577755c55575565655555e5e5e5e5555555555555555555555555555555555555555555555555555555555555555555555555555
55555e555ee55e5e5555566655555ccc5755565655555eee5ee55555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555e5555ee55ee5eee5e555555566655555666565556655566577556665577555555555555555555555555555555555555555555555555555555555555
555555555e555e5e5e555e5e5e555555565657775656565556565655575555655557555555555555555555555555555555555555555555555555555555555555
555555555e555e5e5e555eee5e555555566555555665565556565666575555655557555555555555555555555555555555555555555555555555555555555555
555555555e555e5e5e555e5e5e555555565657775656565556565556575555655557555555555555555555555555555555555555551555555555555555555555
555555555eee5ee555ee5e5e5eee5555566655555666566656665665577556665577555555555555555555555555555555555555517155555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555517715555555555555555555
555555555eee5eee5555557556665555565655555566566656665555565655575cc55ccc5c55557555555eee5e5e5eee5ee55555517771555555555555555555
5555555555e55e5555555755565655555656555556555656566655555656557555c55c5c5c555557555555e55e5e5e555e5e5555517777155555555555555555
5555555555e55ee555555755566555555666577756555666565655555666575555c55ccc5ccc5557555555e55eee5ee55e5e5555517711555555555555555555
5555555555e55e5555555755565655555556555556555656565655555556557555c5555c5c5c5557555555e55e5e5e555e5e5555551171555555555555555555
555555555eee5e555555557556665575566655555566565656565575566655575ccc555c5ccc5575555555e55e5e5eee5e5e5555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555555566655555666556656565566565655555ccc55555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555555565655555565565656565655565657775c5c55555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555555566555555565565656565655566655555c5c55555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555555565655555565565656565655565657775c5c55555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555555566655755565566555665566565655555ccc55555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
5555555555555eee55ee5eee55555666566655555cc555555cc555555ee555ee5555555555555555555555555555555555555555555555555555555555555555
5555555555555e555e5e5e5e555555565565577755c5555555c555555e5e5e5e5555555555555555555555555555555555555555555555555555555555555555
5555555555555ee55e5e5ee5555555655565555555c5555555c555555e5e5e5e5555555555555555555555555555555555555555555555555555555555555555
5555555555555e555e5e5e5e555556555565577755c5557555c555555e5e5e5e5555555555555555555555555555555555555555555555555555555555555555
5555555555555e555ee55e5e55555666566655555ccc57555ccc55555eee5ee55555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555666566656565666555556565666566656665666557556665555566656555566556656665566577556665666557755555cc55ccc5c555555
55555555555555555556565656565655557556565656556555655655575556565555565556555656565656565655575555565565555755555c5c55c55c555555
55555555555555555565566556565665555556565665556555655665575556655555566556555656565656655666575555655565555755555c5c55c55c555555
55555555555555555655565656565655557556665656556555655655575556565555565556555656565656565556575556555565555755755c5c55c55c555575
55555555555555555666566655665655555556665656566655655666557556665575565556665665566556565665577556665666557757555c5c5ccc5ccc5755
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
5555555555555eee5ee55ee555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
5555555555555e555e5e5e5e55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
5555555555555ee55e5e5e5e55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
5555555555555e555e5e5e5e55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
5555555555555eee5e5e5eee55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555555566656555665556655555566555555555cc555555555555555555555555555555555555555555555555555555555555555555555555555555555
5555555555555656565556565655555556555575577755c555555555555555555555555555555555555555555555555555555555555555555555555555555555
5555555555555665565556565666555556555777555555c555555555555555555555555555555555555555555555555555555555555555555555555555555555
5555555555555656565556565556555556555575577755c555555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555555566656665666566556665566555555555ccc55555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555556665655566555665775566656555665556655555566557755555666555555555555555555555555555555555555555555555555555555555555
55555555555556565655565656555755565656555656565555555655555757775656555555555555555555555555555555555555555555555555555555555555
55555555555556655655565656665755566556555656566655555655555755555665555555555555555555555555555555555555555555555555555555555555
55555555555556565655565655565755565656555656555655555655555757775656555555555555555555555555555555555555555555555555555555555555
55555555555556665666566656655775566656665666566556665566557755555666555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555eee5ee55ee5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555e555e5e5e5e555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555ee55e5e5e5e555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555e555e5e5e5e555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
555555555eee5e5e5eee555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555eee5ee55ee55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555e555e5e5e5e5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555ee55e5e5e5e5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555e555e5e5e5e5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555eee5e5e5eee5555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555555555555555d5d5ddd5dd55ddd5ddd5ddd55555ddd5ddd5dd55d5d55dd5555555555555555555555555555555555555555555555555555555555555555
55555555555555555d5d5d5d5d5d5d5d55d55d55555555d55d5d5d5d5d5d5d555555555555555555555555555555555555555555555555555555555555555555
55555ddd5ddd55555d5d5ddd5d5d5ddd55d55dd5555555d55ddd5d5d5dd55ddd5555555555555555555555555555555555555555555555555555555555555555
55555555555555555d5d5d555d5d5d5d55d55d55555555d55d5d5d5d5d5d555d5555555555555555555555555555555555555555555555555555555555555555
555555555555555555dd5d555ddd5d5d55d55ddd555555d55d5d5d5d5d5d5dd55555555555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
55555666565655555566565556665566556655555656566656655666566656665575557555555555555555555555555555555555555555555555555555555555
55555565565655555655565556565655565555755656565656565656556556555755555755555555555555555555555555555555555555555555555555555555
55555565566555555655565556665666566655555656566656565666556556655755555755555555555555555555555555555555555555555555555555555555
55555565565655555655565556565556555655755656565556565656556556555755555755555555555555555555555555555555555555555555555555555555
55555565565656665566566656565665566555555566565556665656556556665575557555555555555555555555555555555555555555555555555555555555
55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
88888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
82888222822882228888822282288228888282228282822288888888888888888888888888888888888888888882828228822282228882822282288222822288
82888828828282888888828888288828882882828282828288888888888888888888888888888888888888888882828828828282828828828288288282888288
82888828828282288888822288288828882882228222822288888888888888888888888888888888888888888882228828822282828828822288288222822288
82888828828282888888888288288828882888828882888288888888888888888888888888888888888888888888828828888282828828828288288882828888
82228222828282228888822282228222828888828882888288888888888888888888888888888888888888888888828222888282228288822282228882822288
88888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888

__gff__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0000000000000000000000000000000012121212121212121212121212121212000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000c0c1c2c30000000000000000000022222222222222222222222222222222000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000d0d1d2d30000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e4e5c4c50000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000f4f5d4d50000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0101010101010101010101010101010100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
00020006136101c620226102d620376102e7200d7100b720000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000167001650016300162001620016100160014000120001200015000150000e0000e0000e0000e0000e000000000000000000000000000000000000000000000000000000000000000000000000000000
0110000000200122511e2511e2212a2551e255112550f2500d2550f25518250242513025124251142501425321253212530020000200002000020000200002000020000200002000020000200002000020000200
00050000136301e750106401030017100252002120025253062000320005200022000220024600226003b6001f6001b60017600126000b6000760003600026000160000000000000000000000000000000000000
010b00001325513255132551325501600016000160001600016000160013255132551325513255016000160001600016001325513255132550160001600016000160001600016001325513255132551f25501600
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
00 41020444
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344

