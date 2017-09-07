pico-8 cartridge // http://www.pico-8.com
version 8
__lua__
local futures={c=0}
local cam={}
-- active buildings
local blds={}
local blds_c=0
-- floor constants
flr_n=7
flr_h=10
flr_w=24
flr_zmax=(flr_n-1)*flr_h
local flr_ramps={
	{{6,5},{12,13}}, -- grey
	{{13},{12,13}}, -- violet
	{{9},{4,15}} -- brownish
}
-- camera
hh=63
hw=64
-- player settings
plyr_zmax=flr_zmax+16
plyr_zmin=0
plyr_vmax=1
local plyr={
	x=0,
	y=0,
	z=plyr_zmax,
	zi=1,
	vx=0,
	vy=1,
	lives=3,
	hit=false,
	crash_z=56,
	playing=true,
	crashing=false,
	safe=false,
	side=0,
	cam_yoffset=24,
	cam_zoffset=0
}
plyr_r=12
plyr_r2=plyr_r*plyr_r
plyr_rotor={128,131,134}
plyr_body={64,66,68}
plyr_body3d={141,137,139}
plyr_rotor3d={
	[1]={187,{-10,5,true},{8,-2,false,true}},
	[2]={185,{-10,2,false,false},{8,2,true,false}},
	[3]={187,{-8,-2,true,true},{8,5}}
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

-- special fx
local fxs={}
fxs_c=0

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

-- futures
function futures:update()
	local n=self.c
	self.c=0
	for i=1,n do
		local f=self[i]
		if(coresume(f)) then
			self.c+=1
			self[self.c]=f
		end
	end
end
function futures:add(fn)
	self.c+=1
	self[self.c]=cocreate(fn)
end

---------------------------
-- helpers
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
			if(s!=0) then
				sx,sy=band(shl(s,3),127),shl(flr(s/16),3)
				sspr(sx,sy,8,8,
					ixl,y+j*scale,scale,scale)
				sspr(sx,sy,8,8,
					ixr,y+j*scale,-scale,scale)
			end
		end
	end
end
function recthstrip(x0,y0,x1,y1,n,cols)
	local x00,y00=x0,y0
	x0,y0=min(x00,x1),min(y00,y1)
	x1,y1=max(x00,x1),max(y00,y1)
	local dy=(y1-y0+1)/n
	if(dy<1) n/=2 dy*=2
	for i=1,n do
		rectfill(x0,y0,x1,y0+dy-1,cols[i%#cols+1])
		y0+=dy
	end
end
function rectvstrip(x0,y0,x1,y1,n,cols)
	local x00,y00=x0,y0
	x0,y0=min(x00,x1),min(y00,y1)
	x1,y1=max(x00,x1),max(y00,y1)
	local dx=(x1-x0+1)/n
	if(dx<1) n/=2 dx*=2
	for i=1,n do
		rectfill(x0,y0,x0+dx-1,y1,cols[i%#cols+1])
		x0+=dx
	end
end
function rectvhstrip(x0,y0,x1,y1,n,nv,cols)
	local x00,y00=x0,y0
	x0,y0=min(x00,x1),min(y00,y1)
	x1,y1=max(x00,x1),max(y00,y1)
	local dx=(x1-x0+1)/n
	if(dx<1) n/=2 dx*=2
	local dy=(y1-y0+1)/nv
	if(dy<1) nv/=2 dy*=2
	for i=1,n do
		local ramp=cols[i%#cols+1]
		local y=y0
		for j=1,nv do
		rectfill(x0,y,x0+dx-1,y+dy-1,ramp[j%#ramp+1])
			y+=dy
		end
		x0+=dx
	end
end
function normz(dx,dy)
	-- avoid overflow
	dx/=128
	dy/=128
	local dist=dx*dx+dy*dy
	if(dist>0) then
		dist=sqrt(dist)
		dx/=dist
		dy/=dist
	end
	return dx,dy
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
			-- avoid conflict!
			sset(sx+8*i,sy,0)
			sset(sx+8*i+1,sy,0)
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
	self.funcs[i+1]=fn
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
	return self.floor[min(y+1,#self.floor)]
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
		-- pico8 trig is inverted!
	self.alpha=1-atan2(focal,hh)
	self:rotate(beta or 0)
end
function cam:rotate(beta)
	self.beta=beta
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
function cam:track(x,y,z)
	self.x=x
	self.y=y-self.cb*self.focal
	self.z=z-self.sb*self.focal
end

--------------------
--- tank -----------
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
	--circfill(x,y,we,sget(8+t,0))
	sspr(112,56,8,8,x-we/2,y-we/2,we,we,time_t%2==0,time_t%4==0)
end
function fxs:update()
	local n=fxs_c
	fxs_c=0
	for i=1,n do
		local fx=self[i]
		if(fx.t>=time_t) then
		 fx:update()
			zbuf:write(fx)
			fxs_c+=1
			self[fxs_c]=fx
		end
	end
end
function fxs:make_blast(x,y,z,dy,dz,w,tmax)
	local fx={
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
	fxs_c+=1
	self[fxs_c]=fx
	return fx
end
function fxs:make_smoke(x,y,z)
	local dly=0.5*30+rnd(10)
	local fx={
		x=x,
		y=y,
		z=z,
		dly=dly,
		t=time_t+dly,
		update=nop,
		draw=smoke_draw
	}
	fxs_c+=1
	self[fxs_c]=fx
	return fx
end

---------------------
--- player ----------
function plyr:init()
	self.x=0
	self.y=0
	self.z=plyr_zmax
	self.lives=3
	self.hit=false
	self.fire_dly=0
	self.crashing=false
	self.playing=true
	futures:add(function()
		self.safe=true
		for i=1,2*30 do yield() end
		self.safe=false
	end)
end
function plyr:draw(x,y,z,w)
	if(self.crashing) return
	if(self.safe and band(time_t,1)==0) return
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
function plyr:die_async()
	for i=0,30 do
		local t=i/30
		self.z=lerp(self.crash_z,0,t)
		self.vy=lerp(self.vy,0,t)
		if(i%4==0) then
			fxs:make_blast(self.x,self.y,self.z,0,0,12)
		end
		yield()
	end
	self.z=0
	self.vy=0
	if(self.lives<=0) then
		for i=1,30 do yield() end
		sm:push(game_over)
		return
	end
	self.crashing=false
	self.playing=true
	for i=1,3*30 do
		yield()
	end
	self.safe=false
end
function plyr:die()
	-- avoid rentrancy
	if(self.safe) assert()
	sfx(1)
	self.lives-=1
	self.crash_z=self.z
	self.crashing=true
	self.safe=true
	self.playing=false
	futures:add(function()
		self:die_async()
	end)
end
function plyr:blt_hit(blt)
	if(self.safe) return false
	if(circ_coll(self,plyr_r,blt,blt_r)) then
		self:die()
		return true
	end
	return false
end
function plyr:update()
	if (self.playing) then
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
		self.z=mid(self.z,plyr_zmin,plyr_zmax)
	
		if(btn(5) or self.z==0) then
			self.vy-=0.25
		else
			self.vy+=max(0.1, 0.1*self.vy)
		end
		self.vy=mid(self.vy,0,plyr_vmax)
		self.y+=self.vy
	
		-- debug
		--if(btn(5)) then
		--	cam.beta+=0.01
		--	if(cam.beta>1) cam.beta=0
		--end
		
		-- fire 
		if (btn(4) and self.fire_dly<=time_t and blts_c<blts_n) then
			blts:make(self.x,self.y+5,self.z-0.5,0,3,-3,self.side)
			self.fire_dly=time_t+0.2*30
		end
	end
	-- cam world position
	cam:track(
		self.x/2,
		self.y+self.cam_yoffset,
		max(8,self.z)+self.cam_zoffset)
	self.zi=zbuf:write(self,self)
end
function plyr:resolve_collisions()
	-- just (re)spawned
	if (self.safe) return
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

-------------------------------
-- helo
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

-----------------------------
-- building
function flr_draw(self,xe,ye,z,w)
	local we=flr_w*w
	if(self.i==1) then
		-- shadow
		local se=1.2*we
		rectfill(xe-se,ye-se,xe+se,ye+se,3)
	end
	local x,y=xe-we,ye-we
	-- todo: rectstrip
	local c=self.ramps[band(self.i,1)+1][1]
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
function flr_chase_draw(self,x,y,z,w)
	local ww=flr_w*w
	local wh=4*flr_w*w
	local k=flr(ww/4+0.5)
	if(x<64) then
		recthstrip(x+ww-k,y-wh,x+ww,y,16,self.ramp)
	else
		recthstrip(x-ww,y-wh,x-ww+k,y,16,self.ramp)
	end
end
function roof_chase_draw(self,x,y,z,w,i)
	local ww=flr_w*w
	local wh=4*flr_w*w
	--sspr(72,0,32,32,x-ww/2,y-wh,ww,wh)
 rectvhstrip(x-ww,y-wh,x+ww,y,9,16,self.ramps)
end
function spawn_building(x,y,ramps)
	local b={
		x=x,y=y,z=0,touch=0,
		floors={}
	}
	if(cam:is_top_down()) then
		for i=0,flr_n-1 do
			add(b.floors,{x=x,y=y,z=i*flr_h,i=i,ramp=ramps[i%2+1],draw=flr_draw})
		end
		add(b.floors,{x=x,y=y,z=flr_n*flr_h,i=8,ramps=ramps,draw=roof_draw})
	else
		for i=1,flr_n do
			add(b.floors,{x=x,y=y+i*flr_h,z=0,i=i,ramp=ramps[i%2+1],draw=flr_chase_draw})
		end
		add(b.floors,{x=x,y=y,z=0,i=8,ramps=ramps,draw=roof_chase_draw})
	end
	blds_c+=1
	blds[blds_c]=b
	return b
end

------------------------------
-- bullet
function blt_update(self)
		self.z+=self.dz
		if(self.z<=0) then
			fxs:make_blast(self.x,self.y,0,0,0,8)
			return false
		elseif(self.t>time_t) then
			self.x+=self.dx
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
			if(flr(dt)%2==0 and plyr.z>self.z) then
				local dx,dy=normz(plyr.x-self.x,plyr.y-self.y)
				self.dx=dx*2
				self.dy=dy*2
			end	
			if(time_t%3==0) fxs:make_smoke(self.x,self.y,self.z)
			self.x+=self.dx
			self.y+=self.dy
			return true
		end
		fxs:make_blast(self.x,self.y,0,0,0,8)
		return false
end
function msl_draw(self,x,y,z,w)
	local we=flr(blt_w*w)+1
	sspr(48,48,8,8,x-we,y-we,2*we,2*we)
end
function blts:make_tracker(x,y,z,dz,side)
	local dx,dy=normz(plyr.x-x,plyr.y+20-y)
	dx*=2
	dy*=2
	return self:make(x,y,z,dx,dy,dz,side)
end
function blts:make(x,y,z,dx,dy,dz,side)
	local b={
		x=x,
		y=y,
		z=max(z,0),
		dx=dx,
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
	local b={
		x=x,
		y=y,
		z=max(z,0),
		dx=0,
		dy=0,
		dz=dz,
		t=time_t+3.5*30,
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
function boss:init(name,x,y,z)
	boss.enabled=true
	boss.x=x
	boss.y=y
	boss.z=z
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
	y+=218
	futures:add(function()
		plyr.playing=false
		local vmax=plyr_vmax
		local z=plyr.z
		for i=0,1*30 do
			local t=i/(1*30)
			plyr_vmax=lerp(vmax,0.5,t)
			plyr.z=lerp(z,32,t)
		end
		plyr.playing=true
		plyr_zmax=32
		plyr_zmin=32
		plyr_vmax=0.5
	end)
	boss:init("mermaster",x,y,0)
	boss.mark="marine fortress ba-001"
	boss.draw=bship_draw
	local parts={
		{0,0,0,0,0,0},
		{0,0,0,0,0,0},
		{0,1,0,0,1,0},
		{0,1,0,0,1,0},
		{0,1,0,0,1,0},
		{0,0,1,1,0,0},
		{0,1,2,2,1,0},
		{0,0,2,2,0,0},
		{0,1,0,0,1,0}}
		
	-- parts
	local py=148
	for j=1,#parts do
		for i=1,6 do
			local t=nil
			local ptype=parts[j][i]
			local px=96*(i-1)/5-48
			if(ptype==1) then
				t={
					x=x+px,
					y=y+py,
					z=0.5,
					lx=px, --local pos
					ly=py,
					sx=16,
					sy=112,
					fire_dly=time_t+rnd(30)+1.2*30,
					update=turret_update
				}
			elseif(ptype==2) then
				t={
					x=x+px,
					y=y+py,
					z=0.5,
					lx=px, -- local pos
					ly=py,
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
	map(2,0,x-8*8/2,y-22*8/2,8,32)
	palt(4,true)
	palt(3,false)
	for i=1,#self.parts do
		local p=self.parts[i]
		sspr(p.sx,p.sy,16,16,x+p.lx-8,y-p.ly-8,16,16)
	end
	palt(4,false)
	palt(3,true)
end
function turret_update(self,p)
	if(self.fire_dly<time_t) then
		blts:make_tracker(self.x,self.y,0.5,1,0)
		self.fire_dly=time_t+rnd(30)+2*30
	end
end
function mslturret_update(self,p)
	if(self.fire_dly<time_t) then
		blts:make_msl(self.x,self.y,0.5,1,0)
		self.fire_dly=time_t+rnd(30)+3*30
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

game_screen={}
function game_screen:update()
	futures:update()
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
				blts:make_tracker(tk.x,tk.y,0.5,1,tk.side)
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
	local imax=-1
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
					if(ze<=16) then
						sspr(16,band(v/2,7)+8,8,1,xe-ze/2,64-i,ze,1)
					else					
						sspr(24,band(v,15),16,1,xe-ze/2,64-i,ze,1)
					end
				end
				imax=64-i
			end
		end
		a+=da
		a_fix+=da
	end
	if(imax>=0) then
		rectfill(0,0,127,imax-16,12)
		map(16,1,0,imax-16,16,2)
		for i=1,24,2 do		
			spr(176,-plyr.x/8+12*i-32,imax-8,2,1)
		end
		for i=1,3 do
			spr(178,-plyr.x/16+32*i+16,imax-24,3,1)
			spr(178,-plyr.x/8+48*i-32,imax-32-4*(i%2),3,1)
			spr(178,-plyr.x/4+56*i-64,imax-52-4*(i%3),3,1)
		end
	end
end

function game_screen:draw_spd(x,y)
	rectfill(x,y,x+12,y+4,1)
	rectfill(x+13,y,x+30,y+4,0)	
	for i=0,4 do
		pset(x+13+4*i,y,8)
		pset(x+13+4*i,y+4,8)		
	end
	rectfill(x+13,y+1,x+13+16*plyr.vy,y+3,11)
	
	print("spd",x+1,y,8)
	
	if(plyr.z==0 and band(time_t,31)>15) then
		printer
		 :centered()
			:shaded(9)
			:print("take off",64,100,10)
	end
	-- debug (beta angle)
	circ(118,17,8,1)
	line(118,17,118+8*cam.cb,17-8*cam.sb,8)
	print(flr(360*cam.beta),118-6,17+12,1)
end

function draw_shadow()
	if (time_t%2==0) then
		local x,y,z,w=cam:project({x=plyr.x,y=plyr.y,z=0})
		if(cam:is_top_down()) then
			local ww=8*w
			local wh=24*w
			sspr(56,32,8,24,x-ww/2,y-wh/2,ww,wh)
		else
			spr(189,x-8,y,1,1)
			spr(189,x,y,1,1,true)
		end
	end
end

function game_screen:draw()
	palt(0,false)
	palt(3,true)
	draw_floor()
	draw_shadow()
	--cls(0)
	zbuf:draw()
	for i=0,plyr.lives-1 do
		spr(2,2+i*9,128-9)
	end
	palt()
	
	rectfill(0,0,127,7,1)
	print("\150:"..flr(100*stat(1)+0.5).."% \152:"..flr(stat(0)+0.5).."kb",1,1,7)
	self:draw_spd(95,120)
	--print("blds:"..blds_c,64,120,0)
	--rectfill(0,8,32,24,0)
	--print("tks :"..tks_c,0,9,12)
	--print("blts :"..blts_c,0,18,12)
	--print_map(0,32)
	--zbuf:print(0,16,1)
	--debug_draw()
	--world:print(0,12)

	if(boss.enabled and boss.msg_dly>time_t) then
		boss:msg_draw(36)
	end
end
function to_chase()
	futures:add(function()
		helos_c=0
		tks_c=0
		zbuf=make_zbuf(32,flr_h)
		for i=0,30 do
			local t=i/30
			cam:rotate(lerp(0.75,1,t))
			plyr.cam_yoffset=lerp(24,0,t)
			plyr.cam_zoffset=lerp(0,12,t)
			yield()
		end
		cam:init(96,-196-plyr_zmax,0)
		plyr.cam_yoffset=0
		plyr.cam_zoffset=12
		flr_n=4
	end)
end
function game_screen:init()
	time_t=0
	cam:init(96,-96-plyr_zmax,0.75)
	-- reset entities
	helos_c=0
	tks_c=0
	blts_c=0
	fxs_c=0
	
	--zbuf=make_zbuf(flr_n,flr_h)
	zbuf=make_zbuf(8,flr_h)
	
	world
		:init(0,56,6*8)
		:register(4,spawn_tk)
		:register(6,function(x,y) spawn_building(x,y,flr_ramps[1]) end)
		:register(7,function(x,y) spawn_building(x,y,flr_ramps[2]) end)
		:register(13,function(x,y) spawn_building(x,y,flr_ramps[3]) end)
		:register(8,spawn_helo)
		:register(9,spawn_bship)
		:register(10,to_chase)
		:register(11,to_top_down)

	--[[
	spawn_building(0,0)
	spawn_building(-48,56)
	spawn_building(48,56)
	]]
	
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
function rooster:apply(fn)
	for i=1,#self.rows do
		local row=self.rows[i]
		if(row.dly<time_t) then
			for j=1,#row.chars do
				local c=row.chars[j]
				if (c.dly<time_t) fn(c)
			end
		end
	end
end
function rooster:update()
	self:apply(char_update)
end
function rooster:draw()
	self:apply(char_draw)
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
				dst={x=x,y=0,z=z}
			}
			add(row.chars,char)
			dt+=1
		end
		x+=8
	end
	add(self.rows,row)
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
		sm:push(game_screen)
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
	cam:track(0,0,0)
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
	cam:track(0,0,0)
end

-- game loop
function _update60()
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
	sm:push(game_screen)
end

__gfx__
88888888a95566673333333305555555555555507777777777777777777777777777777777777777777777777777777777777777333333304333333300000000
88000088000000003777655505555555555555507555557777755555555555777775555766666666666666666666666666666666333334304373333300000000
80800808000000001333133305555555555555507566665555566666666666555556665755555555555555555555555555555555333330004463333300000000
8008800800000000151111c305555555555555507566666666666666666666666666665766666666666666666666666666666666333330355353333300000000
80088008000000003331111105555557755555507566666666666666666666666666665755555555555555555555555555555555333333066433333300000000
80800808000000003335555305555557755555507566666666666666666666666666657766666666666666666666666666666666333330400943333300000000
88000088000000003333333305555557755555507756666666666666666677777776657755555555555555555555555555555555333334c99793333300000000
8888888800000000333333330555555775555550775666666666666666667557557665776666666666666666666666666666666633330cc99c79333300000000
0040444444490440055755500555555775555550775666666666666666667777777665775555555555555555555555555555555534440cc99cc9999300000000
00404444444904400557555005555557755555507756666666666666666675575576665766666666666666666666666666666666300004c94c99000300000000
0040440000490440055555500555555555555550775666666666666666667777777666575555555555555555555555555555555534030c4999c4349300000000
04004499994400400555555005555555555555507756666666666666666675575576665766666666666666666666666666666666333331c94c13333300000000
04004400004400400555555005555555555555507566677777777777666677777776665755555555555555555555555555555555333303044430333300000000
04004000000400400555555005555555555555507566675555555557666655555556665766666666666666666666666666666666333303333330333300000000
00000090040004400557555005555555555555507566675666666657666655555556665755555555555555555555555555555555333333333333333300000000
00900900004004400557555005555555555555507566675677766657666666666666665766666666666666666666666666666666333333333333333300000000
0440900000000440eeeeeeee33338888333995137566675677777657666666666666665755555555555555555555555555555555333333333333333300000000
0440900000000440cccccccc33388899899999517566675677755657666666666666665766666666666666666666666666666666333333333333333300000000
9440900000000440cccccccc33888999aaaaa9957566675677777657666666666666665755555555555555555555555555555555333333304333333300000000
9440090000000440eeeeeeee331889aaaa77a9957566675677755657666666666666665766666666666666666666666666666666333333304333333300000000
9444009000004440eeeeeeee33199aa7777aa9887566675677766657666666666666657755555555555555555555555555555555333333304333333300000000
9000400000040040cccccccc38119a77777a99557756675655566657666666666666657766666666666666666666666666666666333300304393333300000000
9999440900444440eeeeeeee38899aaa9a7a998177566756666666576666666666666577555555555555555555555555555555553334444049c9a33300000000
0000440900440000eeeeeeee3501889aa777aa1377566777777777776666666666666577666666666666666666666666666666663344440099999a3300000000
0000440900440000eeeeeeee3558899777777a937756655555555555666666666666657755555555555555555555555555555555334440044449993300000000
000000090000000077777777158999a777aa99997756655555555555666666666666665766666666666666666666666666666666333000000000033300000000
0440440900440440eeeeeeee518999aaa999888875666666666666666666666666666657555555555555555555555555555555553300000049499a3300000000
0440440900440440777777775511888899988888756666666666666666666666666666575555555000000000000000000055555500404040404049aa00000000
0990440900440990eeeeeeee15511858899818887566666666666666666666666666665755555550c77cc0c77cc0c77cc0555555444494949494949900000000
33333309003333337777777731555855588115517566655555666666666666555556665755555550cc77c0cc77c0cc77c0555555444040404040444400000000
33333309003333337777777733311158888551137555577777555555555555777775555755555550ccc770ccc770ccc770555555000000000000000000000000
33333309003333337777777733333331111113337777777777777777777777777777777755555550cccc70cccc70cccc70555555000333333333300000000000
33333333633333333333333363333333333333336333333333888833333003333333333333335333333333333333333333337333333333333333333333333333
33333330133333333333333013333333333333301333333338aaaa83330000333333333330935333333333333333333330937333333333333333333555333333
3333333161333333333333316333333333333331733333338aaaaaa8330000333333339440956333333333333333339440956333333333333333300090443333
3333331016333333333333101633333333333310173333338aa77aa8300000033333339440937333333333333333339440935333333333333333300094443333
3333350001633333333336000163333333333600017333338aa77aa8300000033333333330937333333333333333333330935333333333333333330074433333
3333350001603333333336000163333333330600017333338aaaaaa8300000033333333330933333333333333333333330000033333333333333333079333333
33333506116033333333360061633333333306001673333338aaaa83300000033333300330933333333333333333333330000033333333333333333049333333
33333500116033333333360011633333333306001173333333888833300000033333000330933333333333333333333330000333333333333333333049333333
33333306113033333333330061333333333303001633333300000000300000033330000330933333303333333333333330000333333333333533445049444335
3333330013003333333330001003333333330030103333330000000033000033333300003093333300033333333333333000033333333333354444509a944445
3333330013303333333330301303333333330330103333337770707033000033333330003093333000003333333333333000333333333333354444509aa44445
3333333013333333333333301333333333333330133333337070707033300333333333000094330000003333333333330000333333333333354444009aa44445
3333333013333333333333301333333333333330133333337070770033300333333333300044300003333333003333330000333333333333353333005aa33335
333333301333333333333330133333333333333013333333777070703330033333333333009400033333333300000333040433333333333333333300c5a33333
333333301333333333333330133333333333333013333333000000003330033333333330000000333333333300000000000043333333333333333300c5a33333
333333301333333333333330133333333333333013333333000000003330033333333330006043333333333300000000006000000000003333333300c5a33333
33336330133633333336333013336333333363301336333333388333003003003333333000004333333333333333333000004000000000333333333059333333
3333666066663333333666601666633333336665166633333300003300000000333333000cc0033333333333333333310c04c333000000333333333049333333
3333633113363333333633301333633333336330033633333005600300300300333300000c70003333333333333333310004c333333000333333333049333333
33333311133333333333333013333333333333300033333380577608333003333000000010070003333333333333333010074333333333333333333049333333
33333311133333333333333013333333333333300033333380567608333003333000003301143000333333333333333300043333333333333333333597333333
33333333333333333333333333333333333333333333333330056003333333333300033335533000033333333333333300033333333333333333333597333333
33333333333333333333333333333333333333333333333333000033333333333330333333333300003333333333333300033333333333333333333353333333
33333333333333333333333333333333333333333333333333388333333333333333333333333300033333333333333300033333333333333333333353333333
c10000000000000000000000bd000000000000000000000000000000000000003333333333333330333333333333333000033333333333333377333300000000
00000000000000000000000000000000000000000000000000000000000000003333333333333333333333333333333000033333333333333777773300000000
00000000000000000000000000000000000000000000000000000000000000003333333333333333333333333333333333333333333333337777777300000000
00000000000000000000000000000000000000000000000000000000000000003333333333333333333333333333333333333333333333333777777700000000
90000000000000000000000000000000000000000000000000000000000000003333333333333333333333333333333333333333333333333777777700000000
00000000000000000000000000000000000000000000000000000000000000003333333333333333333333333333333333333333333333333377777700000000
00000000000000000000000000000000000000000000000000000000000000003333333333333333333333333333333333333333333333333377777300000000
00000000000000000000000000000000000000000000000000000000000000003333333333333333333333333333333333333333333333333337773300000000
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
33333666666333333333333333333333333333330000000000000000000000000000000033333333333333330033333333333333333333333333333300000000
33333cccccc333333333333333333333333333330000000000000000000000000000000033333333333333330000333333333333333333333333333300000000
3dddd666666333333333333333333333333333330000000000000000000000000000000033333333333333333300003333333333333333333333333300000000
5555dcccccc333333333333333333333333333330000000000000000000000000000000000000000000000003300330033333333333000003333333300000000
6666d666666333333333333333777333337733330000000000000000000000000000000033333333333300003333003300333333300000003333333300000000
5555dcccccc5dd333333333337777773377777330000000000000000000000000000000033333333000000333333330033003333000000003333333300000000
6666d6666665dd333333777777777777777777730000000000000000000000000000000033333333333333333333333333330033300000003333333300000000
5555dcccccc5dd336666666666666666666666630000000000000000000000000000000033333333333333333333333333333333333000003333333300000000
33333333505555555055555544499999444444444444444444444444000000000050505000000000999994443336505555555505333333330000000000000000
33333366505555555055555544966666444444444444444444444444000000000cccc6cc00000000666669443336505555555505663333330000000000000000
33333655055555550555555549655555444444444444444440567744000000000766666600000000555556943365055555555550556333330000000000000000
33336500055555550555555549655555444444444444444440007644000000000577777700000000555556943365055555555550005633330000000000000000
33365055555555555555555549655555666666666666666640007644000000000566666600000000555556943650555555555555550563330000000000000000
33650555555555555599995549655555555555555555555544056444000000000565566600000000555556943650555555555555555056330000000000000000
36505555555555555596666549655555555555555555555544005444000000000566666600000000555556946505555555555555555505630000000000000000
65055555555555555596555549655555556000000000000544444444000000000565566600000000555556946505555555555555555550560000000000000000
33365055333333365599995549655555556555555555500543333344000000004000000000000000555556944405633363333333550563330000000000000000
333650553333333655966965496555555565005dd500500507777704000000000555555500000000555556944405633363333333550563330000000000000000
336505553333336555965965496555555565555dd55550050b0b0b04000000000576076000000000555556944405633356333333555056330000000000000000
336505553333336555999965496555555565005dd50050050bbbbb04000000000555555500000000555556944405633356333333555056330000000000000000
3650555533333650555666654965555555655555555550050b0b0b04000000000576076000000000555556944405633305633333555505630000000000000000
3650555533333650555555554965555500000000000000000bbbbb04000000000555555500000000555556944405633305633333555505630000000000000000
65055555333365056666666644966666555555555555555540000044000000000576076000000000666669444405633350563333555550560000000000000000
65055555333365050000000044499999666666666666666644444444000000000555555500000000999994444405633350563333555550560000000000000000
33365044555555554444444444444444333650444444444444443030303444440556666666666550000000004405633300000000000000000000000000000000
33365044555555554444333333334444333650444444444444443030303444440560000000000650000000004405633300000000000000000000000000000000
33365044555555554443030330303444333650444444444444403333333044440560555555550650000000004405633300000000000000000000000000000000
33365044555555554433030330303344333650444444444444407777777044440656666666666560000000004405633300000000000000000000000000000000
3336504455555555443333333333334433365066666666664440b05b05b044440600000000000060000000006605633300000000000000000000000000000000
3336504455555555443333367333334433365055555555554440b00b00b044440605555555555060000000005505633300000000000000000000000000000000
33365044555555554033b060070b330433365055555555554440bbbbbbb044440656666666666560000000005505633300000000000000000000000000000000
333650445555555540bb00600600bb0433365055550777554440b05b05b044440600000000000060000000005505633300000000000000000000000000000000
4444444455555555407700566500770433336505550677554440b00b00b044440605555555555060000000005056333300000000000000000000000000000000
444444445555555540bb06555570bb0433336505550667554440bbbbbbb044440656666666666560000000005056333300000000000000000000000000000000
444444445555555540bb06555560bb04333365055550655544440000000444440600000000000060000000005056333300000000000000000000000000000000
444444445555555540bb05655650bb04333336505550655544440000000444440605555555555060000000000563333300000000000000000000000000000000
444444445555555540bb00566500bb04333336505550655544444444444444440656666666666560000000000563333300000000000000000000000000000000
44444444555555554000000550000004333333650000000044444444444444444060000000000604000000005633333300000000000000000000000000000000
44444444666666664400000000000044333333365555555544444444444444444060555555550604000000006333333300000000000000000000000000000000
44444444000000004444444444444444333333336666666644444444444444444406666666666044000000003333333300000000000000000000000000000000
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
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000c0cd00000000000000000022222222222222222222222222222222000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000d1c2ccdc000000000000000032323232323232323232323232323232000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000cbd200dd000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000d1c1e1e1ccdc0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000cbe1e1e1e1dd0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000d1c1f1f1f1f1ccdc00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0c3e1e1caf0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0d3e1e1daf0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0c6c6c6c6f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0c8f0f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0d8f0f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0e8e9f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f8f9f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0c6f0f0f0f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f0f0f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0c3cac3caf0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0d3dad3daf0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f0f0f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0c6c6f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0e8e9f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f8f9f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f0f0f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0c6f0f0f0f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0e5c4c5e5c4e5db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f5d4d5f5d4f5db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f0f0f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f0f0f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f0f0f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f0f0f0f0db00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e4e5c4c5e5c4e5eb00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000f4f5d4d5f5d4f5fb00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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

