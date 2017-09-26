pico-8 cartridge // http://www.pico-8.com
version 8
__lua__
local before_update={c=0}
local after_draw={c=0}
-- active buildings
local blds={}
local blds_c=0
-- floor constants
flr_n=7
flr_h=10
flr_w=24
flr_zmax=flr_n*flr_h
local flr_ramps={
	{{6,5},{12,13}}, -- grey
	{{13},{12,13}}, -- violet
	{{9},{4,15}} -- brownish
}
-- camera
local cam_x,cam_y,cam_z
local cam_focal,cam_zfar
cam_beta=0
cam_sb,cam_cb=0,0
hh=64
hw=64
-- player settings
good_side=0
bad_side=1
local plyr_zmax,plyr_zmin
plyr_vmax=1
local plyr={
	x=0,
	y=0,
	z=plyr_zmax,
	side=good_side
}
plyr_vx=0
plyr_vy=1
plyr_lives=3
plyr_score=0
plyr_hit=false
plyr_crash_z=56
plyr_playing=true
plyr_crashing=false
plyr_safe=false
plyr_side=0
plyr_cam_yoffset=24
plyr_scale=1
local plyr_blt_dy,plyr_blt_dz
plyr_r=12
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
blt_r=4
blt_r2=blt_r*blt_r
-- zbuffer
local zbuf={}
local zbuf_n
local zbuf_phys={}
local zbuf_phys_n
-- enemies
local nmies={}
local nmies_c
-- tank constants
tk_w=16
tk_h=24
tk_chase_w=16
tk_chase_h=16
-- helos constants
helo_w=24
helo_h=32
helo_r=12
helo_r2=helo_r*helo_r
helo_body={64,88}
-- map
world_scaley=96 -- 1 cell=1 half screen
world_scalex=32
world_ymax=0
local world_far -- how far ahead stuff is created
local world_out -- out of sight
local world_cur
world_factories={} -- entity spawners
local road={} -- road x-offset
road_ymax=0
local world_floor={} -- floor color ramps
local floor_ymax,floor_ymin
-- special fx
local fxs={}
local fxs_c=0

-- boss
local boss={}
local boss_parts={}
local boss_struct={}

boss_enabled=false

-- fade ramp + screen manager
_shex={}
_shexstr="0123456789abcdef"
for i=1,16 do
	_shex[sub(_shexstr,i,i)]=i-1
end
_pl={"00000015d67",
     "0000015d677",
     "0000024ef77",
     "000013b7777",
     "0000049a777",
     "000015d6777",
     "0015d677777",
     "015d6777777",
     "000028ef777",
     "000249a7777",
     "00249a77777",
     "00013b77777",
     "00013c77777",
     "00015d67777",
     "00024ef7777",
     "0024ef77777"}
_pl_from=0.5
function fade(to,f)
	f=mid(f,1,32) -- sensible boundaries
	to=mid(to,0,1)
	futures_add(function()
		for i=0,f do
			local t=i/f
			local pix=flr(10*lerp(_pl_from,to,t))+1
			for x=0,15 do
				pal(x,_shex[sub(_pl[x+1],pix,pix)],1)
			end
			yield()
		end
		_pl_from=to
	end,after_draw)
end
-- screen manager
local sm_t,sm_cur,sm_next,sm_dly=0,nil,nil,0
function sm_push(s)
	sm_t=0
	if sm_cur then
		sm_dly=sm_t+8
		sm_next=s
		fade(0,8)
	else
		sm_cur=s
		sm_cur:init()
	end
end
function sm_update()
	sm_t+=1
	if sm_next then 
		if sm_dly<sm_t then
			sm_cur=sm_next
			sm_next=nil
			sm_cur:init()
			fade(0.5,8)
		end
	else
		sm_cur:update()
	end
end
function sm_draw()
	sm_cur:draw()
end

-- decompress pic --
-- written by dw817 (david w)
-- http://writerscafe.org/dw817
set="abcdefghijklmnopqrstuvwxyz()[]{}"
function str2mem_async(t,m)
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
		if band(m,255)==0 then 
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
-- collision helpers
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

-------------------------
-- futures
function futures_update(futures)
	futures=futures or before_update
	local n=futures.c
	futures.c=0
	for i=1,n do
		local f=futures[i]
		local r,e=coresume(f)
		if r then
			futures.c+=1
			futures[futures.c]=f
		--[[
		else
			printh("exception:"..e)
		]]
		end
	end
end
function futures_add(fn,futures)
	futures=futures or before_update
	futures.c+=1
	futures[futures.c]=cocreate(fn)
end

---------------------------
-- helpers
function nop() end

function map2(cx,cy,sx,sy,cw,ch)
	local i=cw-1
	while(i>=0) do
		local y=sy
		for j=0,ch-1 do
			local s=mget(cx+i,cy+j)
			if (s!=0) spr(s,sx,y,1,1,true)
			y+=8
		end
		i-=1
		sx+=8
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
		-- minor strip optimisation
		if x0<128 and x0+dx-1>0 then
			for j=1,nv do
				rectfill(x0,y,x0+dx-1,y+dy-1,ramp[j%#ramp+1])
				y+=dy
			end
		end
		x0+=dx
	end
end
function sfx_speed(sfx, speed)
  poke(0x3200 + 68*sfx + 65, speed)
end
function normz(dx,dy)
	-- avoid overflow
	dx/=128
	dy/=128
	local dist=dx*dx+dy*dy
	if dist>0 then
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
-- https://github.com/morgan3d/misc/tree/master/p8sort
function sort(t,n,cmp)
	if (n<2) return
 local i,j,temp
 local lower = flr(n/2)+1
 local upper = n
 while 1 do
  if lower>1 then
   lower-=1
   temp=t[lower]
  else
   temp=t[upper]
   t[upper]=t[1]
   upper-=1
   if upper==1 then
    t[1]=temp
    return
   end
  end

  i=lower
  j=lower*2
  while j<=upper do
   if j<upper and cmp(t[j],t[j+1]) then
    j += 1
   end
   if cmp(temp,t[j]) then
    t[i] = t[j]
    i = j
    j += i
   else
    j = upper + 1
   end
  end
  t[i] = temp
 end
end

-----------------------
-- print text helper
txt_center=false
txt_shade=-1
function txt_options(c,s)
	txt_center=c or false
	txt_shade=s or -1
end
function txt_print(s,x,y,col)
	if txt_center then
		x-=flr((4*#s)/2+0.5)
	end
	if txt_shade!=-1 then
		print(s,x+1,y,txt_shade)
	end
	print(s,x,y,col)
end
---------------------------
-- world
function world_init(sx,sy,sw)
	road_ymax=(sw+1)*world_scaley
	for i=1,16 do
		world_factories[i]=nop
	end
	for i=0,sw-1 do
		local noroad=true
		for j=0,7 do
			if sget(sx+i,sy+j)==5 then
				road[i]=(j+1-4)*world_scalex
				noroad=false
				break
			end
		end
		if noroad then
			road_ymax=(i-1)*world_scaley
			break
		end
	end
	-- floor color ramps
	local r={0x33,0x33,0xb3,0x3b}
	for i=0,flr(sw/8+0.5) do
		local c1=sget(sx+8*i,sy)
		if c1!=0 then
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
		world_floor[i+1]=r
	end
end
function world_register(i,fn)
	world_factories[i+1]=fn
end
function road_xoffset(y)
	if(y<0 or y>=road_ymax) return
	local t=min(y/world_scaley,#road-1)
	local i=flr(t)
	-- fractional part
	t-=i
	return lerp(
		road[i],
		road[i+1],
		smoothstep(t))
end
function world_floor_ramp(y)
	y=flr(shr(max(0,y),3)/world_scaley)
	return world_floor[min(y+1,#world_floor)]
end
function world_update()
	-- pick world items
	local plyr_cur=flr((plyr.y+world_far)/world_scaley)
	if world_cur<=plyr_cur then
		local i=0
		for i=world_cur,plyr_cur do
			for j=0,7 do
				world_factories[sget(i,56+j)+1]((j+1-4)*world_scalex,i*world_scaley)
			end
		end
		world_cur=plyr_cur+1
	end
end
---------------------------
-- camera
function cam_init(focal,zfar,beta)
	cam_x=0
	cam_y=0
	cam_z=0
	cam_focal=focal
	cam_zfar=zfar
	cam_rotate(beta or 0)
end
function cam_rotate(beta)
	cam_beta=beta
	cam_cb=cos(cam_beta)
	cam_sb=-sin(cam_beta)
end
function cam_is_top_down()
	return cam_beta>0.6 and cam_beta<0.9
end
function cam_project(pos)
	local y=pos.y-cam_y
	local z=pos.z-cam_z
	local ze=-(y*cam_cb+z*cam_sb)
	-- invalid projection?
	if(ze<cam_zfar or ze>=0) return nil,nil,z,nil
	local w=-cam_focal/ze
	local xe=pos.x-cam_x
	local ye=-y*cam_sb+z*cam_cb
	return hw+xe*w,hh-ye*w,ze,w
end
function cam_track(x,y,z,scale)
	scale=scale or 1
	cam_x=x
	cam_y=y-cam_cb*cam_focal/scale
	cam_z=z-cam_sb*cam_focal/scale
end
--------------------
-- enemies
function nmies_add(e)
	nmies_c+=1
	nmies[nmies_c]=e
end
function nmies_update()
	local n=nmies_c
	nmies_c=0
	for i=1,n do
		local e=nmies[i]
		if e:update() then
			zbuf_write(e,e)
			nmies_c+=1
			if(i!=nmies_c) nmies[nmies_c]=e
		end
	end
end
--------------------
-- tank
function tk_update(self)
	if(self.hit) return false
	if self.y-plyr.y>world_out then
		if self.dly<=time_t and self.y>plyr.y then
			spawn_tracker_blt(self.x,self.y,0.5,1,self.side)
			self.dly=time_t+60+rnd(30)
		end
		return true
	end
	return false
end
function tk_draw(self,x,y,z,w)
	local we,he=tk_w*w,tk_h*w
	sspr(0,8,16,24,x-we/2,y-we/2,we,he)
end
function tk_chase_draw(self,x,y,z,w)
	local we,he=tk_chase_w*w,tk_chase_h*w
	sspr(104,16,16,16,x-we/2,y-we/2,we,he)
end
function tk_blt_collision(self,blt)		
 if circbox_coll(blt,blt_r,self,tk_w,tk_h,2) then
		self.hit=true
		spawn_blast(self.x,self.y,self.z)
		return true
	end
	return false
end

function spawn_tk(x,y)
	local tk={
		x=x,
		y=y,
		z=0,
		dly=60+rnd(60),
		hit=false,
		side=bad_side,
		blt_hit=tk_blt_collision,
		update=tk_update
	}
	if cam_is_top_down() then
		tk.draw=tk_draw
	else
		tk.draw=tk_chase_draw
	end
	nmies_add(tk)
	return tk
end

---------------------
-- special effects
local blast_spr={{88,16},{72,16},{72,0},{88,0}}
function blast_draw(self,x,y,z,w)
	local we=self.width*w
	local t=shr(self.t-time_t,2)
	local s=blast_spr[band(t,3)+1]
	sspr(s[1],s[2],16,16,x-we/2,y-we/2,we,we)
end
function blast_update(self)
	self.x+=self.dx
	self.y+=self.dy
	self.z=max(self.z-self.dz,0)
	--self.dz+=0.05
end
function smoke_draw(self,x,y,z,w)
	local t=shl((self.t-time_t)/self.dly,3)
	local we=flr(t*w)+1
	t=min(flr(8-t),7)
	pal(7,sget(8+t,0))
	sspr(112,56,8,8,x-we/2,y-we/2,we,we,time_t%2==0,time_t%4==0)
	pal(7,7)
end
function spawn_blt_blast(x,y,z,dx,dy,dz)
	spawn_blast(x,y,z,dx,dy,dz,4,59)
end
function spawn_blast(x,y,z,dx,dy,dz,w,snd)
	local fx={x=x,y=y,z=z,
		dx=dx or 0,
		dy=dy or 0,
		dz=dz or 0,
		t=time_t+16,
		width=w or 32,
		update=blast_update,
		draw=blast_draw
	}
	sfx(snd or 62)
	fxs_c+=1
	fxs[fxs_c]=fx
end
function spawn_smoke(x,y,z)
	local dly=15+rnd(10)
	local fx={x=x,y=y,z=z,
		dly=dly,
		t=time_t+dly,
		update=nop,
		draw=smoke_draw
	}
	fxs_c+=1
	fxs[fxs_c]=fx
end

function fxs_update()
	local n=fxs_c
	fxs_c=0
	for i=1,n do
		local fx=fxs[i]
		if fx.t>=time_t then
			fx:update()
			zbuf_write(fx)
			fxs_c+=1
			fxs[fxs_c]=fx
		end
	end
end
---------------------
-- player
function plyr_init()
	sfx(63,1)
	plyr.x=0
	plyr.y=0
	plyr.z=plyr_zmax
	plyr_lives=3
	plyr_score=0
	plyr_hit=false
	plyr_fire_dly=0
	plyr_crashing=false
	plyr_playing=true
	plyr_blt_dy=-3*cos(0.62)
	plyr_blt_dz=-3*sin(0.62)
	futures_add(function()
		plyr_safe=true
		for i=1,180 do yield() end
		plyr_safe=false
	end)
end
function plyr:draw(x,y,z,w)
	if(plyr_crashing) return
	if(plyr_safe and band(time_t,1)==0) return
	local idx=mid(flr(plyr_vx),-1,1)+2
	if cam_is_top_down() then
		spr(plyr_body[idx],x-8,y-9,2,3)
		spr(plyr_rotor[time_t%3+1],x-10,y-11,3,3)
	else
		local pos=plyr_rotor3d[idx][band(time_t,1)+2]
		spr(plyr_rotor3d[idx][1],x-8+pos[1],y+pos[2]-8,2,1,pos[3],pos[4])
		spr(plyr_body3d[idx],x-8,y-8,2,3)
	end
end
function end_game()
		sfx(-1,1) -- stop flip/flop
		sm_push(game_over)
end
function plyr_die_async()
	for i=0,75 do
		local t=i/75
		plyr.z=lerp(plyr_crash_z,0,t)
		plyr_vy=lerp(plyr_vy,0,t)
		if(band(i,7)==0) spawn_blast(plyr.x,plyr.y,plyr.z)
		yield()
	end
	plyr.z=0
	plyr_vy=0
	if plyr_lives<=0 then
		for i=1,30 do yield() end
		end_game()
		return
	end
	plyr_crashing=false
	plyr_playing=true
	for i=1,180 do
		yield()
	end
	plyr_safe=false
end
function plyr_die()
	-- avoid rentrancy
	if(plyr_safe) assert()
	plyr_lives-=1
	plyr_crash_z=plyr.z
	plyr_crashing=true
	plyr_safe=true
	plyr_playing=false
	futures_add(plyr_die_async)
end
function plyr:blt_hit(blt)
	if(plyr_safe) return false
	if circ_coll(self,plyr_r,blt,blt_r) then
		plyr_die()
		return true
	end
	return false
end
function plyr_update()
	plyr_score+=1
	if plyr_playing then
		local dx=0
		if (btn(0)) dx=-0.125
		if (btn(1)) dx=0.125
		if dx==0 then
			plyr_vx*=0.89
			if(abs(plyr_vx)<0.3) plyr_vx=0
		else
			plyr_vx=mid(plyr_vx+dx,-4,4)
		end
		plyr.x=mid(plyr.x+plyr_vx/plyr_scale,-64,64)
		
		if (btn(2)) plyr.z-=0.5
		if (btn(3)) plyr.z+=0.5
		plyr.z=mid(plyr.z,plyr_zmin,plyr_zmax)

		-- !!boss fight:zmax=zmin!!	
		if btn(5) or (plyr_zmin!=plyr_zmax and plyr.z==plyr_zmin) then
			plyr_vy-=0.25
		else
			plyr_vy+=max(0.1, 0.1*plyr_vy)
		end
		plyr_vy=mid(plyr_vy,0,plyr_vmax)
		sfx_speed(63,lerp(8,6,plyr_vy/plyr_vmax))
		plyr.y+=plyr_vy

		-- fire 
		if btn(4) and plyr_fire_dly<=time_t and blts_c<blts_n then
			spawn_blt(plyr.x,plyr.y+8,plyr.z-0.5,0,plyr_blt_dy,plyr_blt_dz,plyr.side)
			plyr_fire_dly=time_t+8
		end
	end
	-- cam world position
	cam_track(
		shr(plyr.x,1),
		plyr.y+plyr_cam_yoffset,
		max(plyr.z,12),
		plyr_scale)
	zbuf_write(plyr,plyr)
end
function plyr_resolve_collisions()
	-- just (re)spawned
	if (plyr_safe) return
	-- against buildings
	if plyr.z<=flr_zmax then
		local b
		for i=1,blds_c do
			b=blds[i]
			if circbox_coll(plyr,plyr_r,b,flr_w,flr_w,flr_zmax) then
				plyr_die()
				b.touch=1
				return
			end
		end
	end
	-- against stuff
	for i=1,zbuf_phys_n do
		local o=zbuf_phys[i]
		if o.plyr_hit and o:plyr_hit(plyr) then
			plyr_die()
			return
		end
	end
end

-------------------------------
-- helo
function helo_update(self)
	if(self.hit) return false
	if (self.dly<=time_t) self.z+=self.dz
	self.y+=self.dy
	if self.y-plyr.y>world_out then
		if self.z>plyr_zmax-8 then
			self.dy=-0.75
			self.dz=0
		end
		return true
	end
	return false
end
function helo_draw(self,x,y,z,w)
	local we,he=helo_w*w,helo_h*w
	sspr(helo_body[band(time_t,1)+1],32,24,32,x-we/2,y-he/2,we,he)
end
function helo_chase_draw(self,x,y,z,w)
	local we,he=helo_w*w,helo_h*w
	sspr(104,0,16,16,x-8*w,y-8*w,16*w,16*w)
 y-=10*w
 w=shl(w,4)
	if time_t%2==0 then
		sspr(72,88,16,8,x-w,y,w,w/2)
	else
		sspr(72,88,16,8,x,y,w,w/2,true)
	end
end
helo_blt_r2=(blt_r+helo_r)*(blt_r+helo_r)
helo_plyr_r2=(plyr_r+helo_r)*(plyr_r+helo_r)
helo_chase_plyr_r2=(plyr_r+8)*(plyr_r+8)
function helo_die(self)
	self.hit=true
	spawn_blast(self.x,self.y,self.z)
end

function spawn_helo(x,y)
	local h={
		x=x,y=y,z=0,
		dy=0,dz=24/30,
		dly=time_t+50,
		die=helo_die,
		side=bad_side,
		hit=false,
		update=helo_update,
		blt_hit=function(self,blt)
			if circ_coll(self,helo_r,blt,blt_r,helo_blt_r2) then
				self:die()
				return true
			end
			return false
		end
	}
	if cam_is_top_down() then
		h.draw=helo_draw
		h.plyr_hit=function(self,p)
			if circ_coll(self,helo_r,plyr,plyr_r,helo_plyr_r2) then
				self:die()
				return true
			end
			return false
		end
	else
		h.draw=helo_chase_draw
		h.plyr_hit=function(self,p)
			if circ_coll(self,8,plyr,plyr_r,helo_plyr_r2) then
				self:die()
				return true
			end
			return false
		end
	end
	nmies_add(h)
	return h
end

-----------------------------
-- building
function flr_draw(self,xe,ye,z,w)
	local we=flr_w*w
	if self.i==0 then
		-- shadow
		local se=1.2*we
		rectfill(xe-se,ye-se,xe+se,ye+se,3)
	end
	local x,y=xe-we,ye-we
	local k=flr(we/4+0.5)
	we*=2
	local c=self.ramp
	if ye>64 then
		rectvstrip(x,y,x+we,y+k,9,c)
	else
		rectvstrip(x,y+we-k,x+we,y+we,9,c)
	end
	if xe<64 then
		recthstrip(x+we-k,y,x+we,y+we,9,c)
	else
		recthstrip(x,y,x+k,y+we,9,c)
	end
end
function roof_draw(self,x,y,z,w,i)
	local we=flr_w*w
	sspr(40,0,32,32,x-we,y-we,2*we,2*we)
end
function flr_chase_draw(self,x,y,z,w)
	local ww=flr_w*w
	local wh=flr_zmax*w
	local k=flr(ww/4+0.5)
	if x<64 then
		recthstrip(x+ww-k,y-wh,x+ww,y,16,self.ramp)
	else
		recthstrip(x-ww,y-wh,x-ww+k,y,16,self.ramp)
	end
end
function roof_chase_draw(self,x,y,z,w,i)
	local ww=flr_w*w
	local wh=flr_zmax*w
 rectvhstrip(x-ww,y-wh,x+ww,y,9,16,self.ramps)
end
function spawn_building(x,y,ramps)
	local b={
		x=x,y=y,z=0,touch=0,
		floors={}
	}
	if cam_is_top_down() then		
		for i=0,flr_n-1 do
			add(b.floors,{x=x,y=y,z=i*flr_h,i=i,ramp=ramps[i%2+1],draw=flr_draw})
		end
		add(b.floors,{x=x,y=y,z=flr_n*flr_h,i=8,ramps=ramps,draw=roof_draw})
	else
		local t,dt=0,1/flr_n
		for i=0,flr_n do
			local dy=lerp(-flr_w,flr_w,t)
			if i<flr_n then
				add(b.floors,{x=x,y=y-dy,z=0,i=i,ramp=ramps[i%2+1],draw=flr_chase_draw})
			else
				add(b.floors,{x=x,y=y-dy,z=0,i=i,ramps=ramps,draw=roof_chase_draw})
			end
			t+=dt
		end
	end
	blds_c+=1
	blds[blds_c]=b
	return b
end

------------------------------
-- bullet
function blt_update(self)
		self.z+=self.dz
		if self.z<0 then
			spawn_blt_blast(self.x,self.y,0,0,0,0,8)
			return false
		elseif self.t>time_t then
			self.x+=self.dx
			self.y+=self.dy
			return true
		end
		return false
end
function blt_draw(self,x,y,z,w)
	local we=flr(self.w*w+0.5)
	sspr(self.sx,self.sy,8,8,x-we,y-we,2*we,2*we)
end
function msl_update(self)
		self.z+=self.dz
		local dt=self.t-time_t
		if dt>0 and self.z>0 then
			if flr(dt)%2==0 and plyr.z>self.z then
				local dx,dy=normz(plyr.x-self.x,plyr.y-self.y)
				self.dx=dx*2
				self.dy=dy*2
			end	
			if(time_t%3==0) spawn_smoke(self.x,self.y,self.z)
			self.x+=self.dx
			self.y+=self.dy
			return true
		end
		spawn_blast(self.x,self.y,self.z,0,0,0,12)
		return false
end
function msl_draw(self,x,y,z,w)
	local we=flr(blt_r*w)+1
	sspr(48,48,8,8,x-we,y-we,2*we,2*we)
end
function spawn_tracker_blt(x,y,z,dz,side)
	local dx,dy=normz(plyr.x-x,plyr.y+32-y)
	dx*=1.5
	dy*=1.5
	return spawn_blt(x,y,z,dx,dy,dz,side)
end
function spawn_blt(x,y,z,dx,dy,dz,side)
	local b={
		x=x,
		y=y,
		z=max(z,0),
		dx=dx,
		dy=dy,
		dz=dz,
		sx=48,
		sy=32,
		w=blt_r,
		t=time_t+90,
		side=side,
		draw=blt_draw,
		update=blt_update
	}
	if(side==good_side) b.sx=120 b.sy=0 b.w=blt_r/plyr_scale
	blts_c+=1
	blts[blts_c]=b
	sfx(61)
	return b
end
function spawn_msl(x,y,z,dz,side)
	local b={
		x=x,
		y=y,
		z=max(z,0),
		dx=0,
		dy=0,
		dz=dz,
		t=time_t+120,
		side=side,
		draw=msl_draw,
		update=msl_update
	}
	blts_c+=1
	blts[blts_c]=b
	sfx(60)
	return b
end
function blts_update()
	local n=blts_c
	blts_c=0
	local b
	for i=1,n do
		b=blts[i]
		if b:update() then
			blts_c+=1
			blts[blts_c]=b
			--insert into zbuffer
			zbuf_write(b)
		end
	end
end
function blts_resolve_collisions()
	for j=1,blts_c do
		local blt=blts[j]
		-- against buildings
		local b
		for i=1,blds_c do
			b=blds[i]
			if circbox_coll(blt,blt_r,b,flr_w,flr_w,flr_zmax) then
				blt.t=0
				b.touch=1
				spawn_blt_blast(blt.x,blt.y,blt.z,0,0,0,4)
				break
			end
		end
		-- against entities
		for i=1,zbuf_phys_n do
			local o=zbuf_phys[i]
			if o.side!=blt.side and o:blt_hit(blt) then
				if(blt.side==good_side) plyr_score+=100
		 	blt.t=0
		 	break
		 end
		end
	end
end

--------------------------
-- boss helpers
function boss_init(x,y,z)
	boss_enabled=true
	boss.x=x
	boss.y=y
	boss.z=z
	boss.side=bad_side
	boss_dy=0
end
function boss_msg_draw(name,rev,y)
	txt_options(true,5)
	txt_print("now entering the enemy's zone.",64,y,7)
	y+=6
	line(6,y,124,y,7)
	y+=4
	txt_print("target code name:",64,y,7)
	y+=18
	local x=24	
	for i=1,#name do
		sprint(sub(name,i,i),x,y,10,2)
		x+=10
	end
	y+=16
	txt_print(rev,64,y,7)
	y+=6
	line(6,y,124,y,7)
	txt_options()
end
function boss_update()
	boss.y+=boss_dy
	zbuf_write(boss)
	for i=1,#boss_parts do
		local t=boss_parts[i]
		t.y+=boss_dy
		if t.hit==false then
			t:update(self)
			zbuf_write(nil,t)
		end
	end
	for i=1,#boss_struct do
		local s=boss_struct[i]
		s.y+=boss_dy
		zbuf_write(s)
	end
end

---------------------------
-- battleship boss (level 1)
function spawn_bship(x,y)
 y+=256
	futures_add(function()
		plyr_playing=false
		local vmax=plyr_vmax
		local z=plyr.z
		for i=0,60 do
			local t=i/60
			plyr_vmax=lerp(vmax,0.5,t)
			plyr.z=lerp(z,32,t)
			yield()
		end
		plyr_playing=true
		plyr_zmax,plyr_zmin=32,32
	end)
	futures_add(function()
		for i=1,180 do
			boss_msg_draw("mermaster","marine fortress ba-001",36)
			yield()
		end
	end,after_draw)

	boss_init(x,y,0)
	boss_dy=0.25
	boss.draw=bship_draw
	-- parts
	-- 1 pixel @z=0 is 4/3 world units
	for j=1,32 do
		local py=-32*(j-15)/3
		for i=0,3 do
			local t=nil
			local ptype=fget(mget(2+i,j))
			local px=32*(i-3)/3
			if ptype==1 then
				t={
					sx=16,
					sy=112,
					update=turret_update,
					dly=(j%8+1)*60
				}
			elseif ptype==2 then
				t={
					sx=48,
					sy=112,
					dly=(j%8+1)*120,
					update=mslturret_update
				}
			end
			if t then
				-- common properties
				t.x=x+px
				t.y=y+py
				t.z=boss.z
				t.i=i-4
				t.j=j-16
				t.hit=false
				t.safe=false
				t.side=bad_side
				t.blt_hit=turret_blt_hit
				t.fire_dly=time_t+t.dly
				add(boss_parts,t)
				-- mirrored turret
				t={
					sx=t.sx,
					sy=t.sy,
					update=t.update,
					dly=t.dly,
					fire_dly=t.fire_dly,
					x=x-px,
					y=y+py,
					z=boss.z,
					i=-i+2,
					j=j-16,
					hit=false,					
					safe=t.safe,
					side=bad_side,
					blt_hit=turret_blt_hit
				}
				add(boss_parts,t)
			end
		end
	end
	-- structures
	local k=1
	for i=1,3 do
		boss_struct[k]={x=x,y=y,z=i*6,s=201,draw=spr_draw}
		boss_struct[k+1]={x=x,y=y-32,z=4*i,s=205,draw=spr_draw}
		k+=2
	end
	boss_struct[k]={x=x,y=y-32,z=24,s=203,draw=spr_draw}
	return boss
end
function spr_draw(self,x,y,z,w)
	spr(self.s,x-8,y-8,2,2)
end
local blast_ramp={2,8,9,10}
function bship_draw(self,x,y,z,w)
	map(2,0,x-32,y-128,4,32)
	map2(2,0,x,y-128,4,32)
	palt(3,false)
	for i=1,#boss_parts do
		local t=boss_parts[i]
		if t.hit then
			local dc=flr(shr(time_t,4))+i
			for c=8,10 do
				pal(c,blast_ramp[(c+dc)%#blast_ramp+1])
			end
		end
		sspr(t.sx,t.sy,16,16,x+t.i*8,y+t.j*8,16,16)
		pal(8,8)
		pal(9,9)
		pal(10,10)
	end
	palt(3,true)
end
function turret_update(self,p)
	if self.fire_dly<time_t and self.y>plyr.y then
		spawn_tracker_blt(self.x,self.y,0.5,1,self.side)
		self.fire_dly=time_t+self.dly
	end
end
local mslturret_frames={0,1,1,2,2,3,3,3,2,1,0}
function mslturret_update(self,p)
	local frame=mslturret_frames[flr(time_t/24)%#mslturret_frames+1]
	self.sx=48+frame*16
	self.safe=frame==0
	if frame==3 and self.fire_dly<time_t and self.y>plyr.y then
		spawn_msl(self.x,self.y,0.5,1,self.side)
		self.fire_dly=time_t+self.dly
	end
end
function turret_blt_hit(self,blt)
	if(self.hit or self.safe) return false
	if circ_coll(self,8,blt,blt_r) then
		self.hit=true
		self.sx=48
		self.sy=96
		spawn_blast(self.x,self.y,0.5)
		spawn_blast(self.x+rnd(8),self.y+rnd(8),0.5)
		spawn_blast(self.x-rnd(8),self.y-rnd(8),0.5)
		return true
	end
	return false
end

-------------------------
-- zbuffer
function zbuf_clear()
	zbuf_n=0
	zbuf_phys_n=0
end
function zbuf_write(obj,phy_obj,i)
	if obj then
		local x,y,z,w=cam_project(obj)
		if w then
			zbuf_n+=1
			zbuf[zbuf_n]={obj,{x,y,z,w}}
		end
	end
	if phy_obj then
		zbuf_phys_n+=1
		zbuf_phys[zbuf_phys_n]=phy_obj
	end
end
function zbuf_sort(a,b)
	return a[2][4]<b[2][4]	
end
function zbuf_draw()
	sort(zbuf,zbuf_n,zbuf_sort)
	for i=1,zbuf_n do
		local o,pos=zbuf[i][1],zbuf[i][2]
		o:draw(pos[1],pos[2],pos[3],pos[4],i)
	end
end

---------------------------
-- game loop
local game_screen={}
function game_screen:update()
	zbuf_clear()
	plyr_update()
	world_update()

	if(boss_enabled) boss_update()

	-- update buildings
	local n=blds_c
	blds_c=0
	for i=1,n do
		local b=blds[i]
		if b.y-plyr.y>-128 then
			b.touch=0
			for f=1,#b.floors do
				zbuf_write(b.floors[f])
			end
			blds_c+=1
			blds[blds_c]=b
		end
	end
	
	nmies_update()
	blts_update()
	fxs_update()
	
	plyr_resolve_collisions()
	blts_resolve_collisions()
end

local road_ramp={13,6}
function draw_floor()
	local p0={x=0,y=floor_ymax+cam_y,z=0}
	local p1={x=0,y=floor_ymin+cam_y,z=0}
	local x0,y0,z0,w0=cam_project(p0)
	local x1,y1,z1,w1=cam_project(p1)
	if w0 and w1 then
		y0=flr(y0)
		y1=flr(y1)
		if y0>1 then -- draw sky
			rectfill(0,0,127,y0-16,12)
			map(16,1,0,y0-16,16,2)
			for i=1,24,2 do		
				spr(176,-plyr.x/8+12*i-32,y0-8,2,1)
			end
			for i=1,3 do
				spr(178,-plyr.x/16+32*i+16,y0-24,3,1)
				spr(178,-plyr.x/8+48*i-32,y0-32-4*(i%2),3,1)
				spr(178,-plyr.x/4+56*i-64,y0-52-4*(i%3),3,1)
			end
		end
		local dy=y1-y0
		local dw=(w1-w0)/dy
		local u,du=p0.y*w0,(p1.y*w1-p0.y*w0)/dy
		local y,ydist,ramp,c
		for i=0,dy do
			if y0>=0 then
				y=u/w0-plyr_vy
				ydist=band(band(0x7fff,y),31)
				ramp=world_floor_ramp(y)
				c=band(band(flr(shr(ydist,4)),31),1)
				memset(0x6000+shl(y0,6),ramp[2*c+band(i,1)+1],64)
				-- road
				local xroad=road_xoffset(y)
				if xroad then
					local we=shl(w0,5)+0.5
					local xe=hw+(xroad-cam_x)*w0
					local v=flr(y)
					pal(5,road_ramp[c+1])
					if we<=8 then
						sspr(16,band(v/2,7)+8,8,1,xe-we/2,y0,we,1)
					else					
						sspr(24,band(v,15),16,1,xe-we/2,y0,we,1)
					end
				end
			end
			u+=du
			w0+=dw
			y0+=1
			if(y0>127) break
		end
	end
	pal(5,5)
end

function draw_spd(x,y)
	rectfill(x,y,x+12,y+4,1)
	rectfill(x+13,y,x+30,y+4,0)	
	for i=0,4 do
		pset(x+13+4*i,y,8)
		pset(x+13+4*i,y+4,8)		
	end
	rectfill(x+13,y+1,x+13+16*plyr_vy,y+3,11)
	
	print("spd",x+1,y,8)
end
function draw_top_banner(x,y)
	txt_options(false,0)
	txt_print("score",x,y,7)
	txt_print(plyr_score,x+25,y,10)
	y+=6
	for i=0,plyr_lives-1 do
		spr(2,x+i*9,y)
	end
end
function draw_shadow()
	if band(time_t,1)==0 then
		local x,y,z,w=cam_project({x=plyr.x,y=plyr.y,z=0})
		if cam_is_top_down() then
			if w then
				local ww=shl(w,3)
				local wh=24*w
				sspr(56,32,8,24,x-shr(ww,1),y-shr(wh,1),ww,wh)
			end
		else
			if w then
				spr(189,x-8,y,1,1)
				spr(189,x,y,1,1,true)
			end
		end
	end
end

function game_screen:draw()
	palt(0,false)
	palt(3,true)
	draw_floor()
	draw_shadow()

	zbuf_draw()
	draw_top_banner(8,2)
	draw_spd(8,120)
	palt()
	
	if plyr_zmin!=plyr_zmax and plyr.z==plyr_zmin and band(time_t,31)>15 then
		txt_options(true,9)
		txt_print("take off",64,100,10)
		txt_options()
	end
	
	--rectfill(0,0,127,7,1)
	--print("\150:"..flr(100*stat(1)+0.5).."% \152:"..flr(stat(0)+0.5).."kb",1,1,7)

	--[[
	txt_options(false,0)
	txt_print("blds :"..blds_c,2,16,7)
	txt_print("nmies:"..nmies_c,2,24,7)
	txt_options()
	--print("blts :"..blts_c,0,18,12)
	]]
end
function cam_rotate_async(from,to,steps)
	for i=from,to,steps do
		local t=i/72
		cam_rotate(lerp(0.75,1,t))
		cam_zfar=lerp(-96-plyr_zmax,-512,t)
		plyr_cam_yoffset=lerp(24,0,t)
		floor_ymax=lerp(128,496,t)
		floor_ymin=lerp(-128,16,t)
		yield()
	end
end
function to_chase()
	futures_add(function()
		nmies_c=0
		plyr_playing=false
		cam_rotate_async(0,72,1)
		world_out,world_far=-32,512
		plyr_blt_dy=-3*cos(0.55)
		plyr_blt_dz=-3*sin(0.55)
		flr_n=4
		flr_zmax=96
		plyr_zmin=3
		plyr_r=8 -- smaller sprite
		plyr_scale=2
		plyr_playing=true
	end)
end
function to_top_down()
	futures_add(function()
		plyr_playing=false
		nmies_c=0
		cam_rotate_async(72,0,-1)
		world_out,world_far=-100,128
		plyr_blt_dy=-3*cos(0.62)
		plyr_blt_dz=-3*sin(0.62)
		flr_n=8
		flr_zmax=flr_n*flr_h
		plyr_zmin=0
		plyr_r=12
		plyr_scale=1
		plyr_playing=true
	end)
end
function game_screen:init()
	time_t=0
	plyr_zmax=flr_zmax+16
	plyr_zmin=0
	cam_init(96,-96-plyr_zmax,0.75)
	-- reset entities
	zbuf_clear()
	nmies_c,blts_c,fxs_c=0,0,0
	floor_ymin,floor_ymax=-120,128
	world_out,world_far=-100,150
	-- reset world factory cursor
	world_cur=0

	-- music
	music(15,0,1)
	plyr_init()
end

-- title_screen
title_screen={}
title_pic=".a163.cecriekbafcr.a13.fcrk.a12.qqkvkvkvkvklnvkd.a24.kv(.a18.ijvkljv(givkn.a12.uukfb.a12.cvkvkkvklvkuvkn.a24.ivkf.a18.fvk)fvkvbvkvb.a11.rskve.a15.rijvknvkvwkvvkb.a22.vkvf.a17.uukvwvkvwvkvg.a10.iymkvs.a15.emgvkvgtzukvwnn.a22.ukvwb.a16.qskvuwkv(wkv(.a11.bllvkc.a14.qiuukv(m]ajv(wvb.a8.qskve.a7.rqkv(gacriecbiukfaukaaakkvskkvk)kvkdqivkvkvaaivmkvkjaivkvkvk.a8.qqkvktvdevkjpvskpvskpvskkvsikvsabskjkvk)askjkvkbev{jkvcaaijvkcjvkklvknuukfjvknuukfrukfbikvsukvc.a8.ckvknwoquknlvkkjvkkjvkkjvkvkvkcquknivkntuknqukfqukvsieaaaevkvkvklnvkvbskjfskvfskvntkvuskvuskvk.a8.iivkvz(bskvnvkvnvkjnvkvnvkvevkvnvkvgvkvnskvnvkvfskvuwvdaaquknsukvwvkvwiecvvkvsukvw)kvk)kvs(kvkb.a9.vkvgldikvwvkvwvkvwvkv(ukv(ukv(ukv(ukvwjkvwtkvwikvwnglaaqskv(wkv(wkv(wkv(ukv(wkv({lvknlvknlvkf.a9.ukv(mnajv(wkv(wkvzwkv(wkvktkvktkvktkvzgjv(gjv(cjv(ovkaaaikvg)kvk)kvkljvk)evk)jvk)evkjnvkjnvkt.a9.ikvjtvbevk)jvg)kvs(kvg)kvsmjvsmjvgnjvg)evgjjvglevkl.a6.jvjkft(mhvknhvzuru(mhv(mtszevtzmsszmc.a9.zmgnwgqszmtsjkhtjkhtjkhtjsftjsftjsftjktszeriekrszmb.a5.umgjvmgt]mgt]mgtgkgtnkgtnkgtekgtecrif.a9.etjsz(akgtnkgj]mgj]mgj]mgjwmgjgkgjwmgjnkgt]crifkgtf.a5.qszevszevtzmwjzezizmwjzmwjzmsizmsmwg.a11.kgjgldizmwjzevtzevtzevtzezizezizezizevjzevjzevizmw.a6.kgtukgtugftugftedftugftugftjcftjkxlqqiec.a6.ieczmnariugriugriecriugriedriedriecriugriugriucrieb.a5.eecriecr(ecr(ecriecr(ecriecriecriaaaeecr.a7.rietvbccr(ecr(ecriecr(ecrmecrmccriccr(ecriecrkccrk.a5.iriecriejtiejtiebriejtiebriebriecriecriec.a6.crqmwgaabtz(ftzkgtz(gtv)gtvk]wf(gtvnglh(]ox(]ovmglb.a5.ntzmgtzkntzkgtzmgtzkgtzmgtvlwoxlvkv)]ofc.a8.btz(aae(]ov(]ov)]ox)]kv)]k)mwiv)]ox)fqzmgtzm]ix)f.a5.u(]ox)]ou(]ou(]ox)]ku)]ox)]kzmgtzmgtzmf.a9.ekvkb.a21.vkv.a9.vkvkvkf.a37.qiecriecri.a312"
scores={
	--name/score/last?
	{"aaa",1000,true},
	{"bbb",900,false},
	{"ccc",800,false},
	{"ddd",600,false},
	{"eee",500,false},
}
ranks={"1st","2nd","3rd","4th","5th"}
game_starting=false
chars="\13\8 abcdefghijklmnopqrstuvwxyz-0123456789\131\132\133\134\135\136\137\138\139\140"
chars_mem={}
rooster_rows={}
function rooster_clear()
	rooster_rows={}
end
function rooster_apply(fn)
	for i=1,#rooster_rows do
		local row=rooster_rows[i]
		if row.dly<time_t then
			for j=1,#row.chars do
				local c=row.chars[j]
				if (c.dly<time_t) fn(c)
			end
		end
	end
end
function rooster_add(s,col)
	local n=#rooster_rows
	local row={
		dly=time_t+n*15,
		chars={}
	}
	local x,z=-48,12-8*n
	local dt=0
	for i=1,#s do
		local c=sub(s,i,i)
		-- no need to display space
		if c!=" " then
			add(row.chars,{
				c=c,
				col=col or 7,
				dly=row.dly+n*60+dt*8,
				src={x=x,y=-64,z=-24},
				dst={x=x,y=0,z=z}
			})
			dt+=1
		end
		x+=8
	end
	add(rooster_rows,row)
end
function char_update(self)
	local t=(time_t-self.dly)/24
	self.cur=lerpn(self.src,self.dst,smoothstep(t))
end
function char_draw(self)
	local x,y,z,w=cam_project(self.cur or self.src)
	if(w) sprint(self.c,x,y,self.col,w)
end

local sprint_lastm=-1
function sprint_init(chars)
	for i=1,#chars do
		local c=sub(chars,i,i)
		cls(0)
		if c=="\13" then spr(86,0,0)
		elseif c=="\8" then spr(127,0,0)
		else print(c,0,0,7) end
		local mem=0x4300+shl(i-1,5)
		for y=0,7 do
			memcpy(mem+4*y,0x6000+shl(y,6),4)
		end
		chars_mem[c]=mem
	end
	cls(0)
end
function sprint(c,x,y,col,size)
	if abs(size-1)<0.01 then
		print(c,x-4,y-4,col)
	else
		local mem=chars_mem[c]
		if mem!=sprint_lastm then
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
	if btnp(4) or btnp(5) then
		game_starting=true
		-- clear last score
		for s in all(scores) do
			s[3]=false
		end
		music(0,250)
		sm_push(game_screen)
	end
	rooster_apply(char_update)
end
function title_screen:draw()	
	local ret,res=coresume(self.str2mem_cor)
	if ret and res then
		res=res-0x6000
		local y=flr(res/64+0.5)
		line(0,y,127,y,7)
	end

	rectfill(0,24,127,127,0)

	print("rank",24,36,5)
	print("score",48,36,5)
	print("name",76,36,5)

	rooster_apply(char_draw)
	
	local s="\151 or \145 to play"
	if time_t%32>16 then
		txt_options(true,9)
		txt_print(s,64,120,10)
	end
end
function title_screen:init()
	time_t=0
	music(0)
	game_starting=false
	cam_init(96,-96)
	cam_track(0,0,0)
	rooster_clear()
	for i=1,#ranks do
		local s=ranks[i].."  "..scores[i][2].."  "..scores[i][1]
		local col=scores[i][3] and 10 or 7
		rooster_add(s,col)
	end	
	self.str2mem_cor=cocreate(function()
		local s=dot2str(title_pic)
		str2mem_async(s,0x6000)
	end)
end
function register_score(name,s)
	local c=1
	for i=1,#ranks do
		if s>=scores[i][2] then
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

game_over={}
go_char_i=1
go_cur_i=1
go_next_i=1
go_moving=false
go_start_a=0
go_char_anim=nil
go_name={4,4,4}
i2c={}
for i=1,#chars do
 local c=sub(chars,i,i)
 i2c[i]=c
end
function get_plyr_name()
	local s=""
	for i=1,#go_name do
		s=s..i2c[go_name[i]]
	end
	return s
end
function game_over_close()
	register_score(get_plyr_name(),plyr_score)
	sm_push(title_screen)
end
function game_over:update()
	if(time_t>900) game_over_close()
	local moved=false
	if btnp(4) or btnp(5) then
		if go_cur_i==1 then
			game_over_close()
		elseif go_cur_i==2 then
			go_char_i-=1
	 else
		local sel=go_char_i
	 	local ci=go_cur_i
	 	futures_add(function()
	 		local c0={x=56*cos(0.75),y=-48*sin(0.75),z=-16}
	 		local c1={x=8*sel-20,y=0,z=18}
	 		local t,dt=0,1/16
	 		for i=0,15 do
				local x,y,z,w=cam_project(lerpn(c0,c1,t))
				go_char_anim={ci=ci,x=x,y=y,z=z,w=w}
				yield()
				t+=dt
			end
			go_char_anim=nil
			go_name[sel]=ci
			go_char_i=min(sel+1,#go_name)
	 	end)
	 end		
	end
	go_char_i=mid(go_char_i,1,#go_name)
	if(btnp(0)) go_next_i-=1 moved=true
	if(btnp(1)) go_next_i+=1 moved=true
	if moved and go_moving==false then
			go_moving=true
			futures_add(function()
				local da=1/#chars
				local t,dt=0,1/16
				for i=0,15 do
					go_start_a=lerp((go_cur_i-1)*da,(go_next_i-1)*da,smoothstep(t))
					t+=dt	
					yield()
				end
				local  i=go_next_i%#chars
				if(i<1) i=#chars-i
				go_cur_i=i
				go_next_i=i
				go_start_a=da*(i-1)
				go_moving=false
			end)
		end
end
function game_over:draw()
	cls(0)
	txt_options(true)
	txt_print("game over",64,12,8)
	txt_print("enter your name",64,24,8)
	txt_options()
	local x=96
	for i=1,#go_name do
		local col=(time_t%32>16 and i==go_char_i) and 10 or 7
		print(i2c[go_name[i]],40+8*i,44,col)
	end
	
	-- carousel
	local da=1/#chars
	local a=-0.25-go_start_a
	for i=1,#chars do
		local c=sub(chars,i,i)
		local x,y,z,w=cam_project({x=56*cos(a),y=-48*sin(a),z=-16})
		local col=i==go_cur_i and 10 or 7
		if(w) sprint(c,x,y,col,w)
		a+=da
	end
	if go_char_anim then
		sprint(i2c[go_char_anim.ci],go_char_anim.x,go_char_anim.y,7,go_char_anim.w)
	end
	print(30-flr(time_t/30),128-8,128-12,1)
end
function game_over:init()
	time_t=0
	go_char_i=1
	go_cur_i=1
	go_next_i=1
	go_moving=false
	go_start_a=0
	go_char_anim=nil
	cam_init(72,-256)
	cam_track(0,0,0)
end

-- game loop
function _update60()
	time_t+=1
	futures_update(before_update)
	sm_update()
end
function _draw()
	sm_draw()
	futures_update(after_draw)
end
function _init()
	cls(0)
	sprint_init(chars)
	world_init(0,56,6*8)
	world_register(4,spawn_tk)
	world_register(6,function(x,y) spawn_building(x,y,flr_ramps[1]) end)
	world_register(7,function(x,y) spawn_building(x,y,flr_ramps[2]) end)
	world_register(13,function(x,y) spawn_building(x,y,flr_ramps[3]) end)
	world_register(8,spawn_helo)
	world_register(9,spawn_bship)
	world_register(10,to_chase)
	world_register(11,to_top_down)
	world_register(15,end_game)
	sm_push(title_screen)
end

__gfx__
888888887a9655503333333305555503305555507777777777777777777777777777777733333333333333333333333333333333333333304333333333333333
8800008800000000377765550555550330555550755555777775555555555577777555573333333333933a333333833338333333333334304373333333333333
808008080000000013331333055555033055555075666655555666666666665555566657339333339993a3333333883333333333333330004463333338833883
8008800800000000151111c305555503305555507566666666666666666666666666665733393989988833333333973383383333333330355353333389a889a8
80088008000000003331111105555503305555507566666666666666666666666666665733389897997733333333397833898333333333066433333389988998
80800808000000003335555305575503305575507566666666666666666666666666657733388877989788333333333333973333333330400943333338833883
88000088000000003333333305575503305575507756666666666666666677777776657793977788799993333333333333383833333334c99793333333333333
8888888800000000333333330557550330557550775666666666666666667557557665773389798979998333333333333333333333330cc99c79333333333333
0040444444490440055105500557550330557550775666666666666666667777777665773339898888978833333833893333333334440cc99cc9999333333333
00404444444904400551055005575503305575507756666666666666666675575576665733389988887989333333383793333333300004c94c99000333333333
0040440000490440057107500557550330557550775666666666666666667777777666573338889888988933333333388833333334030c4999c4349333333333
04004499994400400571075005555503305555507756666666666666666675575576665733339899898933333333333383333333333331c94c13333333333333
04004400004400400571075005555503305555507566677777777777666677777776665733333393338893333333333333333333333303044430333333333333
04004000000400400571075005555503305555507566675555555557666655555556665733933933333339333333333333333333333303333330333303310331
00000090040004400551055005555503305555507566675666666657666655555556665733339333333333333333333333333333333333333333333311111111
00900900004004400551055005555503305555507566675677766657666666666666665733333333333333333333333333333333333333333333333300000000
0440900000000440eeeeeeee00000000000000007566675677777657666666666666665733333333333333333333333333333333333333333333333300000000
0440900000000440cccccccc04444444444444407566675677755657666666666666665733333333333333333333333333333333333333333333333300000000
9440900000000440cccccccc04444444444444407566675677777657666666666666665733333333333333333333333333333333333333304333333300000000
9440090000000440eeeeeeee04444444444444407566675677755657666666666666665733333399993333333333333333333333333333304333333300000000
9444009000004440eeeeeeee04444444444444407566675677766657666666666666657733339779777933333333333773333333333333304333333300000000
9000400000040040cccccccc04444444444444407756675655566657666666666666657733337997777933333333377777733333333300304393333300000000
9999440900444440eeeeeeee044444444444444077566756666666576666666666666577333977777799933333333777777333333334444049c9a33300000000
0000440900440000eeeeeeee044444444444444077566777777777776666666666666577333977777779933333337777777733333344440099999a3300000000
0000440900440000eeeeeeee04444444444444407756655555555555666666666666657733389997779983333333777777773333334440044449993300000000
00000009000000007777777704444444444444407756655555555555666666666666665733389897799883333333377777733333333000000000033300000000
0440440900440440eeeeeeee044444444444444075666666666666666666666666666657333388899988333333333777777333333300000049499a3300000000
0440440900440440777777770444444444444440756666666666666666666666666666573333888888883333333333377333333300404040404049aa00000000
0990440900440990eeeeeeee04444444444444407566666666666666666666666666665733333388883333333333333333333333444494949494949900000000
33333309003333337777777704444444444444407566655555666666666666555556665733333333333333333333333333333333444040404040444400000000
33333309003333337777777704444444444444407555577777555555555555777775555733333333333333333333333333333333000000000000000000000000
33333309003333337777777700000000000000007777777777777777777777777777777733333333333333333333333333333333000333333333300000000000
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
3333330013303333333330301303333333330330103333337770707003000033333330003093333000003333333333333000333333333333354444509aa44445
3333333013333333333333301333333333333330133333337070770003300333333333000094330000003333333333330000333333333333354444009aa44445
3333333013333333333333301333333333333330133333337070770003300333333333300044300003333333003333330000333333333333353333005aa33335
333333301333333333333330133333333333333013333333777070700330033333333333009400033333333300000333040433333333333333333300c5a33333
333333301333333333333330133333333333333013333333000000003330033333333330000000333333333300000000000043333333333333333300c5a33333
333333301333333333333330133333333333333013333333000000003330033333333330006043333333333300000000006000000000003333333300c5a33333
33336330133633333336333013336333333363301336333333387073003003003333333000004333333333333333333000004000000000333333333059333333
3333666066663333333666601666633333336665166633333300003300000000333333000cc0033333333333333333310c04c333000000333333333049333333
3333633113363333333633301333633333336330033633333005600300300300333300000c70003333333333333333310004c333333000333333333049333333
33333311133333333333333013333333333333300033333380577608333003333000000010070003333333333333333010074333333333333333333049333333
33333311133333333333333013333333333333300033333380567608333003333000003301143000333333333333333300043333333333333333333597333333
33333333333333333333333333333333333333333333333330056003333333333300033335533000033333333333333300033333333333333333333597333333
33333333333333333333333333333333333333333333333333000033333333333330333333333300003333333333333300033333333333333333333353333333
33333333333333333333333333333333333333333333333333388333333333333333333333333300033333333333333300033333333333333333333353333333
3b0000060000a00706000000000b000000000000db0d0500cd000000000000003333333333333330333333333333333000033333333333333377333300000000
00606060600000800040808000000006060007000d00500000000000000000003333333333333333333333333333333000033333333333333777773300000000
00000004008555555554040000000000000000004845000000000000000000003333333333333333333333333333333333333333333333337777777300070000
00004440445840800045555500000004600700555554000900000000000f00003333333333333333333333333333333333333333333333333777777700770000
55555555558400060004040055500000004055804840000000000000000000003333333333333333333333333333333333333333333333333777777707777770
000044400000040000000000000555550055800608d0000000000000000000003333333333333333333333333333333333333333333333333377777700770000
0060000400700000000700000000000055400000d000000000000000000000003333333333333333333333333333333333333333333333333377777300070000
00000600600006000600060000000006000606000000000000000000000000003333333333333333333333333333333333333333333333333337773300000000
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
3333333350555555505555554444444444444444444444444444444444444344005050507cc5cc5cc5cc5cc73333666666663333333366666666333344444444
33333366505555555055555544444444444444444444444440444444344003340cccc6ccc7c5cc5cc5cc5c7c3336000000006333333666666666633344444444
333336550555555505555555405677444444444444444444405340404444404407666666cc555555555555cc3336055555506333333666666666633340567744
33336500055555550555555540007644444444444444444444334000800944440577777755555555555555553365666666665633336666666666663340007644
33365055555555555555555540007644666666666666666644445350005a944405666666cc555555555555cc3360000000000633336666666666663340007644
336505555555555555999955440564445555555555555555404885008080443405655666cc555555555555cc3360555555550633336666666666663344056444
36505555555555555596666544005444555555555555555544498000000004440566666655555555555555553365666666665633336666666666663344005444
65055555555555555596555544444444550000000000000553440008a008044405655666cc555555555555cc3360000000000633336666666666663344444444
333650553333333655999955333650555505555555555005534980005300804440000000cc555555555555cc3360555555550633336666666666663333365055
3336505533333336559669653336505555056605d5005005404000a0050000440555555500555555555555003365666666665633336666666666663333365055
3365055533333365559659653365055555055555d555500540400000599050440576076055555555555555553360000000000633336666666666663333650555
3365055533333365559999653365055555056065d50050054444050008a553440555555555555555555555553360555555550633336666666666663333650555
36505555333336505556666536505555550555555555500544434550000033440576076000555555555555003365666666665633336666666666663336505555
36505555333336505555555536505555000000000000000044033409940035040555555555555555555555553336000000006333333666666666633336505555
65055555333365056666666665055555555555555555555544004444444440440576076055505505505505553336055555506333333666666666633365055555
65055555333365050000000065055555666666666666666644444444444444440555555555505505505505553333666666663333333366666666333365055555
33365044555555554444444444444444333650444444444444999999999994444499999999999444449999999999944444999999999994440556666600000000
3336504455555555444433333333444433365044444444444a00000000000a444a00000000000a444a55555555555a444a55555555555a440560000000000000
33365044555555554443030330303444333650444444444490555555555550949055555555555094955666555555559495555555555555940560555500000000
33365044555555554433030330303344333650444444444495555555555555949555555555555594966000666555669495053030303050940656666600000000
33365044555555554433333333333344333650666666666695555555555555949556665555555594900030000666009490503030303505940600000000000000
33365044555555554433333673333344333650555555555595555555555555949660006665556694900033333000009490003333333000940605555500000000
33365044555555554033b060070b3304333650555555555595566655555555949000bb00066600949000bbbbbbb0009490007777777000940656666600000000
333650445555555540bb00600600bb0433365055550777559660006665556694900030030000009490003003003000949000b05b05b000940600000000000000
4444444455555555407700566500770433336505550677559005550006660094900030030030009490003003003000949000b00b00b000940605555500000000
444444445555555540bb06555570bb0433336505550667559555555550005594900000333330009490003333333000949000bbbbbbb000940656666600000000
444444445555555540bb06555560bb0433336505555065559555555555555594900555000030009490003003003000949000b05b05b000940600000000000000
444444445555555540bb05655650bb0433333650555065559555555555555594955555555000559490000003003000949000b00b00b000940605555500000000
444444445555555540bb00566500bb0433333650555065559555555555555594955555555555559490055500003000949000bbbbbbb000940656666600000000
44444444555555554000000550000004333333650000000095555555555555949555555555555594955555555000559490000000000000944060000000000000
4444444466666666440000000000004433333336555555554a55555555555a444a55555555555a444a55555555555a444a00000000000a444060555500000000
44444444000000004444444444444444333333336666666644999999999994444499999999999444449999999999944444999999999994444406666600000000
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
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000200000000000000000000000000000000000000000000000000
__map__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000c00000000000000000000022222222222222222222222222222222000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000d1c20000000000000000000032323232323232323232323232323232000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000d3d20000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000d1c1e10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000d0e1e10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000d1c1f1f10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0e2e3f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f2f3f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0c3c30000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0e10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0e10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0e2e3e10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f2f3e10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0c3f0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0e2e3f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f2f3f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0e6e7f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f6f7d80000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0ee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0e6e7fe0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f6f7f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0c3e2e30000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0c3f2f30000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0e2e3f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f2f3f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f0f0f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0e6e7f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e0f6f7f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000e4e5c4c50000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000f4f5d4d50000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000400003062500000000000000000000000000000030625000000000000000306250000000000306250000000000000000835008355000000000000000000000000030625000000000000000000000000000000
010400000135001350013500135001350013500135501350013500135501350306250000000000063500635006355083503062500000000000135001350013550000001350013500135505350053500535500000
010400000000000000000000000000000000000000000000000000000000000013500135500000306250000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010400003062500000000000000006350063550000000000000000000000000306250000000000000000000000000000003062500000000000000000000000000000000000013500135001350013500135001355
010400001135011350113550635030625000000000012350123501235500000013500135001350013500135001350013550b3500b3500b3550d3500d3500d3550000001350306250000000000000000000000000
010400003062500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010400000000001350013550000030625000000000000000306250000000000000000135001355000000000000000000000000001350013550000000000000000000000000306250000000000000000135001350
010400000135030625000000000001350013500135001350306250000000000013503062500000000000d3500d3500d3550135030625000000000001350013500135001350306250000000000013503062500000
010400000000000000000000000030625000000000000000013500135001355000000000000000000000000000000000000000000000000000000000000000000000000000013500135001355000000000000000
010400000135001350013500135500000063500635500000000000000000000000000a3500a355000000000000000000000000030625000000000000000000000000000000306250000000000000003062500000
0104000000000000000000000000063503062500000000000835008350083550a3503062500000000000b3500b3500b3550000001350013500135001350013500135001355013500135001355013503062500000
010400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000135001355
010400000000000000306250000000000306250000000000000000000000000000003062500000000000000000000000000000000000306250000000000306250000000000000000000000000000003062500000
010400000000006350306250000000000083500835008355013500135001355000000135001350013550535005350053550000011350306250000000000063500635006355123501235012355000000135001350
010400000000000000063500635500000000000000000000000000000000000000000000000000000000000000000000000000000000113501135500000000000000000000000000000000000000000000000000
010400000000000000000000000000000000000b3500b355000000000000000000000000001350013500135001350013500135500000013500135500000000003062500000000003062500000000000000001350
0104000001350013500135001350013550b3503062500000000000d3500d3500d3550135030625000000000000000000000000001350306250000000000013503062500000000003062500000000000135030625
010400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000135001350013500135001350013550000000000
010400000135500000000000000000000000000135001355000000000000000000000000030625000000000000000013500135001350013500135001355000000635006355000000000000000000000000030625
0104000000000000000d3500d3500d35501350306250000000000013500135001350013503062500000000000135030625000000000000000000000000006350306250000000000083500835008355000000a350
010400000000000000000000000000000000000000000000000000000000000000000000001350013500135500000000000000000000000000000000000000000000000000000000000000000000000000000000
010400000000000000000000000000000000003062500000000000000000000000000000000000033500335500000306250000000000000003062500000000003062500000000000000000000000000000000000
010400000a3500a3550b3500b3500b3550000003350033500335003350033500335003355033503062500000000000335003350033550f3503062500000000000335003350033500335003350033500335503350
010400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000306250000000000000000f3500f355000000000000000000000000000000000000000000000
010400000335003350033500335003350033550000030625000000000030625000000000000000000000000000000000000f3500f3500f3500f3500f3500f3550000003350033500335003350033500335500000
0104000030625000000000000000000000000003350306250000000000033500335003350033500335003350033550f3503062500000000000000000000000000335030625000000000000000000000000001350
010400000000000000000000000000000000000000003350033550000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010400000135001350013500135001350013550000030625000000000000000306250000000000306250000000000000000135001350013500135001350013550000030625000000000000000000000000000000
0104000030625000000000000000000000000000000013500135001355013503062500000000000d3500d3500d355013503062500000000000000000000000000000001350013500135001350013500135001355
010400003062500000000000000001350013500135001350013500135500000306250000000000000000000000000000003062500000000000000000000000000000000000043500435004350043500435004355
0104000001350013500135501350306250000000000000000000000000000000d3500d3500d3500d3500d3500d3500d3550135001350013500135001350013500135504350306250000000000000000000000000
010400000000004350043550000030625000000000000000306250000000000000000435004350043500435004350043550000004350043500435004350043500435500000306250000000000000000435004350
010400000435030625000000000004350043500435510350306250000000000043503062500000000000000000000000000435030625000000000000000000000000004350306250000000000043503062500000
010400000000000000000000000030625000000000000000103501035500000000000000000000000000000000000000000000000000000000000000000000000000000000043500435500000000000000000000
010400000435004350043500435500000103501035010350103501035010355000000435004350043500435004350043550000030625000000000000000000000000000000306250000000000000003062500000
010400000000000000000000000010350306250000000000000000000000000043503062500000000000000000000000000000003350033500335003350033500335003355033500335003355033503062500000
010400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000335003355
01040000000000f350306250000000000033500335003350033500335003350033550335003350033500335003350033500335503350306250000000000033500335003350033500335003350033550f3500f350
0104000000000000000f3500f35500000000000000000000000000000000000000000000000000000000000000000000000000000000033500335500000000000000000000000000000000000000000000000000
0104000000000000000f3500f3500f355000000335003350033500335003350033550000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010400000f3500f350306250000000000033503062500000000003062500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010300000315003150031500315003150031500315003150031500315500000031500315003150031550000003150031500315003150031500315003150031500315003155000000315003150031500315500000
010300000d1500d1500d1500d1500d155000000f1500f1500f1500f1500f155031500315003155000000000003150031500315003155000000000003150031500315003150031500315003150031500315003150
010300000315503150031500315500000000000d1500d1500d1500d1500d1550f1500f1500f1500f1500f15500000031500315003150031500315500000031500315003150031500315003150031500315003150
0103000003155031500315003150031550000000000031500315003150031500315003150031500315003150031550315003150031500315500000000000d1500d1500d1500d1500d1550f1500f1500f1500f150
010300000f15500000001500015000150001500015000150001500015000150001550c1500c1500c1500c1500c15500000001500015000150001500015501150011500115001155000000d1500d1500d1500d150
010300000d1550000002150021500215002150021550e1500e1500e1500e1500e1550000003150031500315003150031500315003150031500315003155031500315003150031550000000000031500315003150
01030000031500315003150031500315003150031550315003150031500315500000000000d1500d1500d1500d1500d155000000f1500f1500f1500f1500f1550315003150031550000000000031500315003150
01030000031550000000000031500315003150031500315003150031500315003150031500315503150031500315500000000000d1500d1500d1500d1500d1550f1500f1500f1500f1500f155000000315003150
010300000315003150031550315003150031500315003150031500315003150031500315500000031500315003150031550000003150031500315003150031500315003150031500315003155000000315003150
010300000315003155000000d1500d1500d1500d1500d155000000f1500f1500f1500f1500f15500000001500015000150001500015000150001500015000150001550c1500c1500c1500c1500c1550000000150
010300000015000150001500015501150011500115001155000000d1500d1500d1500d1500d1550000002150021500215002150021550e1500e1500e1500e1500e15500000031500315003150031500315003150
0103000003150031500315003155031500315003150031550000000000031500315003150031500315003150031500315003150031550315003150031500315500000000000d1500d1500d1500d1500d1550f150
010300000f1500f1500f1500f1550000003150031500315500000000000315003150031500315500000031500315003150031500315003150031500315003150031500315500000031500315003155000000d150
010300000d1500d1500d1500d155000000f1500f1500f1500f1500f15500000031500315003150031500315503150031500315003150031500315003150031500315003155000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200000563004640056200262000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00100000296201964019650196601965019640196300d630016200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000100002e6502e6502d6502d6502c6502a650286502665022640206301d62019620146200f6100a6100861006610056100461003610020100201002610020100201002610020100101002010016100161000000
000700000467002660046500264004630036200462004610056100461000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0006000824610026502461003650256100165025610016502b6000160026600056001d60006600306000a6001c600000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
01 00010240
00 03040540
00 06070840
00 090a0b40
00 0c0d0e40
00 0f101140
00 12131440
00 15161740
00 18191a40
00 1b1c0240
00 1d1e0540
00 1f202140
00 22232440
00 0c252640
02 27284040
01 29424344
00 2a424344
00 2b424344
00 2c424344
00 2d424344
00 2e424344
00 2f424344
00 30424344
00 31424344
00 32424344
00 33424344
00 34424344
00 35424344
02 36424344
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

