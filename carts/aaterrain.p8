pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- globals
local time_t=0
local actors,ground_actors,parts,light,cam,plyr,active_ground_actors={},{},{},{0,1,0}

-- world units
local ground_scale,hscale=16,4

local good_side,bad_side,any_side,no_side=0x1,0x2,0x0,0x3

-- register json context here
local _tok={
 ['true']=true,
 ['false']=false}
function nop() return true end
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
  if(err_if_missing) assert'delimiter missing'
  return pos,false
 end
 return pos+1,true
end
local function parse_str_val(str, pos, val)
	val=val or ''
	if pos>#str then
		assert'end of input found while parsing string.'
	end
	local c=sub(str,pos,pos)
	if(c=='"') return _g[val] or val,pos+1
	return parse_str_val(str,pos+1,val..c)
end
local function parse_num_val(str,pos,val)
	val=val or ''
	if pos>#str then
		assert'end of input found while parsing string.'
	end
	local c=sub(str,pos,pos)
	-- support base 10, 16 and 2 numbers
	if(not match(c,"-xb0123456789abcdef.")) return tonum(val),pos
	return parse_num_val(str,pos+1,val..c)
end
-- public values and functions.

function json_parse(str, pos, end_delim)
	pos=pos or 1
	if(pos>#str) assert'reached unexpected end of input.'
	local first=sub(str,pos,pos)
	if match(first,"{[") then
		local obj,key,delim_found={},true,true
		pos+=1
		while true do
			key,pos=json_parse(str, pos, table_delims[first])
			if(key==nil) return obj,pos
			if not delim_found then assert'comma missing between table items.' end
			if first=="{" then
				pos=skip_delim(str,pos,':',true)  -- true -> error if missing.
				obj[key],pos=json_parse(str,pos)
			else
				add(obj,key)
			end
			pos,delim_found=skip_delim(str, pos, ',')
	end
	elseif first=='"' then
		-- parse a string (or a reference to a global object)
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
		assert'invalid json token'
	end
end

-- screen space effects
local shkx,shky=0,0
function screen_shake(pow)
	shkx,shky=min(4,shkx+rnd(pow)),min(4,shky+rnd(pow))
end
function screen_update()
	shkx*=-0.7-rnd(0.2)
	shky*=-0.7-rnd(0.2)
	if abs(shkx)<0.5 and abs(shky)<0.5 then
		shkx,shky=0,0
	end
	camera(shkx,shky)
end

function sprr(sx,sy,x,y,a,w,scale)
	scale=scale or 1
	local ca,sa=cos(a),sin(a)
 local srcx,srcy,addr,pixel_pair
 local ddx0,ddy0=ca/scale,sa/scale
 local mask=shl(0xfff8,(w-1))
 w*=4	
 ca*=w-0.5
 sa*=w-0.5 
 local dx0,dy0=sa-ca+w,-ca-sa+w
 w=scale*(2*w-1)
 for ix=x,x+w do
  srcx,srcy=dx0,dy0
  for iy=y,y+w do
   local c
   if band(bor(srcx,srcy),mask)==0 then
   	c=sget(sx+srcx,sy+srcy)
   	sset(ix,iy,c)
  	end
   srcx-=ddy0
  	srcy+=ddx0
  end
  dx0+=ddx0
  dy0+=ddy0
 end
end

-- zbuffer (kind of)
local drawables,zbuf
function zbuf_clear()
	drawables={}
end
function zbuf_sort()
 zbuf={}
	local ci,cj=flr(cam.lookat[1]/ground_scale),flr(cam.lookat[3]/ground_scale)
	for _,d in pairs(drawables) do
		-- find cell location
		local di,dj=flr(shr(d.pos[1],4)),flr(shr(d.pos[3],4))
		if abs(di-ci)<6 and abs(dj-cj)<6 then
			-- safe index
			dj=band(dj,0x7f)			
			zbuf[dj]=zbuf[dj] or {}
			add(zbuf[dj],{obj=d,key=d.pos[3]})
		end
	end
	-- sort each bucket
	for _,b in pairs(zbuf) do
		sort(b)
	end
end

function zbuf_filter(array)
	for _,a in pairs(array) do
		if not a:update() then
			del(array,a)
		else
			add(drawables,a)
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

function lerp(a,b,t)
	return a*(1-t)+b*t
end
function lerparray(a,t)
	return a[mid(flr((#a-1)*t+0.5),1,#a)]
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

-- https://github.com/morgan3d/misc/tree/master/p8sort
function sort(data)
 for num_sorted=1,#data-1 do 
  local new_val=data[num_sorted+1]
  local new_val_key=new_val.key
  local i=num_sorted+1

  while i>1 and new_val_key>data[i-1].key do
   data[i]=data[i-1]   
   i-=1
  end
  data[i]=new_val
 end
end

function make_v_cross(a,b)
	local ax,ay,az=a[1],a[2],a[3]
	local bx,by,bz=b[1],b[2],b[3]
	return {ay*bz-az*by,az*bx-ax*bz,ax*by-ay*bx}
end
-- world axis
local v_fwd,v_right,v_up={0,0,1},{1,0,0},{0,1,0}

function make_v(a,b)
	return {
		b[1]-a[1],
		b[2]-a[2],
		b[3]-a[3]}
end
function v_clone(v)
	return {v[1],v[2],v[3]}
end
function v_lerp(a,b,t)
	return {
		lerp(a[1],b[1],t),
		lerp(a[2],b[2],t),
		lerp(a[3],b[3],t)}
end
function v_dot(a,b)
	return a[1]*b[1]+a[2]*b[2]+a[3]*b[3]
end
function v_normz(v)
	local d=v_dot(v,v)
	if d>0.001 then
		d=sqrt(d)
		v[1]/=d
		v[2]/=d
		v[3]/=d
	end
	return d
end
function v_clamp(v,l)
	local d=v_dot(v,v)
	if d>l*l then
		v_scale(v,l/sqrt(d))
	end
end
function v_scale(v,scale)
	v[1]*=scale
	v[2]*=scale
	v[3]*=scale
end
function v_add(v,dv,scale)
	scale=scale or 1
	v[1]+=scale*dv[1]
	v[2]+=scale*dv[2]
	v[3]+=scale*dv[3]
end
function in_cone(p,t,fwd,angle,rng)
	local v=make_v(p,t)
	-- close enough?
	if sqr_dist(v,v)<rng*rng then
		v_normz(v)
		-- in cone?
		return v_dot(fwd,v)>angle
	end
	return false
end

-- matrix functions
function m_x_v(m,v)
	local x,y,z=v[1],v[2],v[3]
	v[1],v[2],v[3]=m[1]*x+m[5]*y+m[9]*z+m[13],m[2]*x+m[6]*y+m[10]*z+m[14],m[3]*x+m[7]*y+m[11]*z+m[15]
end
-- 3x3 matrix mul (orientation only)
function o_x_v(m,v)
	local x,y,z=v[1],v[2],v[3]
	v[1],v[2],v[3]=m[1]*x+m[5]*y+m[9]*z,m[2]*x+m[6]*y+m[10]*z,m[3]*x+m[7]*y+m[11]*z
end
function m_x_xyz(m,x,y,z)
	return {
		m[1]*x+m[5]*y+m[9]*z+m[13],
		m[2]*x+m[6]*y+m[10]*z+m[14],
		m[3]*x+m[7]*y+m[11]*z+m[15]}
end
function make_m(x,y,z)
	local m={}
	for i=1,16 do
		m[i]=0
	end
	m[1],m[6],m[11],m[16]=1,1,1,1
	m[13],m[14],m[15]=x or 0,y or 0,z or 0
	return m
end

function make_m_toward(z,up)
 	local x=make_v_cross(up,z)
	-- aligned?
	if v_dot(x,x)<0.001 then
		-- up and z //
		if abs(up[3])==1 then
			z[1]+=0.01
		else
			z[3]+=0.01
		end
		v_normz(z)
		x=make_v_cross(up,z)
	end
		
	v_normz(x)
	local y=make_v_cross(z,x)
	v_normz(y)
 
	return { 
		x[1],x[2],x[3],0,
		y[1],y[2],y[3],0,
  		z[1],z[2],z[3],0,
		0,0,0,1}
end
-- quaternion
function make_q(v,angle)
	angle/=2
	-- fix pico sin
	local s=-sin(angle)
	return {v[1]*s,
	        v[2]*s,
	        v[3]*s,
	        cos(angle)}
end
function q_clone(q)
	return {q[1],q[2],q[3],q[4]}
end
function q_normz(q)
	local d=v_dot(q,q)+q[4]*q[4]
	if d>0 then
		d=sqrt(d)
		q[1]/=d
		q[2]/=d
		q[3]/=d
		q[4]/=d
	end
end

function q_x_q(a,b)
	local qax,qay,qaz,qaw=a[1],a[2],a[3],a[4]
	local qbx,qby,qbz,qbw=b[1],b[2],b[3],b[4]
        
	a[1]=qax*qbw+qaw*qbx+qay*qbz-qaz*qby
	a[2]=qay*qbw+qaw*qby+qaz*qbx-qax*qbz
	a[3]=qaz*qbw+qaw*qbz+qax*qby-qay*qbx
	a[4]=qaw*qbw-qax*qbx-qay*qby-qaz*qbz
end
function m_from_q(q)
	local x,y,z,w=q[1],q[2],q[3],q[4]
	local x2,y2,z2=x+x,y+y,z+z
	local xx,xy,xz=x*x2,x*y2,x*z2
	local yy,yz,zz=y*y2,y*z2,z*z2
	local wx,wy,wz=w*x2,w*y2,w*z2

	return {
		1-(yy+zz),xy+wz,xz-wy,0,
		xy-wz,1-(xx+zz),yz+wx,0,
		xz+wy,yz-wx,1-(xx+yy),0,
		0,0,0,1
	}
end

-- only invert 3x3 part
function m_inv(m)
	m[2],m[5]=m[5],m[2]
	m[3],m[9]=m[9],m[3]
	m[7],m[10]=m[10],m[7]
end
-- inline matrix invert
-- inc. position
function m_inv_x_v(m,v,p)
	p=p or 1
	local x,y,z=v[1]-p*m[13],v[2]-p*m[14],v[3]-p*m[15]
	v[1],v[2],v[3]=m[1]*x+m[2]*y+m[3]*z,m[5]*x+m[6]*y+m[7]*z,m[9]*x+m[10]*y+m[11]*z
end
function m_set_pos(m,v)
	m[13],m[14],m[15]=v[1],v[2],v[3]
end
-- returns foward vector from matrix
function m_fwd(m)
	return {m[9],m[10],m[11]}
end
-- returns up vector from matrix
function m_up(m)
	return {m[5],m[6],m[7]}
end

-- models & rendering
local all_models=json_parse'{"vship":{"c":3},"tree":{"c":3,"r":4},"a10":{"gauss_wp":{"pos":[0,0,-3],"part":"gauss_blt","dly":8,"dmg":1}}}'
local dither_pat=json_parse'[0b1111111111111111,0b0111111111111111,0b0111111111011111,0b0101111111011111,0b0101111101011111,0b0101101101011111,0b0101101101011110,0b0101101001011110,0b0101101001011010,0b0001101001011010,0b0001101001001010,0b0000101001001010,0b0000101000001010,0b0000001000001010,0b0000001000001000,0b0000000000000000]'
local color_lo={1,1,13,6,7}
local color_hi={0x11,0xd0,0x60,0x70,0x70}
function draw_model(model,m)

	-- cam pos in object space
	local cam_pos=v_clone(cam.pos)
	m_inv_x_v(m,cam_pos)
	-- light dir in object space
	local l=v_clone(light)
	m_inv_x_v(m,l,0)
	
	-- faces
	local faces,p={},{}
	for i=1,#model.f do
		local f,n=model.f[i],model.n[i]
		-- viz calculation
		local d=n[1]*cam_pos[1]+n[2]*cam_pos[2]+n[3]*cam_pos[3]
		if true then --if d>=model.cp[i] then
			-- project vertices
			for _,vi in pairs(f.vi) do
				if not p[vi] then
					local v=model.v[vi]
					local x,y,z=v[1],v[2],v[3]
					x,y,z,w=cam:project(m[1]*x+m[5]*y+m[9]*z+m[13],m[2]*x+m[6]*y+m[10]*z+m[14],m[3]*x+m[7]*y+m[11]*z+m[15])
					p[vi]={x,y,z,w}
				end
			end
			-- distance to camera (in object space)
			local d=sqr_dist(f.center,cam_pos)

			-- register faces
			add(faces,{key=d,face=f})
		end
	end
	-- sort faces
	sort(faces)

	-- draw faces using projected points
	for _,f in pairs(faces) do
		f=f.face
		local c=max(v_dot(model.n[f.ni],l))*5
  c=sget(c+8,sget(f.material,8))
		local p0=p[f.vi[1]]
	 	for i=2,#f.vi-1 do
		 	local p1,p2=p[f.vi[i]],p[f.vi[i+1]]
		 	trifill(p0[1],p0[2],p1[1],p1[2],p2[1],p2[2],c)
		end
	end
	fillp()
end

_g.update_plyr=function(self)

 -- auto-stabilize when firing
 if not plyr.firing then
  -- damping
 	self.roll*=0.8
 	self.turn*=0.9
 	self.pitch*=0.91
	end
	self.turn_spring*=0.95
	
	return true
end

_g.die_actor=function(self)
	make_blast(self.pos)
	self.disabled=true
	del(actors,self)
end

_g.hit_npc=function(self,dmg)
	-- avoid reentrancy
	if(self.disabled) return
	self.hp-=dmg
	if self.hp<=0 then
		self:die()
	end
end

local all_actors=json_parse'{"plyr":{"model":"a10","fire_t":0,"pitch":0,"roll":0,"turn":0,"turn_spring":0,"update":"update_plyr","angle":0.25,"smoke_dly":12,"smoke_t":0,"acc":0.8},"tree":{"model":"tree","hp":4,"update":"nop","hit":"hit_npc","die":"die_actor","rnd":{"angle":[0,1]}}}'

-- maths
function sqr_dist(a,b)
	local dx,dy,dz=b[1]-a[1],b[2]-a[2],b[3]-a[3]

	dx=dx*dx+dy*dy+dz*dz
	return dx<0 and 32000 or dx
end

function make_v(a,b)
	return {
		b[1]-a[1],
		b[2]-a[2],
		b[3]-a[3]}
end
function lerp(a,b,t)
	return a*(1-t)+b*t
end
function v_clone(v)
	return {v[1],v[2],v[3]}
end
function v_lerp(a,b,t)
	return {
		lerp(a[1],b[1],t),
		lerp(a[2],b[2],t),
		lerp(a[3],b[3],t)}
end
function v_dot(a,b)
	return a[1]*b[1]+a[2]*b[2]+a[3]*b[3]
end
function v_normz(v)
	local d=v_dot(v,v)
	if d>0.001 then
		d=sqrt(d)
		v[1]/=d
		v[2]/=d
		v[3]/=d
	end
	return d
end
function v_clamp(v,l)
	local d=v_dot(v,v)
	if d>l*l then
		v_scale(v,l/sqrt(d))
	end
end
function v_scale(v,scale)
	v[1]*=scale
	v[2]*=scale
	v[3]*=scale
end
function v_add(v,dv,scale)
	scale=scale or 1
	v[1]+=scale*dv[1]
	v[2]+=scale*dv[2]
	v[3]+=scale*dv[3]
end
function in_cone(p,t,fwd,angle,rng)
	local v=make_v(p,t)
	-- close enough?
	if sqr_dist(v,v)<rng*rng then
		v_normz(v)
		-- in cone?
		return v_dot(fwd,v)>angle
	end
	return false
end

function draw_actor(self,x,y,z,w)
	draw_model(self.model,self.m)
end

function make_actor(src,p)
	-- instance
	local a=clone(all_actors[src],{
		pos=v_clone(p),
		q=q or make_q(v_up,0)
	})
	a.model,a.draw=all_models[a.model],a.draw or draw_actor
	-- init orientation
	local m=m_from_q(a.q)
	m_set_pos(m,p)
	a.m=m
	return add(actors,a)
end

function make_ground_actor(src,i,j)
	local x,z=i*ground_scale,j*ground_scale
	local a=clone(all_actors[src],{
		pos={x,2*get_altitude(x,z),z},
		draw=draw_actor
	})
	a.model=all_models[a.model]
	-- any angle defined in instance?
	local q=make_q(v_up,a.angle or 0)
	local m=m_from_q(q)
	m_set_pos(m,a.pos)
	a.m=m
	-- register
	ground_actors[i+j*128]=a
	return a
end

-- camera
function make_cam(f)
	local c={
		pos={0,6*hscale,0},
		lookat={0,0,-7*ground_scale},
		offset={0,0,6*ground_scale},
		focal=f,
		track=function(self,pos)
			self.pos=v_clone(pos)
			self.lookat=v_clone(pos)
			v_add(self.pos,self.offset)
		end,
		project=function(self,x,y,z)
		 -- todo: make project_v
		 local v={x,y,z}
		 v_add(v,self.lookat,-1)
   local angle=max(-plyr.pitch/64)
	  local c,s=cos(angle),-sin(angle)
			z,y=c*v[3]+s*v[2],-s*v[3]+c*v[2]
			y+=self.lookat[2]  	
			z+=self.lookat[3]  	

  	local xe,ye,ze=x-self.pos[1],y-self.pos[2],z-self.pos[3]

  	local w=-self.focal/ze
  	return 64+xe*w,64-ye*w,ze,w
		end
	}
	return c
end

-- particles & bullets
_g.update_part=function(self)
	if(self.t<time_t or self.r<0) return false
	local p=self.pos
	v_add(p,self.v)
	-- gravity
	v_add(p,{0,-1,0})
	-- ground collision
	local h=get_altitude(p[1],p[3])
	if p[2]<h then
		p[2]=h
		-- todo: proper reflection vector
		v_scale(self.v,0.9)
	end
	
	-- force damping
	v_scale(self.v,self.dv)
	
	self.r+=self.dr
	-- animation frame
	self.frame+=self.df
	
	return true
end

_g.draw_part=function(self)
	local x,y,z,w=cam:project(self.pos[1],self.pos[2],self.pos[3])
	-- behind camera
	if(z>=0) return
	
	-- simple part
	if self.kind==0 then
		-- smoke
		fillp(lerparray(dither_pat,1-self.frame)+0.1)
		circfill(x,y,self.r*w,self.c)
		fillp()
	elseif self.kind==1 then
	 -- bullet
		local v=v_clone(self.pos)
		v_add(v,self.u,self.acc)
		local x1,y1,z1,w1=cam:project(v[1],v[2],v[3])
		if(z1<0) line(x,y,x1,y1,self.c)
	elseif self.kind==2 then
	 -- flash
		circfill(x,y,self.r*w,self.c)
	elseif self.kind==4 then
	 -- blast (sprite)
		local s=flr(6*self.frame)
		local c0,c1=sget(s,2),sget(max(s-1),2)
		fillp(lerparray(dither_pat,1-self.frame))
		circfill(x,y,w*self.r,bor(c1*16,c0))
		fillp()
		--pal(7,sget(s,2))
		--local r=16*w
		--sspr(16*min(s,3)+48,0,16,16,x-r,y-r,r,r)
		--pal()
	end
end

_g.die_blt=function(self)
	-- make_part(self.die_part or "flash",self.pos,v_up)
	make_blast(self.pos)
	
	-- to be removed from set
	return false
end

function blt_obj_col(self,objs)
	for _,a in pairs(objs) do
		local r=a.model and a.model.r
		if r and band(a.side,self.side)==0 then
			r*=r
			local hit=false
			-- edge case: base or tip inside sphere
			if sqr_dist(self.pos,a.pos)<r or sqr_dist(self.prev_pos,a.pos)<r then
				hit=true
			else
				-- point to sphere
				local ps=make_v(self.pos,a.pos)
				-- projection on ray
				local t=v_dot(self.u,ps)
				if t>=0 and t<=self.acc then
					-- distance to sphere?
					local p=v_clone(self.u)
					v_scale(p,t)
					hit=sqr_dist(p,a.pos)<r
				end	
			end
			if hit then
				make_part("flash",self.pos,v_up)
				a:hit(self.dmg,self.actor)
				return true
			end	
		end
	end
	return false
end

_g.update_blt=function(self)
	if(self.t<time_t) return false
	
	-- ground?
	local h=get_altitude(self.pos[1],self.pos[3]) 
 if self.pos[2]<=h then
		return self:die()
	end

	self.prev_pos=v_clone(self.pos)
	v_add(self.pos,self.u,self.acc)

	-- collision?
	if blt_obj_col(self,actors) or blt_obj_col(self,active_ground_actors) then
		return self:die()
	end
	
	return true
end

all_parts=json_parse'{"smoke":{"rnd":{"r":[2,3],"c":[5,6,7],"dly":[24,32]},"frame":0,"dv":0.9,"dr":-0.05,"kind":0,"update":"update_part"},"gauss_blt":{"rnd":{"c":[9,10,9],"dly":[45,58],"acc":[7,7.2]},"frame":0,"dv":0.9,"dr":0,"kind":1,"update":"update_blt","draw":"draw_part","die":"die_blt"},"flash":{"c":7,"rnd":{"r":[1,2],"dly":[4,8]},"frame":0,"dv":1,"dr":-0.05,"kind":2,"dr":0,"update":"update_part"},"blast":{"dly":18,"frame":0,"kind":4,"r":12,"dr":0.01,"dv":1,"update":"update_part"}}'

function make_part(part,p,v)
	local pt=add(parts,clone(all_parts[part],{pos=v_clone(p),v=v_clone(v),draw=_g.draw_part,c=c}))
	pt.t,pt.update=time_t+pt.dly,pt.update or _g.update_part
	pt.df=1/pt.dly
	if(pt.sfx) sfx_v(pt.sfx,p)
	return pt
end

function make_blast(p)
	local pt=make_part("blast",p,v_up)
	pt.r=6
	pt.dr=-0.1
	pt.dv=0
	for i=1,3 do
	 local a,o=rnd(),rnd(0.5)
	 local c,s=cos(o),-sin(o)
	 -- ranom unit vector
	 local x,y,z=c*cos(a),s,-c*sin(a)
	 for j=1,4 do
	 	local v={x,y,z}
	 	v_scale(v,2*j)
	 	v_add(v,p)
	 	pt=make_part("blast",v,{x,y,z})
	  pt.r=4/j
	  pt.dly=12*j
	  pt.t=time_t+pt.dly
	  pt.df=1/pt.dly
	 end
	end
end

function make_blt(self,wp)
	-- rebase gun anchor
	local p=v_clone(wp.pos)
	m_x_v(self.m,p)

	local pt=add(parts,clone(all_parts[wp.part],{
			actor=self, --blt owner
			pos=p,
			u=m_fwd(self.m),
			side=self.side,
			dmg=wp.dmg}))
	pt.t=time_t+pt.dly
 pt.df=1/pt.dly	
	if(wp.sfx) sfx(wp.sfx)
	return pt
end

-- map helpers
local qmap,hmap={},{}
function safe_index(i,j)
	return bor(band(i,0x7f),shl(band(j,0x7f),7))
end
function get_raw_qcode(i,j)
	return qmap[safe_index(i,j)]
end
function get_height(i,j)
	-- already scaled
	return shl(hmap[safe_index(i,j)],4)
end
function get_color(q)
	return shr(band(0xf0,q),4),shr(band(0xf00,q),8)
end

function get_altitude(x,z)
	-- cell
	local dx,dz=shr(band(x,0x7f)%ground_scale,4),shr(band(z,0x7f)%ground_scale,4)
	local i,j=flr(shr(x,4)),flr(shr(z/4))
	local h0,h1=lerp(get_height(i,j),get_height(i,j+1),1-dz),lerp(get_height(i+1,j),get_height(i+1,j+1),1-dz)
	return lerp(h0,h1,1-dx)
end

function update_ground()
	active_ground_actors={}
	
	local pos=plyr and plyr.pos or cam.lookat
	
	local i0,j0=flr(pos[1]/ground_scale),flr(pos[3]/ground_scale)
	for i=i0-4,i0+4 do
		local cx=band(i,0x7f)
		for j=j0-4,j0+4 do
			local cy=band(j,0x7f)
			local t=ground_actors[cx+cy*128]
			if t and not t.disabled then
				t:update(i,j)
				add(active_ground_actors,t)
			 add(drawables,t)
			end
		end
	end
end

function draw_tex_quad(a,b,sx,sy)
	palt(14,true)
	palt(0,false)
	local t,invdx,wa,wb=0,1/(b[2]-a[2]),a[4],b[4]
	for y=a[2],b[2] do
		local x,w=lerp(a[1],b[1],t),lerp(wa,wb,t)
		-- persp correction
		local u=t*wb/w
		sspr(sx,sy+16*u,16,1,x,y,shl(w,4),1)
		t+=invdx
	end
	palt()
end

function draw_ground(self)
	local imin,imax=-5,5
	local cx,cz=cam.lookat[1],cam.lookat[3]
	local dx,dz=cx%ground_scale,cz%ground_scale
	-- cell coordinates
	local nx,ny=flr(cx/ground_scale),flr(cz/ground_scale)
	
	-- project anchor points
	local p={}	
	-- grid depth extent
	for j=-8,4 do
	 -- compute grid points centered on lookat pos
		local x,y,z,w=cam:project(-dx+cx+imin*ground_scale,0,-dz+cz+j*ground_scale)
		add(p,{x,y,z,w,ny+j})
	end

 local dither={
 	[imin]=lerparray(dither_pat,1-dx/ground_scale),
 	[imax]=lerparray(dither_pat,dx/ground_scale),
 }
 
	local v0=p[1]
	local w0,nj=v0[4],v0[5]
	local dw0=ground_scale*w0
	for j=2,#p do
		local v1=p[j]		
		local w1=v1[4]
		local dw1=ground_scale*w1
		local x0,x1=flr(v0[1]),flr(v1[1])
		local x2,x3=flr(v1[1]+dw1),flr(v0[1]+dw0)
		-- depth dither
		if j==2 then
			fillp(lerparray(dither_pat,1-dz/ground_scale)+0.1)
		end
		-- grid width extent
		local ni=nx+imin
		local h0,h1=get_height(ni,nj),get_height(ni,nj+1)
		local y0,y1=flr(v0[2]-w0*h0),flr(v1[2]-w1*h1)
		for i=imin,imax do		 
		 local q=get_raw_qcode(ni,nj)
			local h2,h3=get_height(ni+1,nj+1),get_height(ni+1,nj)
			local y2,y3=flr(v1[2]-w1*h2),flr(v0[2]-w0*h3)

   local fp=dither[i]
   if(fp) fillp(fp)
   
   	--[[
			if i==imin then
			 local t=dx/ground_scale
				x0=lerp(x0,x3,t)
				x1=lerp(x1,x2,t)
			elseif i==imax then
			 local t=dx/ground_scale
				x3=lerp(x0,x3,t)
				x2=lerp(x1,x2,t)
			end
		]]
		 -- out of screen tile?
		 if x3>0 then
 			local c_hi,c_lo=get_color(q)
 			local q_code=band(q,0xf)
 			
 			if q_code==1 or q_code==4 then
 				trifill(x0,y0,x2,y2,x1,y1,c_hi)		
 				trifill(x0,y0,x2,y2,x3,y3,c_lo)
 			elseif q_code==9 then
 				draw_tex_quad({x0,y0,0,w0},{x1,y1,0,w1},16,0)
 			else
 				trifill(x1,y1,x3,y3,x0,y0,c_hi)
 				trifill(x1,y1,x3,y3,x2,y2,c_lo)		
 			end
			end
					
			if(fp) fillp()
			
			-- no need to go further, tile is not visible
			if(x0>127) break
			
			x0,x1=x3,x2
			y0,y1=y3,y2
			x2+=dw1
			x3+=dw0
			ni+=1
		end
		-- draw actors
		local bucket=zbuf[band(nj,0x7f)]
		if bucket then
			for _,d in pairs(bucket) do
				d=d.obj
			 -- draw shadow
			 --[[
			 local dx,dz=d.pos[1],d.pos[3]
			 local dy=get_altitude(dx,dz)
			 local sx0,sy0,sz0,sw0=cam:project(dx,dy,dz-8)
			 local sx1,sy1,sz1,sw1=cam:project(dx,dy,dz+8)
			 
			 local scale=1--1-(dy-d.pos[2])/dy
			 --sw0*=scale
			 --sw1*=scale
				sprr(32,0,48,0,plyr.angle/256,2,0.5)
				draw_tex_quad({sx0-8*sw0,sy0,sz0,sw0},{sx1-8*sw1,sy1,sz1,sw1},48,0)
				]]
				d:draw()
			end
		end
		v0,w0,dw0=v1,w1,dw1
		nj+=1
		fillp()
	end
end

function control_plyr()
	local pitch,roll,input=0,0,false

	if not plyr.firing then 
 	if(btn(0)) roll=-1 input=true
 	if(btn(1)) roll=1 input=true
 	if(btn(2)) pitch=-1
 	if(btn(3)) pitch=1		
	end

 plyr.angle+=roll
	plyr.turn_spring+=roll
	plyr.q=make_q(v_up,-plyr.angle/256)
	q_x_q(plyr.q,make_q(v_fwd,plyr.turn_spring/128))	
 
	plyr.pitch+=pitch
	q_x_q(plyr.q,make_q(v_right,-plyr.pitch/96))

	-- avoid matrix skew
	q_normz(plyr.q)
	
 -- bank turn
	-- q_x_q(plyr.q,make_q(v_fwd,plyr.turn_spring/128))

	local m=m_from_q(plyr.q)
	local fwd=m_fwd(m)
	
	-- update pos
	v_add(plyr.pos,fwd,plyr.acc)
	m_set_pos(m,plyr.pos)
	plyr.m=m
	
	-- brrt
	plyr.firing=false
	if btn(4) then
	 if plyr.fire_t<time_t then
			local wp=plyr.model["gauss_wp"]
			make_blt(plyr,wp)
			plyr.fire_t=time_t+wp.dly
		end
		plyr.firing=true
		screen_shake(1)
	end	
end

function _update()
	time_t+=1

 screen_update()

	zbuf_clear()
	
	if plyr then
		control_plyr()
		-- do not track dead player
		if not plyr.disabled then
			-- update cam
			local lookat=v_clone(plyr.pos)
			v_add(lookat,m_fwd(plyr.m),24)
			-- keep altitude
			lookat[2]=plyr.pos[2]
			cam:track(lookat)
		end
	end
	
	update_ground()
	
	zbuf_filter(actors)
	zbuf_filter(parts)
end

local padding_mask="0000"
function padding(n)
 local 	s=tostr(flr(n))
	return sub(padding_mask,1,4-#s)..s
end

function _draw()
	cls(0)
	
	zbuf_sort()
	draw_ground()
 --
 if plyr then
 	local x,y,z,w=cam:project(plyr.pos[1],plyr.pos[2],plyr.pos[3])
	 local h=get_altitude(plyr.pos[1],plyr.pos[3])
	 h=plyr.pos[2]-h
	 rectfill(106,y-7,124,y+1,0)
	 print(padding(h),108,y-5,11)
	 rect(106,y-7,124,y+1,11)
	
	 rectfill(4,y-7,22,y+1,0)
	 print(padding(300),6,y-5,11)
	 rect(4,y-7,22,y+1,11)

	 -- target reticule
	 local fwd=m_fwd(plyr.m)
	 if fwd[2]<0 then
 	 local t=-h/fwd[2]
 	 x,z=plyr.pos[1]+t*fwd[1],plyr.pos[3]+t*fwd[3]
 		h=get_altitude(x,z)
 		x,y,z,w=cam:project(x,h,z)
 		spr(36,x-4,y-3)
		end

		-- compass
		clip(40,32,48,12)
		local angle=flr(36*plyr.angle/256)
		local dx=flr(angle%16)
		for i=-1,2 do
			local a=((angle+i)%36+36)%36
			a=tostr(a)
			print(a,64-dx+i*16-#a,32,11)
		end
		for i=-1,2,0.5 do
			pset(64-dx+i*16,40,11)
	  if flr(i)==i then
				pset(64-dx+i*16,41,11)
	  end
	 end
	 clip()
	 spr(37,64,43)
	end

 -- radar	
 spr(6,0,0,2,2)
 spr(6,15,0,2,2,true)
 spr(6,0,15,2,2,false,true)
 spr(6,15,15,2,2,true,true)
	clip(0,0,31,31)
	
	-- select closest targets
 for _,a in pairs(ground_actors) do
 	local v=make_v(a.pos,plyr.pos)
		v_scale(v,0.1)
		
		print("a",15+v[1],15-v[3],11)
 end
 pal()
 palt(0,true)
 pal(14,0)
 spr(38,0,0,2,2)
 spr(38,15,0,2,2,true)
 spr(38,0,15,2,2,false,true)
 spr(38,15,15,2,2,true,true)
 
	clip()
	pal()
	
 -- fuel
 camera(-97,0)
 spr(8,0,0,2,2)
 spr(8,15,0,2,2,true)
 spr(8,0,15,2,2,false,true)
 spr(8,15,15,2,2,true,true)
 camera()
	
	local angle=time_t/96
	local c,s=cos(angle),-sin(angle)
	local x,y=112+14*c,15-14*s
	local bx,by=112+6*c,15-6*s
	trifill(bx-2*s,by-2*c,bx+2*s,by+2*c,x,y,7)

 -- status panel
 -- mission panel? cdu
 
 	
	--print("gbu:12",98,2,11)
	
	--rectfill(0,0,127,8,1)
	--print("mem:"..stat(0).." cpu:"..stat(1).."("..stat(7)..")",2,2,7)		
end

-- main
function _init()
	local noise={}
	local idx_offsets={
		0,1,128,129
	}
	local q_codes={
		{0,0,0,0},
		{0,0,1,0},
		{0,0,0,2},
		{0,0,5,5},
		{0,4,0,0},
		{2,0,0,8},
		{0,5,0,5},
	 {2,5,5,5},
	 {8,0,0,0},
	 {5,0,5,0},
	 {5,1,4,5},
	 {5,1,5,5},
	 {5,5,0,0},
	 {5,5,5,8},
	 {5,5,4,5},
	 {5,5,5,5}	
	}
	local get_noise=function(i,j)
		return noise[band(i,0x3f)+64*band(j,0x3f)+1]
	end
 -- returns whether value is above a given level
 local is_solid=function(i,j,level)
  return get_noise(i,j)>level and 1 or 0
 end
 -- converts four corners into a single sprite lookup index
 -- cf 'marching square' thingy
 local marching_code=function(i,j,level)
  return
   8*is_solid(i,j,level)+
   4*is_solid(i+1,j,level)+
   2*is_solid(i+1,j+1,level)+
   is_solid(i,j+1,level)
 end
 
 os2d_noise(48)

 for y=0,63 do
  for x=0,63 do
   local c
   -- base noise is strongest
   c=os2d_eval(x/4,y/4)
   -- next is weaker
   c+=os2d_eval(x/2,y/2)/2
   -- and so on
   c+=os2d_eval(x,y)

   -- set in stoooone
   add(noise,c)
  end
 end
	
	--[[
 for j=0,63 do
  for i=0,63 do
  	local x,y=i%8,j%8
   add(noise,(x==0 or y==0) and 1 or 0)
  end
 end
 ]]
 
 -- convert into marching quadrants
	for j=0,63 do
  for i=0,63 do
   local q=marching_code(i,j,-0.1)
   local idx=2*i+2*128*j
  	local code=q_codes[q+1]
  	for k=1,4 do
  	 local h
  	 if k==1 then
  	 	h=get_noise(i,j)
  	 elseif k==2 then
  	 	h=0.5*get_noise(i,j)+0.5*get_noise(i+1,j)
				elseif k==3 then 	 	
  	 	h=0.5*get_noise(i,j)+0.5*get_noise(i,j+1)
				elseif k==4	then
  	 	h=(get_noise(i,j)+get_noise(i,j+1)+get_noise(i+1,j)+get_noise(i+1,j+1))/4
	   end
				
  	 hmap[idx+idx_offsets[k]]=max(h)
  	end
  end
 end 

	local layers={
		{level=0.75,hi=5,lo=11},
		{level=0.5,lo=3},
		{level=0,lo=9},
		{level=-0.1,lo=12},
		{level=-0.2,lo=1}
	}
	for l=1,#layers do
	 local layer=layers[l]
 	for j=0,63 do
   for i=0,63 do
    local q=marching_code(i,j,layer.level)
    local idx=2*i+2*128*j
   	local code=q_codes[q+1]
   	for k=1,4 do
   		q=code[k]
   		-- hi/lo colors
   		local prev_q=qmap[idx+idx_offsets[k]]
   		local hi,lo
   		if prev_q then
   			hi,lo=get_color(prev_q)
 	  		-- replace hi color
	   		lo=layer.lo
   		else
   			hi,lo=layer.hi,layer.lo
   		end
   		if q==0 then
   			q=bor(q,bor(lo*16,lo*256))
   		elseif q==5 then
   			q=bor(q,bor(hi*16,hi*256))
   		elseif q==1 or q==8 then
 	  		q=bor(q,bor(hi*16,lo*256))  			
   		elseif q==4 or q==2 then
 	  		q=bor(q,bor(lo*16,hi*256))  			
   		end
    	qmap[idx+idx_offsets[k]]=q
   	end
   end
  end 
 end

	-- landing strip
	for j=0,3 do
		local idx=safe_index(0,j)
		qmap[idx]=9
		for k=1,4 do
			hmap[idx+idx_offsets[k]]=0.2
		end
	end
	
	local max_tree=100
	for j=0,63 do
  for i=0,63 do
			if max_tree>0 and is_solid(i,j,0.7)==1 then
				make_ground_actor("tree",2*i,2*j)
				max_tree-=1
			end
 	end
 end
 
	-- read models from gfx/map data
	unpack_models()

	cam=make_cam(96)
	plyr=make_actor("plyr",{0,24,0})
end

-->8
-- opensimplex noise

-- adapted from public-domain
-- code found here:
-- https://gist.github.com/kdotjpg/b1270127455a94ac5d19

--------------------------------

-- opensimplex noise in java.
-- by kurt spencer
-- 
-- v1.1 (october 5, 2014)
-- - added 2d and 4d implementations.
-- - proper gradient sets for all dimensions, from a
--   dimensionally-generalizable scheme with an actual
--   rhyme and reason behind it.
-- - removed default permutation array in favor of
--   default seed.
-- - changed seed-based constructor to be independent
--   of any particular randomization library, so results
--   will be the same when ported to other languages.

-- (1/sqrt(2+1)-1)/2
local _os2d_str=-0.211324865405187
-- (  sqrt(2+1)-1)/2
local _os2d_squ= 0.366025403784439

-- cache some constant invariant
-- expressions that were 
-- probably getting folded by 
-- kurt's compiler, but not in 
-- the pico-8 lua interpreter.
local _os2d_squ_pl1=_os2d_squ+1
local _os2d_squ_tm2=_os2d_squ*2
local _os2d_squ_tm2_pl1=_os2d_squ_tm2+1
local _os2d_squ_tm2_pl2=_os2d_squ_tm2+2

local _os2d_nrm=47

local _os2d_prm={}

-- gradients for 2d. they 
-- approximate the directions to
-- the vertices of an octagon 
-- from the center
local _os2d_grd = 
{[0]=
	 5, 2,  2, 5,
	-5, 2, -2, 5,
	 5,-2,  2,-5,
	-5,-2, -2,-5,
}

-- initializes generator using a 
-- permutation array generated 
-- from a random seed.
-- note: generates a proper 
-- permutation, rather than 
-- performing n pair swaps on a 
-- base array.
function os2d_noise(seed)
	local src={}
	for i=0,255 do
		src[i]=i
		_os2d_prm[i]=0
	end
	srand(seed)
	for i=255,0,-1 do
		local r=flr(rnd(i+1))
		_os2d_prm[i]=src[r]
		src[r]=src[i]
	end
end

-- 2d opensimplex noise.
function os2d_eval(x,y)
	-- put input coords on grid
	local sto=(x+y)*_os2d_str
	local xs=x+sto
	local ys=y+sto
	
	-- flr to get grid 
	-- coordinates of rhombus
	-- (stretched square) super-
	-- cell origin.
	local xsb=flr(xs)
	local ysb=flr(ys)
	
	-- skew out to get actual 
	-- coords of rhombus origin.
	-- we'll need these later.
	local sqo=(xsb+ysb)*_os2d_squ
	local xb=xsb+sqo
	local yb=ysb+sqo

	-- compute grid coords rel.
	-- to rhombus origin.
	local xins=xs-xsb
	local yins=ys-ysb

	-- sum those together to get
	-- a value that determines 
	-- which region we're in.
	local insum=xins+yins

	-- positions relative to 
	-- origin point.
	local dx0=x-xb
	local dy0=y-yb
	
	-- we'll be defining these 
	-- inside the next block and
	-- using them afterwards.
	local dx_ext,dy_ext,xsv_ext,ysv_ext

	local val=0

	-- contribution (1,0)
	local dx1=dx0-_os2d_squ_pl1
	local dy1=dy0-_os2d_squ
	local at1=2-dx1*dx1-dy1*dy1
	if at1>0 then
		at1*=at1
		local i=band(_os2d_prm[(_os2d_prm[(xsb+1)%256]+ysb)%256],0x0e)
		val+=at1*at1*(_os2d_grd[i]*dx1+_os2d_grd[i+1]*dy1)
	end

	-- contribution (0,1)
	local dx2=dx0-_os2d_squ
	local dy2=dy0-_os2d_squ_pl1
	local at2=2-dx2*dx2-dy2*dy2
	if at2>0 then
		at2*=at2
		local i=band(_os2d_prm[(_os2d_prm[xsb%256]+ysb+1)%256],0x0e)
		val+=at2*at2*(_os2d_grd[i]*dx2+_os2d_grd[i+1]*dy2)
	end
	
	if insum<=1 then
		-- we're inside the triangle
		-- (2-simplex) at (0,0)
		local zins=1-insum
		if zins>xins or zins>yins then
			-- (0,0) is one of the 
			-- closest two triangular
			-- vertices
			if xins>yins then
				xsv_ext=xsb+1
				ysv_ext=ysb-1
				dx_ext=dx0-1
				dy_ext=dy0+1
			else
				xsv_ext=xsb-1
				ysv_ext=ysb+1
				dx_ext=dx0+1
				dy_ext=dy0-1
			end
		else
			-- (1,0) and (0,1) are the
			-- closest two vertices.
			xsv_ext=xsb+1
			ysv_ext=ysb+1
			dx_ext=dx0-_os2d_squ_tm2_pl1
			dy_ext=dy0-_os2d_squ_tm2_pl1
		end
	else  --we're inside the triangle (2-simplex) at (1,1)
		local zins = 2-insum
		if zins<xins or zins<yins then
			-- (0,0) is one of the 
			-- closest two triangular
			-- vertices
			if xins>yins then
				xsv_ext=xsb+2
				ysv_ext=ysb
				dx_ext=dx0-_os2d_squ_tm2_pl2
				dy_ext=dy0-_os2d_squ_tm2
			else
				xsv_ext=xsb
				ysv_ext=ysb+2
				dx_ext=dx0-_os2d_squ_tm2
				dy_ext=dy0-_os2d_squ_tm2_pl2
			end
		else
			-- (1,0) and (0,1) are the
			-- closest two vertices.
			dx_ext=dx0
			dy_ext=dy0
			xsv_ext=xsb
			ysv_ext=ysb
		end
		xsb+=1
		ysb+=1
		dx0=dx0-_os2d_squ_tm2_pl1
		dy0=dy0-_os2d_squ_tm2_pl1
	end
	
	-- contribution (0,0) or (1,1)
	local at0=2-dx0*dx0-dy0*dy0
	if at0>0 then
		at0*=at0
		local i=band(_os2d_prm[(_os2d_prm[xsb%256]+ysb)%256],0x0e)
		val+=at0*at0*(_os2d_grd[i]*dx0+_os2d_grd[i+1]*dy0)
	end
	
	-- extra vertex
	local atx=2-dx_ext*dx_ext-dy_ext*dy_ext
	if atx>0 then
		atx*=atx
		local i=band(_os2d_prm[(_os2d_prm[xsv_ext%256]+ysv_ext)%256],0x0e)
		val+=atx*atx*(_os2d_grd[i]*dx_ext+_os2d_grd[i+1]*dy_ext)
	end
	return val/_os2d_nrm
end
-->8
-- trifill
function p01_trapeze_h(l,r,lt,rt,y0,y1)
 lt,rt=(lt-l)/(y1-y0),(rt-r)/(y1-y0)
 if(y0<0)l,r,y0=l-y0*lt,r-y0*rt,0 
	y1=min(y1,128)
	for y0=y0,y1 do
  rectfill(l,y0,r,y0)
  l+=lt
  r+=rt
 end
end
function p01_trapeze_w(t,b,tt,bt,x0,x1)
 tt,bt=(tt-t)/(x1-x0),(bt-b)/(x1-x0)
 if(x0<0)t,b,x0=t-x0*tt,b-x0*bt,0 
 x1=min(x1,128)
 for x0=x0,x1 do
  rectfill(x0,t,x0,b)
  t+=tt
  b+=bt
 end
end
function trifill(x0,y0,x1,y1,x2,y2,col)
 color(col)
 if(y1<y0)x0,x1,y0,y1=x1,x0,y1,y0
 if(y2<y0)x0,x2,y0,y2=x2,x0,y2,y0
 if(y2<y1)x1,x2,y1,y2=x2,x1,y2,y1
 if max(x2,max(x1,x0))-min(x2,min(x1,x0)) > y2-y0 then
  col=x0+(x2-x0)/(y2-y0)*(y1-y0)
  p01_trapeze_h(x0,x0,x1,col,y0,y1)
  p01_trapeze_h(x1,col,x2,x2,y1,y2)
 else
  if(x1<x0)x0,x1,y0,y1=x1,x0,y1,y0
  if(x2<x0)x0,x2,y0,y2=x2,x0,y2,y0
  if(x2<x1)x1,x2,y1,y2=x2,x1,y2,y1
  col=y0+(y2-y0)/(x2-x0)*(x1-x0)
  p01_trapeze_w(y0,y0,y1,col,x0,x1)
  p01_trapeze_w(y1,col,y2,y2,x1,x2)
 end
end

-->8
-- unpack models
local mem=0x1000
function unpack_int()
	local i=peek(mem)
	mem+=1
	return i
end
function unpack_float(scale)
	local f=(unpack_int()-128)/32	
	return f*(scale or 1)
end
-- valid chars for model names
local itoa='_0123456789abcdefghijklmnopqrstuvwxyz'
function unpack_string()
	local s=""
	for i=1,unpack_int() do
		local c=unpack_int()
		s=s..sub(itoa,c,c)
	end
	return s
end
function unpack_models()
	-- for all models
	for m=1,unpack_int() do
		local model,name,scale={},unpack_string(),unpack_int()
		scale=4
		-- vertices
		model.v={}
		for i=1,unpack_int() do
			add(model.v,{unpack_float(scale),unpack_float(scale),unpack_float(scale)})
		end
		
		-- faces
		model.f={}
		for i=1,unpack_int() do
			local f={p0=unpack_int(),ni=i,vi={}}
			for i=1,unpack_int() do
				add(f.vi,unpack_int())
			end
			-- center point
			f.center={unpack_float(scale),unpack_float(scale),unpack_float(scale)}
			-- material id
			f.material=unpack_int()
			add(model.f,f)
		end

		-- normals
		model.n={}
		for i=1,unpack_int() do
			add(model.n,{unpack_float(),unpack_float(),unpack_float()})
		end
		printh("n:"..#model.n)
		
		-- n.p cache	
		model.cp={}
		for i=1,#model.f do
			local f,n=model.f[i],model.n[i]
			add(model.cp,v_dot(n,model.v[f.p0]))
		end

		-- merge with existing model
		all_models[name]=clone(model,all_models[name])
	end
end
__gfx__
1ca9b345000700004070000770000704eeeeeeeeeeeeeeee00000000000333330000000000077777000000000000000000000000000000000000000000000000
0000000001dc70004070000770000704eeee00000000eeee00000000333100030000000077750007000000000000000000000000000000000000000000000000
7985100012e770004070000770000704eeee00000000eeee00000033100000030000007750700007000000000000000000000000000000000000000000000000
0000000013b7b0004070000000000704eeeeeee00eeeeeee00000310000000030000075000500000000000000000000000000000000000000000000000000000
00000000249a70004070000000000704eeeeeee00eeeeeee00003000000000030000700000000000000000000000000000000000000000000000000000000000
0000000015d670004070000000000704eeeeee0000eeeeee00030300000000030007070000000000000000000000000000000000000000000000000000000000
00000000000000004070000000000704eeeeee0000eeeeee00310030000000030075007000000000000000000000000000000000000000000000000000000000
00000000000000004070000000000704ee000000000000ee00300003000000030070000000000000000000000000000000000000000000000000000000000000
5c000000000000004070000770000704e00000000000000e03100000000133330750000000000000000000000000000000000000000000000000000000000000
00000000000000004070000770000704e00000000000000e03000000003310030700000000000000000000000000000000000000000000000000000000000000
00000000000000004070000770000704eeeeee0000eeeeee030000000310000b0775000000000000000000000000000000000000000000000000000000000000
00000000000000004070000000000704eeeeee0000eeeeee310000001300000b7500000000000000000000000000000000000000000000000000000000000000
00000000000000004070000000000704eeeeee0000eeeeee30000000310000bb7000000000000000000000000000000000000000000000000000000000000000
00000000000000004070000000000704eeeeee0000eeeeee30000000300000007000000000000000000000000000000000000000000000000000000000000000
00000000000000004070000000000704eeeeeee00eeeeeee300000003000b0007000000000000000000000000000000000000000000000000000000000000000
00000000000000004070000000000704eeeeeeeeeeeeeeee3333333333bbb00b7770000000000000000000000000000000000000000000000000000000000000
00000000000000004070000000000704000bb00000b00000eeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000
0000033000333000407000000000070400b00b000bbb0000eeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000
00003333033303004070000000000704bb0000bbbbbbb000eeeeee00000000000000000000000000000000000000000000000000000000000000000000000000
0000000333300000407000000000070400b00b0000000000eeeee000000000000000000000000000000000000000000000000000000000000000000000000000
00003333333000004070000000000704000bb00000000000eeee0000000000000000000000000000000000000000000000000000000000000000000000000000
000333055033000040700000000007040000000000000000eee00000000000000000000000000000000000000000000000000000000000000000000000000000
003300450003300040700000000007040000000000000000ee000000000000000000000000000000000000000000000000000000000000000000000000000000
000000440033300040700000000007040000000000000000ee000000000000000000000000000000000000000000000000000000000000000000000000000000
00000040003300004070000000000704eeeeeeee00000000e0000000000000000000000000000000000000000000000000000000000000000000000000000000
00000040000000004070770770770704e76eeeee00000000e0000000000000000000000000000000000000000000000000000000000000000000000000000000
00000440000000004070770770770704e7776eee00000000e0000000000000000000000000000000000000000000000000000000000000000000000000000000
00000440000000004070770770770704e777776e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000440000000004070770770770704e777776e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00004440000000004070770770770704e7776eee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00004444000000004070770770770704e76eeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00044444400000004070000000000704eeeeeeee0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
305012e13141b1806069080888b7a908b7e6a6080808b8a787b7a980103010205098287800103010302098c73800203020306008b77800303030105088188700
403040506077287800403040603077c7380020302060500818c80030303050408718870080b8c9b89816e7080608f8099657c9b87716e708d9e817099640f1d1
010110806808380868080808a7080a08a708380848f8d848873748876010301020302828f7003030302050e728f700603060407048e828005030502010082828
00703070408008e8b7008030804060c7e828006099e82776e827b988f808e8d90888165688f830c03020109358c8ab78d73a48183378193a489833582829e968
6358282748785d48085d386894e92827e928293868637d9877e9c9647d98d8e9b983e9689488f845e8594578f837e87937e8884559f845e8783769f83708a87c
b7c8ab0839bb97d73ac7183397193ac7983308193ab72829266863b72827c7785dc7085dd76894262827262829d7686392987726c9649298d826b98326689487
f84527594597f837277937278845b6f845277837a6f83722103010e1c118d8fb10504050220230085833002040204050306868e60040404020a09068589b00d0
40d0c0f0119b582800314031b0e07019680400d040d06080c029282800314031702101e9090400414041517161b829460061406171b1a1e8f83700a140a1b191
8129b84600814081915141e8f84500614061a18141b8b84600b140b171519129294600e130e1103218090b10d130d1c1e1f7d8fb0050405040122208d8c600f1
40f1022212a768e60030403002f12008f7c6001240127282f1a7589b0090409072124008c8bb00b240b2f2d2a27458280013401352c292f6680400b240b2a262
42e6282800134013e20352260904002340234353335729460043404383935327f83700834083637393e6b8460063406323337327f8450043404323638357b846
00934093733353e6294600e130e132d1f7090b00904090a0827208385d00a040a020f18208e7bb00227617470808060a08f70a0828c70a08080a08080a080608
089679f708080a7996f70808069696f77979f766e618991747080ae70608f70806f706082808f978480a08080a08080a080a08087979f708080a9696f7080806
7996f79679f7a9e61808080a08062800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
