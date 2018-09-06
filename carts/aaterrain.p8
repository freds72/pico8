pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- globals
local time_t=0
local actors,ground_actors,parts,light,cam,plyr,active_ground_actors={},{},{},{0,1,0}

-- world units
local ground_shift,hscale=4,4
local ground_scale=2^ground_shift
local v_grav={0,-0.1,0}

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
	local ci,cj=flr(shr(cam.lookat[1],ground_shift)),flr(shr(cam.lookat[3],ground_shift))
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
local v_fwd,v_right,v_up,v_zero={0,0,1},{1,0,0},{0,1,0},{0,0,0}

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
local all_models=json_parse'{"tree":{"c":3,"r":4},"a10":{"gauss_wp":{"pos":[0,0,-3],"part":"gauss_blt","dly":8}},"tower":{},"t72":{"gun":{"pos":[0,4,0],"part":"gun_blt","dly":8}}}'
local dither_pat=json_parse'[0b1111111111111111,0b0111111111111111,0b0111111111011111,0b0101111111011111,0b0101111101011111,0b0101101101011111,0b0101101101011110,0b0101101001011110,0b0101101001011010,0b0001101001011010,0b0001101001001010,0b0000101001001010,0b0000101000001010,0b0000001000001010,0b0000001000001000,0b0000000000000000]'
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
		local d=v_dot(n,cam_pos)
		f.flipn=(f.double_sided and d<model.cp[i]) and -1 or 1
		
		if f.double_sided or d>=model.cp[i] then
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
		local c=max(f.flipn*v_dot(model.n[f.ni],l))*5
		c=sget(c+8,f.c)
		local p0=p[f.vi[1]]
	 	for i=2,#f.vi-1 do
		 	local p1,p2=p[f.vi[i]],p[f.vi[i+1]]
		 	trifill(p0[1],p0[2],p1[1],p1[2],p2[1],p2[2],c)
		end
	end
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

 self.r_fuel-=plyr.r_acc
 self.l_fuel-=plyr.l_acc
 	
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

_g.update_ground_npc=function(self)
	
end

local all_actors=json_parse'{"plyr":{"model":"a10","l_acc":0.8,"r_acc":0.8,"l_fuel":3000,"r_fuel":3000,"fire_t":0,"pitch":0,"roll":0,"turn":0,"turn_spring":0,"update":"update_plyr","angle":0.25,"smoke_dly":12,"smoke_t":0},"tower":{"model":"tower","update":"nop"},"tree":{"model":"tree","hp":4,"update":"nop","hit":"hit_npc","die":"die_actor","rnd":{"angle":[0,1]}},"t72":{"model":"t72","hp":4,"update":"nop","hit":"hit_npc","die":"die_actor","rnd":{"angle":[0,1]}}}'

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
	local x,z=shl(i+rnd(),ground_shift),shl(j-rnd(),ground_shift)
	local a=clone(all_actors[src],{
		pos={x,get_altitude(x,z),z},
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
		lookat={0,0,-7*16},
		focal=f,
		dist=shl(4,ground_shift),
		-- camera rotation
		c=1,s=0,
		track=function(self,pos,angle)
			self.pos=v_clone(pos)
			self.lookat=v_clone(pos)
			angle=max(angle,0.02)
			self.c,self.s=cos(angle),-sin(angle)
			v_add(self.pos,{0,self.dist*self.s,self.dist*self.c})
		end,
		project=function(self,x,y,z)
			x-=self.lookat[1]
			y-=self.lookat[2]
			z-=self.lookat[3]
			z,y=self.c*z+self.s*y,-self.s*z+self.c*y

  			local xe,ye,ze=x,y,z-self.dist

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
	v_add(p,self.v,self.acc)
	-- gravity
	v_add(self.v,v_grav)
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
	 -- fire smoke
		local s=flr(6*self.frame)
		local c0,c1=sget(s,2),sget(max(s-1),2)
		fillp(lerparray(dither_pat,1-self.frame))
		circfill(x,y,w*self.r,bor(shl(c1,4),c0))
		fillp()
	end
end

_g.die_blt=function(self)
	make_part(self.die_part or "flash",self.pos,v_zero)
	--make_blast(self.pos)
	
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

_g.update_emitter=function(self)
	if time_t%4==0 then
		make_part(self.part,self.pos,v_up)
	end
	
	return _g.update_part(self)
end

all_parts=json_parse'{"smoke":{"rnd":{"r":[2,3],"c":[5,6,7],"dly":[24,32]},"frame":0,"dv":0.9,"dr":-0.05,"kind":0},"gauss_blt":{"dmg":2,"rnd":{"c":[7,10,10],"dly":[45,58],"acc":[7,7.2]},"frame":0,"dv":0.9,"dr":0,"kind":1,"update":"update_blt","draw":"draw_part","die":"die_blt"},"flash":{"c":7,"rnd":{"r":[1,2],"dly":[4,8]},"frame":0,"dv":1,"dr":-0.05,"kind":2,"dr":0},"blast":{"dly":18,"frame":0,"kind":2,"r":8,"dr":-0.8,"dv":0,"c":7},"debris":{"part":"fire","frame":0,"kind":-1,"r":1,"dr":0,"dv":1,"rnd":{"acc":[0.8,1.5],"dly":[24,40]},"update":"update_emitter"},"fire":{"rnd":{"r":[1,2],"dly":[18,24]},"frame":0,"dv":0.9,"dr":0.05,"kind":4}}'

function make_part(part,p,v)
	local pt=add(parts,clone(all_parts[part],{pos=v_clone(p),v=v_clone(v),draw=_g.draw_part,c=c}))
	pt.t,pt.update=time_t+pt.dly,pt.update or _g.update_part
	pt.df=1/pt.dly
	if(pt.sfx) sfx_v(pt.sfx,p)
	return pt
end

function make_blast(p)
	local pt=make_part("blast",p,v_zero)
	for i=1,3 do
	 local a,o=rnd(),rnd(0.5)
	 local c,s=cos(o),-sin(o)
	 -- ranom unit vector (half dome)
	 local x,y,z=c*cos(a),s,-c*sin(a)
	 local v={2*x,2*y,2*z}
		v_add(v,p)
	 make_part("debris",v,{x,y,z})
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
			side=self.side}))
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
	return hmap[safe_index(i,j)]
end
function get_q_colors(q)
	return shr(band(0xf0,q),4),shr(band(0xf00,q),8)
end

function get_altitude(x,z)
	-- cell
	local dx,dz=shr(band(x,0x7f)%ground_scale,ground_shift),shr(band(z,0x7f)%ground_scale,ground_shift)
	local i,j=flr(shr(x,ground_shift)),flr(shr(z,ground_shift))
	local h0,h1=lerp(get_height(i,j),get_height(i,j+1),dz),lerp(get_height(i+1,j),get_height(i+1,j+1),dz)
	return lerp(h0,h1,dx)
end

function update_ground()
	active_ground_actors={}
	
	local pos=plyr and plyr.pos or cam.lookat
	
	local i0,j0=flr(shr(pos[1],ground_shift)),flr(shr(pos[3],ground_shift))
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

function draw_tex_quad(a,b,s)
 -- sprite num to coords
 local sx,sy=band(s*8,127),8*flr(shr(s,4))
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

-- draw actors on strip j
function draw_actors(j)
	local bucket=zbuf[band(j,0x7f)]
	if bucket then
		for _,d in pairs(bucket) do
			d=d.obj
			d:draw()
			if d==plyr then
				local x,z=d.pos[1],d.pos[z]
				local y=get_altitude(x,z)
				local dx,dz=shr(band(x,0x7f)%ground_scale,ground_shift),shr(band(z,0x7f)%ground_scale,ground_shift)
				local i,j=flr(shr(x,ground_shift)),flr(shr(z,ground_shift))
				local x,y,z,w=cam:project(d.pos[1],y,d.pos[3])
				circfill(x,y,2,8)
				
				print(dx.."/"..dz,x,y-12)
				print(i.."/"..j,x,y-6)
			end
		end
	end
end

function draw_ground(self)
	local imin,imax=-5,5
	local cx,cz=cam.lookat[1],cam.lookat[3]
	-- cell x/z ratio
	local dx,dz=cx%ground_scale,cz%ground_scale
	-- cell coordinates
	local nx,ny=flr(shr(cx,ground_shift)),flr(shr(cz,ground_shift))
	
	-- project anchor points
	local p={}
	-- grid depth extent
	for j=-9,3 do
	 -- compute grid points centered on lookat pos
		local x,y,z,w=cam:project(-dx+cx+shl(imin,ground_shift),0,-dz+cz+shl(j,ground_shift))
		add(p,{x,y,z,w,ny+j})
	end

 local dither={
 	[imin]=lerparray(dither_pat,1-shr(dx,ground_shift)),
 	[imax]=lerparray(dither_pat,shr(dx,ground_shift)),
 }
 
	local v0=p[1]
	local w0,nj=v0[4],v0[5]
	local dw0=shl(w0,ground_shift)
	for j=2,#p do
		local v1=p[j]
		local w1=v1[4]
		local dw1=shl(w1,ground_shift)
		local x0,x1=v0[1],v1[1]
		local x2,x3=v1[1]+dw1,v0[1]+dw0
		-- depth dither
		if j==2 then
			fillp(lerparray(dither_pat,1-shr(dz,ground_shift))+0.1)
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

		 -- in screen tile?
		 if x3>0 then
 			local c_hi,c_lo=get_q_colors(q)
 			local q_code=band(q,0xf)
 			if q_code==1 or q_code==4 then
 				trifill(x0,y0,x2,y2,x1,y1,c_hi)		
 				trifill(x0,y0,x2,y2,x3,y3,c_lo)
 			elseif q_code==9 then
 				draw_tex_quad({x0,y0,0,w0},{x1,y1,0,w1},shr(band(q,0xfff0),4))
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
		draw_actors(nj)
		
		v0,w0,dw0=v1,w1,dw1
		nj+=1
		fillp()
	end
	-- last strip
	draw_actors(nj)
end

function control_plyr()
	local pitch,roll=0,0

 if btn(5) then
 	local acc=0
 	if(btn(2)) acc=-0.05
 	if(btn(3)) acc=0.05
 	plyr.l_acc=mid(plyr.l_acc+acc,0,1)
 	plyr.r_acc=mid(plyr.r_acc+acc,0,1)
	elseif not plyr.firing then 
 	if(btn(0)) roll=-1
 	if(btn(1)) roll=1
 	if(btn(2)) pitch=-1
 	if(btn(3)) pitch=1		
	end

 -- total thrust
	plyr.acc=0.5*(plyr.l_acc+plyr.r_acc)

 -- add engines thrust to roll
 plyr.angle+=roll+(0.2*plyr.l_acc-0.2*plyr.r_acc)
	plyr.turn_spring+=roll
	plyr.q=make_q(v_up,-plyr.angle/256)
	q_x_q(plyr.q,make_q(v_fwd,plyr.turn_spring/128))	
 
	plyr.pitch+=pitch
	q_x_q(plyr.q,make_q(v_right,-plyr.pitch/96))

	local m=m_from_q(plyr.q)
	local fwd=m_fwd(m)
	
	-- update pos
	v_add(plyr.pos,fwd,plyr.acc)
	--todo perf check only
	--v_add(plyr.pos,v_grav,1-plyr.acc/0.8)
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

local cx,cy,cz=0,0,0
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
			cam:track(lookat,-plyr.pitch/64)
		end
	end


	update_ground()

	zbuf_filter(actors)
	zbuf_filter(parts)
end

-- string functions
local padding_mask="0000"
function padding(n)
 	local s=tostr(flr(n))
	return sub(padding_mask,1,4-#s)..s
end

-- from:https://www.lexaloffle.com/bbs/?tid=3217
function smallcaps(s)
  local d,c=""
  for i=1,#s do
    local a=sub(s,i,i)
    if a!="^" then
      if not c then
        for j=1,26 do
          if a==sub("abcdefghijklmnopqrstuvwxyz",j,j) then
            a=sub("\65\66\67\68\69\70\71\72\73\74\75\76\77\78\79\80\81\82\83\84\85\86\87\88\89\90\91\92",j,j)
          end
        end
      end
      d=d..a
      c=true
    end
    c=not c
  end
  return d
end

function draw_num_box(n,x,y,c)
 rectfill(x,y-7,x+18,y+1,0)
 print(padding(n),x+2,y-5,c)
 rect(x,y-7,x+18,y+1,c)
end

function draw_fuel_level(f,isleft)
	local angle=0.80+0.4*f/3000
	local c,s=cos(angle),-sin(angle)
	if(isleft) c=-c
	local x,y=112+14*c,15-14*s
	local bx,by=112+6*c,15-6*s
	trifill(bx-2*s,by-2*c,bx+2*s,by+2*c,x,y,7)
end

-- x/y:top/left corner
function draw_gauge(s,x,y)
	pal(14,0)
	spr(s,x,y,2,2)
	spr(s,x+15,y,2,2,true)
	spr(s,x,y+15,2,2,false,true)
	spr(s,x+15,y+15,2,2,true,true)
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
		draw_num_box(h,106,y,11)

		draw_num_box(plyr.acc*300,4,y,11)

		-- collision warning
		local fwd=m_fwd(plyr.m)
		if h<4 and time_t%16>8 and v_dot(fwd,v_up)<0 then
			line(64-16,y-16,64+16,y+16,11)
			line(64+16,y-16,64-16,y+16,11)
		end

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


		-- radar	
		draw_gauge(6,0,0)
		clip(0,0,31,31)

		-- todo: select closest targets
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
		draw_gauge(8,97,0)
		print("0",111,21,7)

		draw_fuel_level(plyr.r_fuel)
		draw_fuel_level(plyr.l_fuel,true)
		draw_num_box(plyr.r_fuel+plyr.l_fuel,103,7,7)

		-- status panel
		-- coordinates
		local airports={
			{name=smallcaps("kutaisi"),coords="15/45"},
			{name=smallcaps("senaki"),coords="56/7"}}
		print(flr(plyr.pos[x]).."/"..flr(plyr.pos[3]),33,0,3)
	 y=6
	 for i=1,#airports do
	 	local airport=airports[i]
	  print(airport.name,33,y,3)
	  y+=6
	  print("◆"..airport.coords,32,y,3)
	 	y+=5
	 end
		
		-- engine rpm
		pal(14,0)
		spr(40,80,0,2,2)
		spr(40,63,0,2,2)
		pal()

		local angle=0.2-plyr.r_acc*0.8
		line(87,8,87+6*cos(angle),8+6*sin(angle),7)
		angle=0.2-plyr.l_acc*0.8
		line(70,8,70+6*cos(angle),8+6*sin(angle),7)
		--print("gbu:12",98,2,11)
 	end
	--rectfill(0,0,127,8,1)
	--print("mem:"..stat(0).." cpu:"..stat(1).."("..stat(7)..")",2,2,7)
end

-- main

function _init()
	local idx_offsets=json_parse'[0,1,128,129]'
	local q_codes=json_parse'[[0,0,0,0],[0,0,1,0],[0,0,0,2],[0,0,5,5],[0,4,0,0],[2,0,0,8],[0,5,0,5],[2,5,5,5],[8,0,0,0],[5,0,5,0],[5,1,4,5],[5,1,5,5],[5,5,0,0],[5,5,5,8],[5,5,4,5],[5,5,5,5]]'
	
	-- temp array to store the 64x64 noise map
	local noise={}
	local get_noise=function(i,j)
		return noise[band(i,0x3f)+64*band(j,0x3f)+1]
	end
	-- returns whether value is above a given level
	local is_solid=function(i,j,level)
		return get_noise(i,j)>level and 1 or 0
	end
	-- converts four corners into a single lookup index
	-- cf: https://en.wikipedia.org/wiki/marching_squares
	local marching_code=function(i,j,level)
		return
		8*is_solid(i,j,level)+
		4*is_solid(i+1,j,level)+
		2*is_solid(i+1,j+1,level)+
		is_solid(i,j+1,level)
	end
	local set_q_colors=function(q,lo,hi)
		return bor(q,bor(lo*16,hi*256))
	end
	
	local perlin="210000000000000000000000000000100000000000012344566788887788876453210000000000000000000000000010000000000001234455578887778987646542110000000000000000000000000000000000000012345556788777899763766421100000000000000000000000000000000000001234566778888888875377754320000000000000000000000000000000000000123466677899898876538876653110000000000000000000000000000000000012346667889889987643887776421000000000000000000000000000000000001224566788889998764488877643211000000000000000000000000000000000112345577788999865447887775432110000000000000000000000000000000011123457888888876544788776654332100000000000000000000000000000011101235788777766544478876666554420000000000000000000000112210011211123577777766544337887666766542000000000000000000000012321222222112357777766654322777666777654210000000000000000000011233223322111246777776654432266667776655321000000000000000000000122222333222234677666665444336556776555532100000000000000000000001122233333323567766655334444446666655554221000000000000000000000000123443333456776554433566634666555565433210000000000000000000000002344433445666554444457763456654556544321000000000000000000000000123433333466665444446887334565455543333211111111100000000000000000122233334566544556788823455543333344542221222210000000000000000011122223456655566788882345544333234555443333321000000000000000000111223345555556778998134544333223455555544542100000000000000000001222233455556678999813444333222345666665554210000000000000000000232333455556667888882334332221235666665555421000000000000000000123444456655566668888222322222223566665445542100000000000000001223455556655556655677801221112233455556544454211000000000000000123455556665555555456770000001234444445554444321100000000000000012345545555445554444666000000123444333455444311110000000000000001234444444444444444456600000012334432234444432100000000000000000123333334443333334445660000001223444322344543210111000100000000013444323333332122344555000001222333432234465432111100111000000112344442222222101223344400000223322233223456554222222221000011123334454332222100112223330000012322212222445665544333332100001223444455433322110111122333000001332221112235566554444432100001223444555665543321111101233300000133222112234555554444443220000233344566667776554211122223230000012233211234555554334454443211234444567888999876543333333334000001223211122344443333445555442234555568899baa99876544444444450000012232122222333322344566666544455556789aabba9988765555566556000001233322112222222234678888866666666789aabbbaa9876655555677660000012333221111212223357789aa98777887899aabbbaaa986554455577777000011233321111111112235779aaaa9899a99aaabbbcbaa98754555566777780000122332222221111112457899abbbaabbbbbbcccddcba97644566677677870001233333332221111001357899abbccccdccccdeeedcba876445666776677700013443323222221110024579aaabbccddeddddeffedcba875445666766667700002444322222222111124689aabcccdeeedddeeffedcb9865445666566776600001344322222111111234689abcddddeeddddeeeedcba9865455665567776600112344322222221101234789bcdddcddccccdddeddcb9876544555456776650111233322222232111124579abcdddddccbbbcccdedca9876544454456777650112223321122222222235689bbccddddccbbbbbcdedca9876655444456666540012222211112233333456789bbccccdddcbbbbbcdddca987665544445555543000112210001123444556789aaabbbccddcbbaaabcccba887655554344444432000001110001223456778999aa999abbdccba9aabbbba9877655554443332222000000000001235667899999998889abcccba99aaaa98877665555433222111100000000011234667899aa987778899abcbba99999976665555434422101110000000000122234578999aa9876777899abbba99aa8766544444433321100000000000000122234578899a9987666678899aa99aa98665433444333331000011200000001222334677889aaa9766667778998899a976643223333443310000123000000012334456777899aa9876666668888899a987643221223443221000233000000013445666777899aaa98765555778899aa98765332112344321100023300000002356777777889999999765445778899aaa8766432112333211001123300000002468898778888889998765445677789aaa876654211222222101222330000000247899998998777898877644567789aaaa987653222122333222222210000001357899aa9998766788765544578889aaa998765433322233333322210000000247889abba988765677655444678889aaa988765444433334444321100"
	
	for i=1,#perlin do
		local c=tonum("0x"..sub(perlin,i,i))
		add(noise,c)
	end

	-- height map weights
	local hweights=json_parse'[[1,0,0,0],[0.5,0,0.5,0],[0.5,0.5,0,0],[0.25,0.25,0.25,0.25]]'
	-- explode the 64x64 map into 128x128
	for j=0,63 do
		for i=0,63 do
			local idx=2*i+2*128*j
			for k=1,4 do
				local w=hweights[k]
				
				local h=w[1]*get_noise(i,j)+w[2]*get_noise(i,j+1)+w[3]*get_noise(i+1,j)+w[4]*get_noise(i+1,j+1)
				hmap[idx+idx_offsets[k]]=shl(max(h),1)
			end
		end
	end 	
	
	-- create multiple layers
	local layers=json_parse'[{"level":1,"hi":10,"lo":12,"h":0},{"level":4,"hi":9},{"level":7,"hi":5},{"level":12,"hi":3}]'

	for l=1,#layers do
		local layer=layers[l]
		for j=0,63 do
			for i=0,63 do
				local q=marching_code(i,j,layer.level)
				local idx=2*i+2*128*j
				local code=q_codes[q+1]
				for k=1,4 do
					local q,k=code[k],idx+idx_offsets[k]
					-- hi/lo colors
					local hi,lo=layer.hi,layer.lo
					-- previous tile
					local prev_q=qmap[k]
					if prev_q then
						prev_hi,prev_lo=get_q_colors(prev_q)
						-- replace lo color
						lo=prev_lo
					end
					-- replace only full hi tiles
					prev_q=band(0xf,prev_q or 5)
					if prev_q==5 then
						if q==0 then
							q=set_q_colors(q,lo,lo)
						 -- kill height
						 if layer.h then
						 	hmap[k]=layer.h
						 end
						elseif q==5 then
							q=set_q_colors(q,hi,hi)
						elseif q==1 or q==8 then
							q=set_q_colors(q,hi,lo)
						elseif q==4 or q==2 then
							q=set_q_colors(q,lo,hi)
						end
						qmap[k]=q
					end
				end
			end
		end
	end

 -- textured tile example
	-- landing strip
	local idx=safe_index(0,0)
	qmap[idx]=bor(9,16*34)
 for j=1,7 do
		local idx=safe_index(0,j)
		qmap[idx]=bor(9,16*2)
		for k=1,4 do
			hmap[idx+idx_offsets[k]]=2
		end
	end
		
	-- control tower (test)
	local a=make_ground_actor("tower",2,4)
	m_set_pos(a.m,{16,0.2,34})

	local max_tree=100
	for j=0,63 do
		for i=0,63 do
			if max_tree>0 and is_solid(i,j,8)==1 then
				make_ground_actor("tree",2*i,2*j)
				max_tree-=1
			end
		end
	end

	-- read models from gfx/map data
	unpack_models()

	cam=make_cam(96)

	plyr=make_actor("plyr",{0,44,0})
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
			-- color
			f.c=unpack_int()
			-- double_sided?
			f.double_sided=unpack_int()==1
			add(model.f,f)
		end

		-- normals
		model.n={}
		for i=1,unpack_int() do
			add(model.n,{unpack_float(),unpack_float(),unpack_float()})
		end
		
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
1ca9b345000700000600000000000060eeeeeeeeeeeeeeee00000000000333330000000000077777eeeee000000eeeee00000000000000000000000000000000
0000000001dc77000600000000000060eeee00000000eeee000000003331eee3000000007775eee7eee0000000000eee00000000000000000000000000000000
7985100012e777000600000000000060eeee00000000eeee000000331eeeeee3000000775e7eeee7ee000000000000ee00000000000000000000000000000000
000000003333b7000600000770000060eeeeeee00eeeeeee0000031eeeeeeee30000075eee5eeeeee00000000000000e00000000000000000000000000000000
000000002497a7000600000770000060eeeeeee00eeeeeee00003eeeeeeeeee300007eeeeeeeeeeee00000000000000e00000000000000000000000000000000
0000000015d767000600000770000060eeeeee0000eeeeee0003e3eeeeeeeee30007e7eeeeeeeeee000000000000000000000000000000000000000000000000
00000000156767000600000000000060eeeeee0000eeeeee0031ee3eeeeeeee30075ee7eeeeeeeee000000000000000000000000000000000000000000000000
00000000000000000600000000000060ee000000000000ee003eeee3eeeeeee3007eeeeeeeeeeeee000000000000000000000000000000000000000000000000
5c000000000000000600000000000060e00000000000000e031eeeeeeee13333075eeeeeeeeeeeee000000000000000000000000000000000000000000000000
00000000999a90000600000000000060e00000000000000e03eeeeeeee331ee307eeeeeeeeeeeeee000000000000000000000000000000000000000000000000
00000000000000000600000000000060eeeeee0000eeeeee03eeeeeee31eeeeb0775eeeeeeeeeeee000000000000000000000000000000000000000000000000
00000000000000000600000770000060eeeeee0000eeeeee31eeeeee13eeeeeb75eeeeeeeeeee555e00000000000000e00000000000000000000000000000000
000000001dc77c000600000770000060eeeeee0000eeeeee3eeeeeee31eeeebb7eeeeeeeeeee5666e00000000000000e00000000000000000000000000000000
00000000000000000600000770000060eeeeee0000eeeeee3eeeeeee3eeeeeee7eeeeeeeeee56666ee000000000000ee00000000000000000000000000000000
00000000000000000600000000000060eeeeeee00eeeeeee3eeeeeee3eeebeee7eeeeeeeeee56666eee0000000000eee00000000000000000000000000000000
00000000000000000600000000000060eeeeeeeeeeeeeeee3333333333bbbeeb777eeeeeeee56666eeeee000000eeeee00000000000000000000000000000000
00000000000000000600000000000060000bb00000b00000eeeeeeeeeee00000eeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000
0000000000000000060770077007706000b00b000bbb0000eeeeeeee00000000eeeee88855eeeeee000000000000000000000000000000000000000000000000
00000000000000000607700770077060bb0000bbbbbbb000eeeeee0000000000eee990070055eeee000000000000000000000000000000000000000000000000
0000000000000000060770077007706000b00b0000000000eeeee00000000000ee30000000005eee000000000000000000000000000000000000000000000000
00000000000000000607700770077060000bb00000000000eeee000000000000e3070000000705ee000000000000000000000000000000000000000000000000
000000000000000006077007700770600000000000000000eee0000000000000e3000000000005ee000000000000000000000000000000000000000000000000
000000000000000006077007700770600000000000000000ee00000000000000300000000000005e000000000000000000000000000000000000000000000000
000000000000000006000000000000600000000000000000ee00000000000000300000000000005e000000000000000000000000000000000000000000000000
00000000000000000600000000000060eeeeeeee00000000e000000000000000370000000000073e000000000000000000000000000000000000000000000000
00000000000000000600000000000060e76eeeee00000000e000000000000000300000000000003e000000000000000000000000000000000000000000000000
00000000000000000600077007700060e7776eee00000000e000000000000000300000000000003e000000000000000000000000000000000000000000000000
00000000000000000600077007700060e777776e000000000000000000000000e3000000000003ee000000000000000000000000000000000000000000000000
00000000000000000600077007700060e777776e000000000000000000000000e3070000000703ee000000000000000000000000000000000000000000000000
00000000000000000600077007700060e7776eee000000000000000000000000ee30000000003eee000000000000000000000000000000000000000000000000
00000000000000000600077007700060e76eeeee000000000000000000000000eee330070033eeee000000000000000000000000000000000000000000000000
00000000000000000600000000000060eeeeeeee000000000000000000000000eeeee33333eeeeee000000000000000000000000000000000000000000000000
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
4040f1d10101808068083808c8080808a7086a08a7083808a8f8d8a88737a8876010301020302848f740003030302050e748f740006030604070484928300050
30502010084828400070307040800849b730008030804060c74928300060b98817568817b988f80888f90888165688f830c03020402458c8ab08a87cb7c8ab08
39bb08193a27493797f837677837d67837a6f83727195577d855577855e67855c6d855e8493778f837a8783739783769f837e8195598d855b8785529785549d8
5578e73a481833582829e9686358282708385d386894e92827e928293868637d9877e9c9647d98d8e9b983e9689497e73ac7183308193ab72829266863b72827
d76894262827262829d7686392987726c9649298d826b98326689478f83a58193a089833487833b7193a97f83ac77833a7987ca7f77c68987c68f77cb2103010
204018d8fbc000403040501018090bc0003030304020f7d8fbc0004030403050f7090bc000a040a090e0f0c6a846600080408070c0d077a8466000604060a0f0
b0e61946600090409080d0e0277846600070407060b0c057194660007050708090a06027d8370000c050c0b0f0e0d027b855900041404191813149a846600021
402171611198a84660000140015191412919466000314031817121e878466000114011615101b81946600011501101413121e8d837000061506171819151e8b8
559000a140a183b3b16858e660002240221242629b582860108240820232d11968046010224022c1e1122928286010824082d17252e909046010d340d3f30492
a7584b6000b140b1a292a108f7c66000a330a393c308d8d660001340135333037458286010734073d223f2f66804600013401303e2c2e6282860107340734363
d22609046010924092a2e3d3a758e66000f130f124040808cc6000934093a3b38348c8a66000833083149368d85b6000d340d3e3a3c3c7c8a66000b150b1b3a3
e3a20848336000c330c3f3d3a7d85b6000834083a1241468584b6000043004f3f1c748dc600004400424a19208e75b600014301424f14848dc6000144014f3c3
9308d86b6000f130f1f3140878dc6000b299f8c8a929f776f8c86629f72667f7e967f7d6a9e708060839a9e708080a080806e967f72667f739a9e7080608d6a9
e708080a0808060a08f7c70a08080a08080a080608080608180806f7080ae7480a08080a08080a080a08080608f708169819b9e7796948f6b9e7080806966948
0a08182608c8080618e908c808f97808d9e850f1a12201d14031080dd8570d68570da7b80da7b80d68083e19173e98173e77083ef6f83e77f83e980908090908
07070807070809090d09090d07070d07070d0990504050b0a040d8ad08c00020402070601097adb8c00010401060b05078adb8c00030403080702037ad08c000
7060708090a0b060083e086000014001312111080d086000c040c00111d0098a086000e040e02131f0078a086000014001c0f031088a09600090f9a70807a7b9
09a7b916a708080a08080a080a080806080808080a30f1904020a289087589085b6a08756a085b6a2a5b8979bc6a79bc6af91c8949a36a49a36a2a040a3a950a
2b950a3a680a2b68593a6959ea59483a99489a6e86087586085ba52a5ba50875a5085b08f91c8679bca579bca5f91ca549a38649a3a52a04063a95062b95063a
68062b68083a99b63a6908da99b6ea59083b5ec73a99c79a6e917070708050b0a030406a29285000204020607040f9c80c500020402010906089d83850003040
30a09010f9a894500080408070609159c94c3000614061f1b050082aa73000f140f1d1a0b008b9d33000804080c16150081abb3000d040d0f03212082b073000
014001426211a88a793000e040e0f0d0c00aaa073000e040e00111f0b99ad83000313031a28208da5e000062406221318228bafb3000b170b18171d1f161c1a5
2928500051405181b1a116c80c5000514051a1e14186d838500071407141e1d116a8945000c140c191a1b1b6c94c30006250627232f011080bd8300052405272
6242678a79300022402202123206aa073000c040c0d0120208aa953000224022327252569ad8300062406282a292e7bafb3000910a080808a6790608080866d6
089949080a0808c82608f988080a086828f90a0808a9083908280ad9d8f706080808a6790a08080866d608994908f988a728f906080808080666083936d8f700
