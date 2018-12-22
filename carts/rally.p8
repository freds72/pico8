pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- globals
local time_t,time_dt=0,1/30
local actors,ground_actors,parts,v_light,cam,plyr,active_ground_actors={},{},{},{0,1,0}
local track,ghost

function nop() return true end

local physic_actors={}
-- physic thresholds
local k_small=0.001
local k_small_v=0.01
-- baumgarte
local k_bias=0.2
local k_slop=0.05

-- world units
local ground_shift,hscale=1,4
local ground_scale=2^ground_shift
local ground_left,ground_right,ground_far,ground_near=-7,7,5,-7
local v_grav={0,-1,0}
local world={}
-- transitions mgt
local draw_state,update_state=nop,nop

-- track events
local on_new_lap=nop

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
		local di,dj=flr(shr(d.pos[1],ground_shift)),flr(shr(d.pos[3],ground_shift))
		-- todo: incorrect
		-- within viz grid?
		if abs(di-ci)<6 and abs(dj-cj)<10 then
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
function acos(c)
 return atan2(c,sqrt(1-c*c))
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

-- vector math
function sqr_dist(a,b)
	local dx,dy,dz=b[1]-a[1],b[2]-a[2],b[3]-a[3]

	dx=dx*dx+dy*dy+dz*dz
	return dx<0 and 32000 or dx
end

function make_v_cross(a,b)
	local ax,ay,az=a[1],a[2],a[3]
	local bx,by,bz=b[1],b[2],b[3]
	return {ay*bz-az*by,az*bx-ax*bz,ax*by-ay*bx}
end
-- world axis
local v_fwd,v_right,v_up,v_zero={0,0,1},{1,0,0},{0,1,0},function() return {0,0,0} end

function make_v(a,b)
	return {
		b[1]-a[1],
		b[2]-a[2],
		b[3]-a[3]}
end
function v_clone(v)
	return {v[1],v[2],v[3]}
end
function v_dot(a,b)
	return a[1]*b[1]+a[2]*b[2]+a[3]*b[3]
end
function v_sqr(a)
	return {a[1]*a[1],a[2]*a[2],a[3]*a[3]}
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
function v_min(a,b)
	return {min(a[1],b[1]),min(a[2],b[2]),min(a[3],b[3])}
end
function v_max(a,b)
	return {max(a[1],b[1]),max(a[2],b[2]),max(a[3],b[3])}
end

function serialize(v,state)
	for i=1,#v do
		add(state,v[i])
	end
end
function deserialize(state,k,v)
	for i=1,#v do
		v[i]=state[k]
		k+=1
	end
	return k
end

-- 3x3 matrix operations
function make_m(x,y,z)
	return {
		x or 1,0,0,
		0,y or 1,0,
		0,0,z or 1}
end
function m_x_v(m,v)
	local x,y,z=v[1],v[2],v[3]
	return {m[1]*x+m[4]*y+m[7]*z,m[2]*x+m[5]*y+m[8]*z,m[3]*x+m[6]*y+m[9]*z}
end
-- inplace matrix multiply invert
function m_inv_x_v(m,v,p)
	local x,y,z=v[1],v[2],v[3]
	v[1],v[2],v[3]=m[1]*x+m[2]*y+m[3]*z,m[4]*x+m[5]*y+m[6]*z,m[7]*x+m[8]*y+m[9]*z
end

-- generic matrix inverse
function m_inv(me)
	local te={
	me[9]*me[5]-me[6]*me[8],me[9]*me[2]+me[3]*me[8],me[6]*me[2]-me[3]*me[5],
	-me[9]*me[4]+me[6]*me[7],me[9]*me[1]-me[3]*me[7],-me[6]*me[1]+me[3]*me[4],
	me[9]*me[4]-me[5]*me[8],-me[8]*me[1]+me[2]*me[7],me[5]*me[1]-me[2]*me[4]}

	local det = me[1]*te[1]+me[2]*te[4]+me[3]*te[7]
	-- not inversible?
	assert(det>0)
	m_scale(te,1/det)
	return te
end

function m_scale(m,scale)
	for i=1,#m do
		m[i]*=scale
	end
end
-- matrix transpose
function m_transpose(m)
	return {
		m[1],m[4],m[7],
		m[2],m[5],m[8],
		m[3],m[6],m[9]}
end
-- matrix 
function m_x_m(a,b)
	local a11,a12,a13=a[1],a[4],a[7]
	local a21,a22,a23=a[2],a[5],a[8]
	local a31,a32,a33=a[3],a[6],a[9]
	local b11,b12,b13=b[1],b[4],b[7]
	local b21,b22,b23=b[2],b[5],b[8]
	local b31,b32,b33=b[3],b[6],b[9]
	
 return {
		a11*b11+a12*b21+a13*b31,
		a21*b11+a22*b21+a23*b31,
		a31*b11+a32*b21+a33*b31,
		a11*b12+a12*b22+a13*b32,
		a21*b12+a22*b22+a23*b32,
		a31*b12+a32*b22+a33*b32,
		a11*b13+a12*b23+a13*b33,
		a21*b13+a22*b23+a23*b33,
		a31*b13+a32*b23+a33*b33
    }
end

-- returns right vector from matrix
function m_right(m)
	return {m[1],m[2],m[3]}
end
-- returns up vector from matrix
function m_up(m)
	return {m[4],m[5],m[6]}
end
-- returns foward vector from matrix
function m_fwd(m)
	return {m[7],m[8],m[9]}
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
function q_dydt(q,v,dt)
	local dq={v[1]*dt,v[2]*dt,v[3]*dt,0}
	q_x_q(dq,q)

	q[1]+=0.5*dq[1]
	q[2]+=0.5*dq[2]
	q[3]+=0.5*dq[3]
	q[4]+=0.5*dq[4]
	q_normz(q)
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
		1-(yy+zz),xy+wz,xz-wy,
		xy-wz,1-(xx+zz),yz+wx,
		xz+wy,yz-wx,1-(xx+yy)}
end

-- model bounding box
function get_modelsize(model)
	local vmin,vmax={32000,32000,32000},{-32000,-32000,-32000}
	for _,v in pairs(model.v) do
		vmin,vmax=v_min(vmin,v),v_max(vmax,v)
	end
 return make_v(vmin,vmax)
end

-- models & rendering
local all_models={}
local dither_pat2={0xffff,0xa5a5,0x0000}

function draw_model(model,m,pos,outline)
	-- cam pos in object space
	local cam_pos=make_v(pos,cam.pos)
	m_inv_x_v(m,cam_pos)
	
	-- faces
	local faces,p={},{}
	for i=1,#model.f do
		local f,n=model.f[i],model.n[i]
		-- viz calculation
		if v_dot(n,cam_pos)>=model.cp[i] then
			-- project vertices
			for _,vi in pairs(f.vi) do
				if not p[vi] then
					local v=m_x_v(m,model.v[vi])
					v_add(v,pos)
					local x,y,z,w=cam:project(v)
					-- avoid rehash for u/v
					p[vi]={x,y,0,0}
				end
			end
			-- distance to camera (in object space)
			local d=sqr_dist(f.center,cam_pos)

			-- register faces
			add(faces,{key=d,face=f})
		end
	end

 if not outline then
		-- sort faces
		sort(faces)
	end
	
	-- draw faces using projected points
	for _,f in pairs(faces) do
		f=f.face
		local p0,uv0=p[f.vi[1]],f.uv[1]
		p0[3],p0[4]=uv0[1],uv0[2]
		for i=2,#f.vi-1 do
			local p1,p2=p[f.vi[i]],p[f.vi[i+1]]
			if outline then
				fillp(0xf0f0.f)
				trifill(p0[1],p0[2],p1[1],p1[2],p2[1],p2[2],0x1)
				fillp()
			else
				local uv1,uv2=f.uv[i],f.uv[i+1]
				p1[3],p1[4]=uv1[1],uv1[2]
				p2[3],p2[4]=uv2[1],uv2[2]
				tritex(p0,p1,p2)
			end
		end	
	end
end

function draw_model_shadow(model,m,pos)
	-- fillp(0xa5a5.f)
	-- v_light dir in object space
	local l=v_clone(v_light)
	m_inv_x_v(m,l)
	
	-- faces
	local p={}
	for i=1,#model.f do
		local f,n=model.f[i],model.n[i]
		-- viz calculation
		if v_dot(n,l)<0 then
			-- project vertices
			for _,vi in pairs(f.vi) do
				if not p[vi] then
					local v=m_x_v(m,model.v[vi])
					v_add(v,pos)
					v[2]=get_altitude_and_n(v)
					local x,y,z,w=cam:project(v)
					p[vi]={x,y,z,w}
				end
			end
			-- draw faces using projected points
			-- (no need to sort)
			local p0=p[f.vi[1]]
		 	for i=2,#f.vi-1 do
			 	local p1,p2=p[f.vi[i]],p[f.vi[i+1]]
		 		trifill(p0[1],p0[2],p1[1],p1[2],p2[1],p2[2],1)
			end
		end
	end
	fillp()
end

local sa_curve,sr_curve
function apply_curve(curve,t)
	t=127*mid(t,0,1)
	return curve[flr(t)+1]/127
end

function ssprt(self,s,x,y,z,w)	
	local x,y,z,w=cam:project(self.pos)
	-- behind cam?
	if(z>0) return
	palt(0,false)
	palt(14,true)
	local sw=self.w or 16
	w*=0.1875*sw
	s=s or self.spr
	-- todo: bench
	local sx,sy=shl(s,3)%128,shl(flr(s/16),3)
	sspr(sx,sy,sw,sw,x-w/2,y-w/2,w,w)
	palt()
end

function make_plyr(p,angle)
	local model,q=all_models["205gti"],make_q(v_up,angle or 0)
	local brake,turn,traction,rpm,max_rpm,rpm_decay=0,0,0,0,32,0

	local add_tireforce=function(self,offset,right,fwd,brake,rpm)
		-- force to world
		right=m_x_v(self.m,right)
		fwd=m_x_v(self.m,fwd)
		
		-- application point (world space)
		local pos,slide=self.pt_world(offset),false

		-- point velocity
		local relv=self:pt_velocity(pos)
		local relv_len=v_dot(relv,relv)
		-- slip angle
		local sa,sa_ratio=0,1
		if relv_len>k_small then
			-- 
			sa=-v_dot(right,relv)

			-- limiting factor (normalized unit)
			local t=acos(abs(sa)/sqrt(relv_len))-0.75
			t*=360/25
			sa_ratio=apply_curve(sr_curve,t)
		end	
		
		-- long. slip
		relv_len=v_dot(fwd,relv)
		-- convert rpm to rps
		local sr=(brake*(rpm or relv_len)-relv_len)
		relv_len=abs(relv_len)
		-- limit initial conditions
		sr=mid(sr,-relv_len,relv_len)
		if relv_len>k_small then
			sr/=relv_len
		end
		local sr_ratio=sa_ratio*apply_curve(sr_curve,abs(sr))
		-- todo: include speed
		if sr_ratio<0.25 then
			slide=true
		end
		-- todo: include terrain quality
		-- adjust long.
		sr*=48*sr_ratio

		-- limit overall enveloppe
		sa*=48*sr_ratio
		
		self.slip_angles[rpm and 2 or 1][time_t%32]=sa
		
		-- impulse factors
		sa*=self.traction_ratio
		if abs(sa)>k_small then
			v_scale(right,sa)
			self:add_impulse(right,pos)
		end
		
		sr*=self.traction_ratio
		if abs(sr)>k_small then
			v_scale(fwd,sr)
			self:add_force(fwd,pos)
		end
	
		if slide then
			pos=v_clone(pos)
			v_add(pos,m_right(self.m),rnd(2)-1)
			make_part("smoke",pos)
		end

		return sr_ratio*self.traction_ratio
	end

	local a={
		mass=32,
		hardness=0.02,
		traction_ratio=0,
		pos=v_clone(p),
		q=q,
		-- init orientation
		m=m_from_q(q),
		-- obj to world space
		pt_world=function(self,p)
			p=m_x_v(self.m,p)
			v_add(p,self.pos)
			return p
		end,
		draw=function(self)
			draw_model(model,self.m,self.pos)
		end,
		draw_shadow=function(self)
			draw_model_shadow(model,self.m,self.pos)
		end,
		control=function(self)
			local angle=0
			if(btn(0)) angle=1
			if(btn(1)) angle=-1

		--[[
			local z=0
			if(btn(2)) z=-1
			if(btn(3)) z=1
			plyr.pos[1]-=turn/4
			plyr.pos[3]+=z/4
		]]
		
			turn+=0.2*angle
			-- brake (full lock:0)
			if btn(3) then
				brake=max(brake-0.1)
			else
				brake=1
			end
			-- accelerate
			if btn(2) then
				rpm=min(rpm+1.8,max_rpm)
			else
				rpm=max(rpm-0.3)
			end
		
			-- steering angle
			angle=1-0.05*turn
			
			-- front wheels
			local c,s=cos(angle),sin(angle)
			add_tireforce(self,1,{c,0,-s},{s,0,c},brake)
			-- rear wheels
			rpm_decay=add_tireforce(self,-1.2,v_right,v_fwd,btn(5) and 0 or brake,rpm)

			if btn(4) then
				local pos=v_clone(self.pos)
				v_add(pos,m_up(self.m),3)
				local force=v_clone(v_up)
				v_scale(force,12)
				self:add_force(force,pos)
			end
		end,
		update=function(self)
			traction+=self:up_ratio()
			self.traction_ratio=traction/20
			
			-- time decay
			traction*=0.8
			turn*=0.92
			--rpm*=(1-0.24*rpm_decay)
			
			self.rpm_ratio=rpm/max_rpm

			-- sound
			local speed=rpm*(0.8+0.2*rnd())
			local sspd = speed*2
			if (sspd>=1) sspd=speed*1.2
			if (sspd>=1) sspd=speed*0.7
			if (sspd>=1) sspd=speed*0.49
			sspd=sspd-flr(sspd)+speed/6
			poke(0x3200, sspd*2)
			poke(0x3202, sspd*4)

 		if rnd()>0.9 then
 			local pos=self:pt_world({0,0,-1.8})
 			--add(pos,v_up)
 			make_part("fire",pos)
 		end
		
			return true
		end
	}
	return add(actors,make_rigidbody(a,all_models["205gti_bbox"]))
end
--[[
function make_ghost()
	local model,k,best,hist=all_models["205gti"],1,{},{}
	-- listen to track event
	on_new_lap=function(t,best_t)
		-- new best?
		if t<=best_t then
			best,hist=hist,{}
		end
		-- restart replay
		k=1
	end
	
	return add(actors,{
		pos=v_zero(),
		q=make_q(v_up,0),
		draw=function(self)
		 if self.m then
				draw_model(model,self.m,self.pos,true)
			end
		end,
		update=function(self)
			if time_t%2==0 then
				-- capture current pos
				serialize(plyr.pos,hist)
				serialize(plyr.q,hist)
				-- replay best track
				local n=#best
				if n>0 then
					k=deserialize(best,k,self.pos)
					k=deserialize(best,k,self.q)
					self.m=m_from_q(self.q)
					-- loop
					if(k>n) k=1
				end
			end
			return true
		end
	})
end
]]

-- note: limited to a single actor per tile
local all_ground_actors={
	-- checkpoint
	{spr=35,hit_part="chkpt"},
	-- cone
	{spr=53,hit_part="cone",w=8},
	-- tree/random props
	{rnd={spr={96,108,108}},hit_part="tree"},
	-- lil'people
	{rnd={spr={66,68,98,33}},hit_part="angel",hit_t=150},
}
function make_ground_actor(i,j,kind)
	local x,z,idx=shl(i+rnd(),ground_shift),shl(j+rnd(),ground_shift),safe_index(i,j)
	local a=clone(all_ground_actors[kind],{
		pos={x,get_altitude_and_n({x,0,z})+0.5,z},
		idx=idx,
		draw=ssprt
	})
	ground_actors[idx]=a
	return a
end

-- creates a collision solver for:
-- body
-- normal
-- body contact point (world position)
-- penetration
function make_contact_solver(a,p,n,d)
	local padot=a:pt_velocity(p)
	local vrel=v_dot(n,padot)
	-- resting condition?
	if(d<k_small and vrel>-k_small_v) return
	local nimpulse=0
	local ra=make_v(a.pos,p)
	local racn=make_v_cross(ra,n)

	local nm=a.mass_inv
	nm+=v_dot(racn,m_x_v(a.i_inv,racn))
	nm=1/nm
	
	-- baumgarte
	local bias=-k_bias*max(d+k_slop)/time_dt

	-- restitution bias
	local va=v_clone(a.v)
	v_add(va,make_v_cross(a.omega,ra))
	local dv=-v_dot(va,n)
	-- todo:find out unit??
	if dv<-1 then
		bias-=a.hardness*dv
	end
	
	-- contact solver
	return function()
		local dv,n=v_clone(a.v),v_clone(n)
		v_add(dv,make_v_cross(a.omega,ra))

		local lambda=-nm*(v_dot(dv,n)+bias)
	
		local tempn,nimpulse=nimpulse,max(nimpulse+lambda)
		lambda=nimpulse-tempn
		
		-- impulse too small?
		if(lambda<k_small) return false
	-- correct linear velocity
		v_scale(n,lambda)
		v_add(a.v,n,a.mass_inv)
		-- correct angular velocity
		v_add(
			a.omega,
			m_x_v(
				a.i_inv,
				make_v_cross(ra,n)
			))
		return true
	end
end

-- rigid body extension for a given actor
-- bounding box
function make_rigidbody(a,bbox)
	local force,torque=v_zero(),v_zero()
	-- compute inertia tensor
	local size=v_sqr(get_modelsize(bbox))
	local ibody=make_m(size[2]+size[3],size[1]+size[3],size[1]+size[2])
	m_scale(ibody,a.mass/12)
	
	-- invert 
	local ibody_inv=m_inv(ibody)
	-- 
	local g={0,-12*a.mass,0}
	local rb={
		i_inv=make_m(),
		v=v_zero(),
		omega=v_zero(),
		mass_inv=1/a.mass,
		-- world velocity
		pt_velocity=function(self,p)
			p=make_v_cross(self.omega,make_v(self.pos,p))
			v_add(p,self.v)
			return p
		end,
		incident_face=function(self,rn)
			rn=v_clone(rn)
			-- world to local
			m_inv_x_v(self.m,rn)
			local dmin,fmin,nmin=32000
			for _,f in pairs(bbox.f) do
				local n=bbox.n[f.ni]
				local d=v_dot(rn,n)
				if d<dmin then
					dmin,fmin,nmin=d,f,n
				end
			end
			return fmin,nmin
		end,
			-- register a force
		add_force=function(self,f,p)
			v_add(force,f,a.mass)
			v_add(torque,make_v_cross(make_v(self.pos,p),f))
		end,
		add_impulse=function(self,f,p)
		 
			v_add(self.v,f,self.mass_inv)
			v_add(self.omega,m_x_v(self.i_inv,make_v_cross(make_v(self.pos,p),f)))
		end,
		-- apply forces & torque for iteration
		prepare=function(self,dt)
			-- add gravity
			v_add(force,g)
		
			-- inverse inertia tensor
			self.i_inv=m_x_m(m_x_m(self.m,ibody_inv),m_transpose(self.m))
	
			-- velocity
			v_add(self.v,force,self.mass_inv*dt)
	
			-- angular velocity
			v_add(self.omega,m_x_v(self.i_inv,torque),dt)
			
			-- friction
			v_scale(self.v,1/(1+dt*0.4))
			v_scale(self.omega,1/(1+dt*0.6))
		end,
		integrate=function(self,dt)
			v_add(self.pos,self.v,dt)
			q_dydt(self.q,self.omega,dt)
			self.m=m_from_q(self.q)
			-- clear forces
			force,torque=v_zero(),v_zero()
		end,
		update_contacts=function(self,contacts)
			-- ground contacts against incident face
			local f=self:incident_face(v_up)
			for _,vi in pairs(f.vi) do
				local v=bbox.v[vi]
				-- to world space
				local p=self:pt_world(v)
				local h,n=get_altitude_and_n(p,true)
				local depth=h-p[2]
				if depth>k_small then
					depth=v_dot(n,{0,depth,0})
					-- deep enough?
					if depth>-k_small then
						local ct=make_contact_solver(self,p,n,depth)
						if ct then
							add(contacts,ct)
							-- record contact time
							v.contact_t=time_t
							v.n=n
						end
					end
				end
			end
			
			-- hit ground actors?
			for _,a in pairs(active_ground_actors) do
				local v=plyr:is_colliding(a.pos)
				if v then
					-- make it fly a bit!
					v[2]+=2+rnd(3)
					make_part(a.hit_part,a.pos,v)
					-- shake car
					self:add_force({0,96+rnd(32),0},a.pos)
					-- penalty time?
					if(a.hit_t) track:penalty(a.hit_t)
					-- kill actor
					ground_actors[a.idx]=nil
				end
			end
		end,
		-- is rigid body on ground?
		up_ratio=function(self)
			local r,up=0,m_up(self.m)
			for i=1,4 do
				local v=bbox.v[i]
				if v.contact_t and time_t-v.contact_t<5 then
					-- contact quality
					r+=max(v_dot(up,v.n))
				end
			end
			return r
		end,
		-- return if point p is colliding with rigidbody bbox
		-- returns relative velocity of impact
		is_colliding=function(self,p)
			if(sqr_dist(self.pos,p)>12) return
			-- world to obj space
			p=make_v(self.pos,p)
			m_inv_x_v(self.m,p)
			for i=1,#bbox.f do
				local n=bbox.n[i]
				-- front facing: no collision
				if(v_dot(n,p)>=bbox.cp[i]) return
			end
			-- relative velocity
			p=make_v_cross(self.omega,p)
			v_add(p,self.v)
			return p
		end
	}
	
	-- register rigid bodies
	return add(physic_actors,clone(rb,a))
end

-- physic world
function world:update()
	local contacts={}
	for _,a in pairs(physic_actors) do
		-- collect contacts
		a:update_contacts(contacts)
		a:prepare(time_dt)
	end
	-- solve contacts
	for _,c in pairs(contacts) do
		-- multiple iterations
		-- required to fix deep contacts
		for i=1,5 do
			if(c()==false) break
		end
	end
	
	-- move bodies
	for _,a in pairs(physic_actors) do
		a:integrate(time_dt)
	end
end

-- track
-- best lap time to beat
function make_track(best_t,segments)
	-- "close" track
	add(segments,segments[#segments])
	-- reset segment time
	foreach(segments,function(v)
		v.best_t,v.dt=32000,0
	end)
	-- active index
	local checkpoint,checkpoint_t=0,0
	local free_t=150
	-- lap_time
	-- remaining time before game over (+ some buffer time)
	local lap_t,remaining_t=0,best_t+free_t
	return {	
		get_best_t=function()
			return best_t
		end,
		get_startpos=function(self)
			return segments[1].pos
		end,
		get_dir=function(self,pos)
			local v=make_v(pos,segments[checkpoint%#segments+1].pos)
		 	return (atan2(v[1],v[3])+1)%1,sqrt(v_dot(v,v))
		end,
		-- time penalty
		penalty=function(self,t)
			lap_t+=t
		end,
		update=function(self)
			remaining_t-=1
			if remaining_t==0 then
				next_state(gameover_state)
				return
			end
			checkpoint_t+=1
			lap_t+=1
			local p=segments[checkpoint%#segments+1]
			if sqr_dist(plyr.pos,p.pos)<64 then
				checkpoint+=1
				if checkpoint%#segments==0 then
					checkpoint=0
					best_t=min(lap_t,best_t)
					on_new_lap(lap_t,best_t)
					-- next time 
					lap_t,remaining_t=0,best_t+free_t
				else
				 -- don't reset timer for start checkpoint
					if checkpoint_t<p.best_t then
						-- record delta time
						p.dt=p.best_t-checkpoint_t
						-- record segment duration
						p.best_t=checkpoint_t
					end
					checkpoint_t=0
				end
			end
		end,
		draw=function(self)
			-- lap time
			printb("⧗"..time_tostr(lap_t),2,2,7)
			-- best time
			printb("★"..time_tostr(best_t),2,8,10)
	
			-- count down when below 10s
			if remaining_t<=300 then
			 pal(7,8)
			 -- bip
			 if(remaining_t%30==0) sfx(1)
			end
			sprint(padding(flr(remaining_t/30)),60,2,21,2)
			pal()
			
			local angle,dist=self:get_dir(plyr.pos)
			spr(116+flr(8*angle),60,108)
			printb(flr(dist).."m",64-6,118,7)
		end
	}
end

-- camera
function make_cam(focal)
	-- camera rotation
	local cc,ss=1,0
	local dist=shl(8,ground_shift)
	return {
		pos={0,6*hscale,0},
		lookat={0,0,-7*16},
		track=function(self,pos,angle,d)
			dist=d
			self.pos=v_clone(pos)
			self.lookat=v_clone(pos)
			cc,ss=cos(angle),-sin(angle)
			v_add(self.pos,{0,dist*ss,dist*cc})
		end,
		project=function(self,v)
			-- pitch 
			local x,y,z=v[1]-self.lookat[1],-self.lookat[2],v[3]-self.lookat[3]
			z,y=cc*z+ss*y,-ss*z+cc*y

			local xe,ye,ze=x,y,z-dist

			local w=-focal/ze
  			return 64+xe*w,64-(v[2]+ye)*w,ze,w
		end
	}
end

-- particles
function update_part(self)
	if(self.t<time_t) return false
	-- gravity
	v_add(self.v,v_grav,self.g_scale)
	-- update pos
	local p=self.pos
	v_add(p,self.v,0.1)
	-- ground collision
	local h=get_altitude_and_n(p)
	if p[2]<h then
		p[2]=h
		-- todo: proper reflection vector
		v_scale(self.v,0.8)
	end
	
	-- force damping
	v_scale(self.v,self.dv or 0.9)
	
	-- animation frame
	self.frame+=self.df
	
	return true
end

function draw_part(self)
	local n,s=#self.frames
	if self.kind==0 then
		s=self.frames[flr(self.frame*(n-1))+1]
	elseif self.kind==1 then
		s=self.frames[flr((self.frame*(n-1))%n)+1]
	elseif self.kind==3 then
	end
	ssprt(self,s)
end

function draw_part_shadow(self)
	local x,y,z,w=cam:project({self.pos[1],get_altitude_and_n(self.pos),self.pos[3]})
	-- behind camera
	if(z>=0) return
	
	spr(37,x-4,y)
end

all_parts={
	hit={
	  	rnd={
    	    dly={
                8,
                12
            }
     	},
     	g_scale=0,
     	kind=3
				},
    smoke={
        frames={
            64,
            80,
            81,
            65
        },
        rnd={
            dly={
                8,
                12
            },
            g_scale={
                -0.03,
                -0.05
            }
        },
        kind=0,
        w=8
    },
    fire={
        frames={22,38,54},
        rnd={
			         dly={3,6}
        },
        g_scale=0,
        kind=0,
        w=8
    },
    chkpt={
        draw_shadow=draw_part_shadow,
        frames={3},
        rnd={
            dly={
                30,
                60
            }
        },
        kind=1
    },
    tree={
        draw_shadow=draw_part_shadow,
        frames={
            70
        },
        rnd={
            dly={
                24,
                32
            }
        },
        kind=1,
        w=8
    },
    angel={
        draw_shadow=draw_part_shadow,
        frames={
            110
        },
        rnd={
            dly={
                30,
                60
            }
        },
        kind=1,
        w=16,
        g_scale=-0.3
    },
    cone={
     	draw_shadow=draw_part_shadow,
        frames={
            100,
            101,
            102,
            103,
            104,
            105,
            106,
            107
        },
        rnd={
            dly={
                70,
                90
            }
        },
        kind=1,
        w=8
    }
}

function make_part(part,p,v)
	local pt=add(parts,clone(all_parts[part],{pos=v_clone(p),v=v and v_clone(v) or v_zero(),frame=0,draw=draw_part}))
	pt.t,pt.update=time_t+pt.dly,pt.update or update_part
	pt.df=1/pt.dly
	if(pt.sfx) sfx_v(pt.sfx,p)
	return pt
end

-- map helpers (marching codes, height map, normal map cache)
local qmap,hmap={},{}
function safe_index(i,j)
	return bor(band(i,0x7f),shl(band(j,0x7f),7))
end
function get_raw_qcode(i,j)
	return qmap[safe_index(i,j)] or 0
end
function get_height(i,j)
	return hmap[safe_index(i,j)]
end

-- return altitude & normal (optional)
function get_altitude_and_n(v,with_n)
	-- cell
	local x,z=v[1],v[3]
	local dx,dz=shr(x%ground_scale,ground_shift),shr(z%ground_scale,ground_shift)
	local i,j=flr(shr(x,ground_shift)),flr(shr(z,ground_shift))
	local h0,h1,n
	if dx>dz then
		local h=get_height(i,j)
		h0,h1=lerp(h,get_height(i+1,j),dz),lerp(h,get_height(i+1,j+1),dx)
		if with_n then
			n=make_v_cross(
				{ground_scale,get_height(i+1,j+1)-h,ground_scale},
				{ground_scale,get_height(i+1,j)-h,0})
			v_normz(n)
		end
	else
		local h=get_height(i+1,j+1)
		h0,h1=lerp(get_height(i,j),h,dz),lerp(get_height(i,j+1),h,dx)
		if with_n then
			n=make_v_cross(
				{0,get_height(i,j+1)-h,ground_scale},
				{ground_scale,get_height(i+1,j+1)-h,ground_scale})
			v_normz(n)
		end
	end
	return lerp(h0,h1,dz),n
end

-- draw actors on strip j
function draw_actors(j)
	local bucket=zbuf[band(j-1,0x7f)]
	if bucket then
		-- shadow pass
		for _,d in pairs(bucket) do
			d=d.obj
			-- draw shadow
			if (d.draw_shadow) d:draw_shadow()
		end
		for _,d in pairs(bucket) do
			d=d.obj
			d:draw()
		end
	end
end

function update_ground()
	local pos=plyr and plyr.pos or cam.lookat
	local i0,j0=flr(shr(pos[1],ground_shift)),flr(shr(pos[3],ground_shift))
	-- clear active list
	active_ground_actors={}
	for i=i0+ground_left,i0+ground_right do
		local cx=band(i,0x7f)
		for j=j0+ground_near,j0+ground_far do
			local cy=band(j,0x7f)
			local t=ground_actors[cx+shl(cy,7)]
			if t then
				add(active_ground_actors,t)
				add(drawables,t)
			end
		end
	end
end

local colors={11,4,9}
local shade=function(lvl,c)
	c=colors[c+1]
	return bor(shl(sget(max(lvl-1)+16,c),4),sget(lvl+16,c))
end

function draw_ground(self)
	local cx,cz=cam.lookat[1],cam.lookat[3]
	-- cell x/z ratio
	local dx,dz=cx%ground_scale,cz%ground_scale
	-- cell coordinates
	local nx,ny=flr(shr(cx,ground_shift)),flr(shr(cz,ground_shift))
	
	-- project anchor points
	local p,xmin={},shl(ground_left,ground_shift)-dx+cx
	-- grid depth extent
	for j=ground_near,ground_far do
	 -- project leftmost grid points
		local x,y,z,w=cam:project({xmin,0,-dz+cz+shl(j,ground_shift)})
		add(p,{x,y,z,w,ny+j})
	end
	
	-- move to 0-1 range
	dx/=ground_scale
	dz/=ground_scale
	
	local v0=p[1]
	local w0,nj=v0[4],v0[5]
	local dw0=shl(w0,ground_shift)
	for j=2,#p do
		-- offset to grid space
		local ni=nx+ground_left
		local i,di=ground_left,1
		
		local v1=p[j]
		local w1=v1[4]
		local dw1=shl(w1,ground_shift)
		local x0,x1=v0[1],v1[1]
		
		-- todo: unit for w?
		local h0,h1=get_height(ni,nj),get_height(ni,nj+1)
		local y0,y1=flr(v0[2]-w0*h0),flr(v1[2]-w1*h1)
		local q0=get_raw_qcode(ni,nj)
		while i<=ground_right do
			local q3=get_raw_qcode(ni+1,nj)
			-- detail tile?
			if i==ground_left or i>=ground_right-1 or ni%2==1 or q0!=q3 then
				di=1
			else
				di=2
				q3=get_raw_qcode(ni+2,nj)
			end
			local x2,x3=x1+di*dw1,x0+di*dw0
			local h2,h3=get_height(ni+di,nj+1),get_height(ni+di,nj)
			local y2,y3=flr(v1[2]-w1*h2),flr(v0[2]-w0*h3)

			-- in screen tile?
			if x3>0 then
				-- left/right cliping
				if i==ground_left then
					x0,y0=lerp(x0,x3,dx),lerp(y0,y3,dx)
					x1,y1=lerp(x1,x2,dx),lerp(y1,y2,dx)
				elseif i==ground_right then
					x3,y3=lerp(x0,x3,dx),lerp(y0,y3,dx)
					x2,y2=lerp(x1,x2,dx),lerp(y1,y2,dx)
				end

				-- backup values
				local xx0,yy0,xx3,yy3=x0,y0,x3,y3
				local xx1,yy1,xx2,yy2=x1,y1,x2,y2
				-- depth cliping
				if j==2 then
					x0,y0=lerp(x0,x1,dz),lerp(y0,y1,dz)
					x3,y3=lerp(x3,x2,dz),lerp(y3,y2,dz)
				elseif j==#p then
					x1,y1=lerp(x0,x1,dz),lerp(y0,y1,dz)
					x2,y2=lerp(x3,x2,dz),lerp(y3,y2,dz)
				end
				
				local c_hi,c_lo,c_dither=shr(band(0b00111000,q0),3),band(0b111,q0)

				local strip=(nj%4<2) and 0 or 1
				strip+=((ni%4<2) and 0 or 1)
				c_hi,c_lo=shade(1,c_hi),shade(1,c_lo)

				fillp(dither_pat2[strip+1])

				if band(q0,0x40)>0 then
					trifill(x0,y0,x2,y2,x1,y1,c_hi)
					trifill(x0,y0,x2,y2,x3,y3,c_lo)
				else
					trifill(x1,y1,x3,y3,x0,y0,c_lo)
					trifill(x1,y1,x3,y3,x2,y2,c_hi)
				end
				
				-- restore values (for clipping)
				x0,y0,x3,y3=xx0,yy0,xx3,yy3
				x1,y1,x2,y2=xx1,yy1,xx2,yy2
			end
					
			-- no need to go further, tile is not visible
			if(x0>127) break
			x0,y0,x1,y1=x3,y3,x2,y2
			h0,h1=h3,h2
			q0=q3

			ni+=di
			i+=di
		end
		
		fillp()
		draw_actors(nj)
		
		v0,w0,dw0=v1,w1,dw1
		nj+=1
	end
	-- last strip
	draw_actors(nj)
end

-- game states
-- transition to next state
function next_state(state)
	draw_state,update_state=state()
end

function start_state()
	-- reset arrays & counters
	time_t,actors,ground_actors,parts,active_ground_actors=0,{},{},{},{}

	-- read track
	track=make_track(9000,unpack_track())

	-- read actors
	unpack_actors()

	local pos=track:get_startpos()
	plyr=make_plyr({pos[1],pos[2]+4,pos[3]},0.6)
	plyr.slip_angles={{},{}}

	local ttl=4*30
	return 
		-- draw
		function()
			local t=flr(ttl/30)
			banner(0x12)
			local x=t==0 and 50 or 60
			sprint(t==0 and "go!" or tostr(t),x,46,21,2)	
		end,
		-- update
		function()
		 if(ttl%30==0) sfx(ttl<=30 and 2 or 1)
			ttl-=1
			if(ttl<0) next_state(play_state)
		end
end

function play_state()
	return
		function()
			draw_hud()
		end,
		function()
			if plyr then
				track:update()
				plyr:control()
			end
		end
end

function gameover_state()
	local ttl=900
	return 
		-- draw
		function()
			banner(0x82)
			sprint("game over",35,46,21,2)
			rectfill(0,59,127,65,2)
			print("best time:"..time_tostr(track:get_best_t()),36,60,10)

			print("press ❎/🅾️ to continue",24,110,ttl%2==0 and 7 or 11)
		end,
		-- update
		function()
			ttl-=1
			if btnp(4) or btnp(5) or ttl<0 then
				next_state(start_state)
			end
		end
end

function _update()
	time_t+=1

	-- basic state mgt
	update_state()
	
	zbuf_clear()
	
 	-- update active ground objects	
	update_ground()
	
	-- physic update
	world:update()
	
	-- game logic update
	zbuf_filter(actors)
	zbuf_filter(parts)
	
	if plyr then
		-- update cam
		local lookat=v_clone(plyr.pos)
		v_add(lookat,m_fwd(plyr.m),3)
		-- keep altitude
		lookat[2]=plyr.pos[2]+2
		cam:track(lookat,0.15,mid(sqrt(v_dot(plyr.v,plyr.v)),8,15))
	end
end

function padding(n)
	n=tostr(flr(min(n,99)))
	return sub("00",1,2-#n)..n
end

function time_tostr(t)
	if(t==32000) return "--"
	-- frames per sec
	local s=padding(flr(t/30)%60).."''"..padding(flr(10*t/3)%100)
	-- more than a minute?
	if(t>1800) s=padding(flr(t/1800)).."'"..s
	return s
end

-- print bold
function printb(s,x,y,c)
	print(s,x,y+1,1)
	print(s,x,y,c)
end

function banner(c,t)
	local x=48*smoothstep(t and 1-t or 0)
	rectfill(x,44,127-x,59,c)
	fillp(0xa5a5)
	rectfill(x,45,127-x,58,c)
	fillp()
end

function draw_gauge()
	circ(16,111,16,7)
	local i,di,c=0.75,-0.1,7
	while i>0 do
		pset(16+13.5*cos(i),111+13.5*sin(i),c)
		if(i<0.2) di=-0.025 c=8
		i+=di
	end
	local rpm=1-plyr.rpm_ratio
	rpm*=0.75
	color(8)
	line(16,111,15+10*cos(rpm),111+10*sin(rpm))
	circfill(16,111,3)
end

function draw_hud()
	camera(0,-1)
	memset(0x5f00,0x1,16)
	draw_gauge()
	camera()
	pal()
	draw_gauge()
	
	track:draw()
end

function draw_curve(x,y,points,scale)
	rectfill(x,y,x+32,y+32,0)
	line(x,y+16,x+32,y+16,1)
	for i=0,31 do
		local p=points[(time_t-i)%32]
		if p then
			p/=scale
			line(x+i,y+16,x+i,y+16*(1-p),abs(p)>1 and 8 or 7)
		end
	end
end

function draw_tireforce(self,offset)
	local pos=self:pt_world(offset)
	local relv=self:pt_velocity(pos)
	draw_vector(self,pos,relv)
end

function draw_vector(self,p,v,c)
	v=m_x_v(self.m,v)
	v_add(v,p)
	local x0,y0=cam:project(p)
	local x1,y1=cam:project(v)
	line(x0,y0,x1,y1,c)
end

function _draw()
	cls(0)

	zbuf_sort()
	draw_ground()
	
	draw_state()
	
	draw_curve(0,48,plyr.slip_angles[1],48)
	draw_curve(0,76,plyr.slip_angles[2],48)
	
		

	--rectfill(0,0,127,8,8)
	--print("mem:"..stat(0).." cpu:"..stat(1).."("..stat(7)..")",2,2,7)
 
 --print(plyr.acc,2,18,7)
 
end

function _init()
	sfx(0,3,0)
	-- q binary layout
	-- 0b01000000: rle bit
	-- 0b01000000: q code
	-- 0b00111000: hi color
	-- 0b00000111: lo color
		
	-- read models from gfx/map data
	unpack_models()
	
	-- unpack curves
	sa_curve,sr_curve=unpack_bytes(),unpack_bytes()

	-- unpack map
	local i=0
	unpack_rle(function(v)
		qmap[i]=v
		i+=1
		return i<0x4000
	end)
	i=0
	local tmp_hmap={}
	unpack_rle(function(v)
		tmp_hmap[i],tmp_hmap[i+1]=shr(band(0xf0,v),4),band(0xf,v)
		-- lower heightmap
		tmp_hmap[i]/=3
		tmp_hmap[i+1]/=3
		i+=2
		return i<0x1000
	end)
	-- linear lerp to 128x128
	for j=0,62 do
		for i=0,62 do
			local idx=i+64*j
			local h0,h1,h2,h3=tmp_hmap[idx],tmp_hmap[idx+64],tmp_hmap[idx+65],tmp_hmap[idx+1]
			idx=2*i+256*j
			hmap[idx]=h0
			hmap[idx+1]=(h0+h3)/2
			hmap[idx+128]=(h0+h1)/2
			hmap[idx+129]=(h0+h1+h2+h3)/4
		end
	end
		
	cam=make_cam(96)
		
	-- read track
	-- ghost=make_ghost()

	-- init state machine
	next_state(start_state)
end

-->8
-- trifill
-- by @p01
function p01_trapeze_h(l,r,lt,rt,y0,y1)
 lt,rt=(lt-l)/(y1-y0),(rt-r)/(y1-y0)
 if(y0<0)l,r,y0=l-y0*lt,r-y0*rt,0 
	for y0=y0,min(y1,128) do
  rectfill(l,y0,r,y0)
  l+=lt
  r+=rt
 end
end
function p01_trapeze_w(t,b,tt,bt,x0,x1)
 tt,bt=(tt-t)/(x1-x0),(bt-b)/(x1-x0)
 if(x0<0)t,b,x0=t-x0*tt,b-x0*bt,0 
 for x0=x0,min(x1,128) do
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
-- unpack models & data
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
			local f={ni=i,vi={},uv={}}
			-- vertex indices
			for i=1,unpack_int() do
				add(f.vi,unpack_int())
			end
			-- uv coords (if any)
			for i=1,unpack_int() do
				add(f.uv,{56+unpack_int(),unpack_int()})
			end
			-- center point
			f.center={unpack_float(scale),unpack_float(scale),unpack_float(scale)}
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
			add(model.cp,v_dot(n,model.v[f.vi[1]]))
		end
	
		-- merge with existing model
		all_models[name]=clone(model,all_models[name])
	end
end
function unpack_rle(decode)
	local read=true
	while read do
		-- peek data
		local k,q=unpack_int()
		-- rle or value?
		if band(0x80,k)>0 then
		-- run length
			k,q=band(0x7f,k),unpack_int()
		else
			k,q=1,k
		end
		for j=1,k do
			read=decode(q)
		end
	end
end
-- unpack int array
local mem_track
function unpack_track()
	if mem_track then
		mem=mem_track
	else
		mem_track=mem
	end
	local track={}
	for k=1,unpack_int() do
	 -- +1 shift to center track marker
	 local pos={shl(unpack_int()+1,ground_shift+1),0,shl(unpack_int()+1,ground_shift+1)}
		pos[2]=get_altitude_and_n(pos)
		add(track,{pos=pos})
	end
	return track
end
-- unpack int array
function unpack_actors()
	for k=1,unpack_int() do
		make_ground_actor(2*unpack_int(),2*unpack_int(),unpack_int())	 
	end
end
function unpack_bytes()
	local bytes={}
	for k=1,unpack_int() do
		add(bytes,unpack_int())
	end
	return bytes
end
-->8
-- trifilltex
-- 
function trapezefill(l,dl,r,dr,start,finish)
	-- layout:x y u v
	local l,dl={l[1],l[3],l[4],r[1],r[3],r[4]},{dl[1],dl[3],dl[4],dr[1],dr[3],dr[4]}
	local dt=1/(finish-start)
	for k,v in pairs(dl) do
		dl[k]=(v-l[k])*dt
	end

	-- cliping
	if start<0 then
		for k,v in pairs(dl) do
			l[k]-=start*v
		end
		start=0
	end

	-- rasterization
	for j=start,min(finish,127) do
		local len=l[4]-l[1]
		if len>0 then
			local u0,v0=l[2],l[3]
			local du,dv=(l[5]-u0)/len,(l[6]-v0)/len
			for i=l[1],l[4] do
				local c=sget(u0,v0)
				if(c!=11) pset(i,j,c)
				u0+=du
				v0+=dv
			end
  end 
		for k,v in pairs(dl) do
			l[k]+=v
		end
	end
end
function tritex(v0,v1,v2)
	local x0,x1,x2=v0[1],v1[1],v2[1]
	local y0,y1,y2=v0[2],v1[2],v2[2]
	if(y1<y0)v0,v1,x0,x1,y0,y1=v1,v0,x1,x0,y1,y0
	if(y2<y0)v0,v2,x0,x2,y0,y2=v2,v0,x2,x0,y2,y0
	if(y2<y1)v1,v2,x1,x2,y1,y2=v2,v1,x2,x1,y2,y1

	-- mid point
	local v02,mt={},1/(y2-y0)*(y1-y0)
	for k,v in pairs(v0) do
		v02[k]=v+(v2[k]-v)*mt
	end
	if(x1>v02[1])v1,v02=v02,v1

	-- note: no need to x/y optimize as we are drawing per pixel
	-- upper trapeze
	trapezefill(v0,v1,v0,v02,y0,y1)
	-- lower trapeze
	trapezefill(v1,v2,v02,v2,y1,y2)
end
-->8
-- sprite print
local bprint_chars=" ▒0123456789-abcdefghijklmnopqrstuvwxyz!"
local chars2mem={}
for i=1,#bprint_chars do
	local c=sub(bprint_chars,i,i)
	cls(0)
	print(c,0,0,7)
	local mem=0x4300+shl(i-1,5)
	for y=0,7 do
		memcpy(mem+4*y,0x6000+shl(y,6),4)
	end
	chars2mem[c]=mem
end
	
function sprint(txt,x,y,...)
	palt(0,false)
	palt(14,true)
	do_sprint(txt,x,y+1,...)
	-- pal(7,7)
	-- do_sprint(txt,x,y,...)
	pal()
end

function do_sprint(txt,x,y,s,w)
	for i=1,#txt do
		local mem=chars2mem[sub(txt,i,i)]
		local xmax=0
		for j=0,5 do
			local mask=peek4(mem)
			for k=0,7 do
				if band(mask,0x.000f)!=0 then
					-- glyph support
					if(k>xmax) xmax=k
					spr(s,x+k*w,y+j*w)
				end
				mask=shr(mask,4)
			end
			mem+=4
		end
		--next char
		x+=w*(xmax+2)
	end
end

__gfx__
000000000001510000000000eeeeeeeeeeeeeeee9aa7900007777000bb88856599777777777777777777777777777777777777788756566bb000000000000000
0000000001dc770001000000eeeeeeeeeeeeeeee9aa7900070000700b888856599766666777777777711666667666677777777788756666bb000000000000000
0070070012e7770052000000eeeeee7eee56eeee9aa7900070700700b8888565657666667777777777116666677777666666667aa756776bb000000000000000
000770003333b70013000000eeee7777ee66eeee0979000070770700b8888565657666667777777777116666677777777777776aa756776bb000000000000000
000770002497a70024000000eee777766eeeeeee0090000070000700bb0555656586666677777777771166666777777887667765a756776bb000000000000000
0070070015d7670015000000ee7777565eeeeeee9aa7900007777000bb0555656c866666777777777711666665777788877766656756556bb000000000000000
000000001567670054000000ee57655556eeeeee0000000000000000bb505565cc866666777777777711666665777780877777656756666bb000000000000000
000000001556670067000000eee6565655eeeeee0000000000000000bb055565cc866666777777777711666665777787877777656756556bb000000000000000
1ca9b3452288ee0028000000eee56565566eeeee77eeeeeeeeeeeeeebb505565ca766666777777777711666665777788877777656756666bb000000000000000
00000000499ffa0054000000eeee55556e56eeee77eeeeeeeeeeeeeebb056565aa766666777777777711666665777778877777656756556bb000000000000000
4950000099aaa7009a000000eeeee566ee6eeeeeeeeeeeeeeee898eebb566565a1766666777777777711666665777777777777765756666bb000000000000000
0000000055533b003b000000eeeeeeeeeeeeeeeeeeeeeeeeee8a79eebb50656511766666777777777711666665777777778877567756556bb000000000000000
7a9420001dcc7c001c000000eeeeeee5e56eeeeeeeeeeeeeee8979eebb56656511766666777777777711666665777778888877656756666bb000000000000000
7e882000115dd6001d000000eeeeeeeee6eeeeeeeeeeeeeeeee898eebb05656518766666777777777711666665777788111177656756556bb000000000000000
777770001122ee002e000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeebb5055658876666677777777771166666577778111aa77656756666bb000000000000000
76d5d00022eeff00ef000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeebb05556585788666777777777711666665777781aacc77656756556bb000000000000000
00000000eeeeeeeeeeeeeeeeeeeee777777eeeee01010100eeeeeeeebb50556565788866777777777711666665777781acc777656756666bb000000000000000
00000000eeeeeeeeeeeeeeeeeeeee777777eeeee10101010eeeeeeeebb05556565780866777777777711666665777781ac7766656756556bb000000000000000
009bb000eeeeeeeeeeeeeeeeeeeee777777eeeee01010100eeeeeeeebb05556565787866777777777711666667777781a7667765a756776bb000000000000000
004bbbb0eeeeeeeeeeeeeeeeeeeee665566eeeee00000000eee9aeeeb8888565657888667777777777116666677777777777776aa756776bb000000000000000
004bbb00eeeee9aaa9eeeeeeeeeee665566eeeee00000000eee89eeeb8888565657886667777777777116666677777666666667aa756776bb000000000000000
00400000eeee9aaaaa9eeeeeeeeee556655eeeee00000000eeeeeeeeb888856599766666777777777711666667666677777777788756666bb000000000000000
00400000eeee99aaa99eeeeeeeeee556655eeeee00000000eeeeeeeebb88856599777777777777777777777777777777777777788756566bb000000000000000
00111110eeeee40404eeeeeeeeeee665566eeeee00000000eeeeeeeebbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb000000000000000
00000000eeeeeefffeeeeeeeeeeee665566eeeeeeee2eeeeeeeeeeeebbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb000000000000000
00000000eeeee88888eeeeeeeeeee556655eeeeeee282eeeeeeeeeeebbbbbbbb7777777777777777777777777bbbbbbbbbbbbbbbbbbbbbbbb000000000000000
00988000eeeef8fff8feeeeeeeeee556655eeeeee27872eeeeeee8eebbbbbbb777777666666676666666666667bbbbbbbbbbbbbbbbbbbbbbb000000000000000
00488880eeeef28f82feeeeeeeeee665566eeeeee28782eeeee9eeeebbbbbb777887666666667666666666666676bbbbbbbbbbbbbbbbbbbbb000000000000000
00488800eeeee28882eeeeeeeeee16655661eeee2688862eeeeeeeeebbbbb7778aa86666666676677777666666676bbbbbbbbbbbbbbbbbbbb000000000000000
00400000eeeee88888eeeeeeeeee15566551eeee2877782eeeee7eeebbbb77778aa866666666766766666666666676bbbbbbbbbbbbbbbbbbb000000000000000
00400000eeee1182811eeeeeeeee15566551eeee1288821eeeeeeeeebbb777777887666666667667777766666666676bbbbbbbbbbbbbbbbbb000000000000000
00111110eeeee11111eeeeeeeeee11111111eeeee11111eeeeeeeeeebb77777777776666666676666666666666666776bbbbbbbbbbbbbbbbb000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee4eeeeeeb77777777777777777777777777777777777777777777777777bbbbbb000000000000000
ee9994eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee777888888888888888777777777777777777888888888888888777bbb000000000000000
e999994eee99eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee33eeee778888111111111111177775575555555577711111111111888877777000000000000000
e999994eee94eeeeeeeeee777eeeeeeeeeeeeeeeeeeeeeeee3bb3ee4558881111aaaaaaaaaaa77755575755757777aaaaaaaa111188887888000000000000000
e499944eeeeeeeeeeeeee49994eeeeeeeeeeee888eeeeeeee33b3eee86688111aaaccccc6656677575757575557777ccccccaa111188878a8000000000000000
e444444eeeeee4eeeeeeef111feeeeeeeeeee82228eeeeeeee33ee4e86660055500ccccc66566777777777777777777cccccca00555007787000000000000000
ee4444eeeeeeeeeeeeeee15051eeeeeeeeeee48884eeeeeeeeeeeeee556055555550ccccc66566777777777777777777ccccc055555550777000000000000000
eeeeeeeeeeeeeeeeeeeeef070feeeeeeeeeeef040feeeeeeeeeeeeee6605555555550cccc66566777878777787877777cccc0555555555055000000000000000
eeeeeeeeeeeeeeeeeeeeef505feeeeeeeeeeeefffeeeeeee000000006655555755555cccccccccc778888888878777777ccc5555575555566000000000000000
ee9994eeee994eeeeeeee20000eeeeeeeeeee28882eeeeee000000006655556665555cccccccccc777777777777777777ccc5555666555566000000000000000
e999994ee99994eeeeeee24442eeeeeeeeeeff282ffeeeee000000006655576667555ccccccccccc777777777777777777cc5557666755565000000000000000
e999444ee49944eeeeeeee252eeeeeeeeeeeff161ffeeeee00000000b655556665555ccccccccccc111111111111111111cc5555666555566000000000000000
e4944eeeee444eeeeeeeee888eeeeeeeeeeeeeccceeeeeee00000000bb55555755555cccccccccccc111111111111111111c5555575555566000000000000000
e444eeeeeeeeeeeeeeeeee8e8eeeeeeeeeeeeefefeeeeeee00000000bbb555555555055555555555555555555555555555550555555555bbb000000000000000
ee44ee94eeeee4eeeeee1151511eeeeeeeee1151511eeeee00000000bbbb5555555bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb5555555bbbb000000000000000
eeeeee44eeeeeeeeeeeee11111eeeeeeeeeee11111eeeeee00000000bbbbb55555bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb55555bbbbb000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee8eeeeeeeee8eeeeeeeeeeee228eeeee222eeeeee822eeeee682eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee686eeee6e888eee286eeeee28786eee28882eeee67882ee688782ee8e6eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee878eee288776ee287886eee8878eeee87778eeee88782e8878782eee88868eeeeeeeeeeeeeeeeeeeeeeeeaaaeeeeee
eeeeeeeeeeeeeeeeeeeeee999eeeeeeee68886ee28788eee2878788e2278786ee68886eee678872ee688782ee687872eeeeeeeeeebbeeeeeeeeeeeaeeeaeeeee
eeeeeeeeeeeeeeeeeeeee49994eeeeeee87778ee28876eee287886eee86888eeee878eeee88868eeeee682eeeee87882eeeeeeee3bbbeeeeeeeeeeeaaaeeeeee
eeeeeeeeeeeeeeeeeeeee44444eeeeeee28882eee282eeeee286eeeeeeee6e8eee686eeeee6eeeeeeeeeeeeeee68782eeeeeebb533bbbeeeeeeeeeeeeeeeeeee
eeeeeeeebbeeeeeeeeeee40404eeeeeeee222eeeeeeeeeeeeeeeeeeeeeeeeeeeeee8eeeeeeeeeeeeeeeeeeeeeeee22eeeeee3b9b53b93eeeeeeeeee777eeeeee
eeeeeebbbb3eeeeeeeeeee444eeeeeee0000000000000000000000000000000000000000000000000000000000000000eeee33bbb333eeeeeeeeee77777eeeee
eeeeebb38bb3eeeeeeeee12221eeeeee0000700000000000000070000000000000070000000007000077700000700000eeeee333335eeeeeeeeeee76667eeeee
eeeebbbbbb333eeeeeee4412144eeeee0777770000777700000777000077770000777770007077700077700007770700eeeeee33e5eeeeeeeeeeee65656eeeee
eee3b8bbb3233eeeeeee4436344eeeee0777777000077700007777700077700007777770007777100077700001777700eeeeeee4e4eeeeeeeeee776666677eee
eee33bb33335eeeeeeeeeebbbeeeeeee0777771000777700001777100077770001777770007771000777770000177700eeeeeeee4eeeeeeeeeeee6666666eeee
eeee333355511eeeeeeeeebebeeeeeee0111710007771700000777000071777000171110007777000177710000777700eeeee11141111eeeeeeeee66666eeeee
eee115551111eeeeeeee11c1c11eeeee0000100001710100000777000010171000010000001111000017100000111100eeeeee11111eeeeeeeeeee66666eeeee
eeee1111eeeeeeeeeeeee11111eeeeee0000000000100000000111000000010000000000000000000001000000000000eeeeeeeeeeeeeeeeeeeeee6e6e6eeeee
206040207021f141100156f67456d874b9f674b9d87456f69956d899b9f699b9d89956f6fb5698fbb9f6fbb998fbb6b9e559b9e5b6b97859b978d04010206050
4000030002720272035607e74030402010400071a071a00000000874e7406080c0a04012001271e271e20008cab84030708040400003720372020002b907e740
90a0c0b0409300e200e271937108fbc7405060a09040720372029322930356cad7408070b0c0407202720393039322b9cad7401090b03040b1b282b282d2b1d2
0838f640d0e001f04011001171917191000837b940d02040e0401100a000a071117108354940e0408001409091000272020291891749408060f0014012711200
9100917108094940d0f06020409091029172020002861749d0060808080806080a380a080808080a0608080a0808080608080a0808b9f6e9c80808994926c808
b040207021f14110d0d0a1321080b9f684b9f6fb56f6fb56f684b6b9b459b9b4b6b94b59b94b6040405070300086285840208060100089285840307080200008
ab584040302010000838f64050608070000808b9401060504000089458600648080a48080878f9080608080a0808280608e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7
e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7e7b7875727f6c696663606d5a5754515e4b4845424f3c393633303d2a2724212f1d1d1c1c1b1b1a1a191918181717161
6161515141413131212111110101f0f0f0e0e0d0d0c0c0b0b0a0a0909080807070706060505040403030202010100000000876768696a6b6c6d6e6f6f6071727
3747576777878787877777676757575747473737272717171717e6c6a686664626f5d5b59575553515e4c4a48464442404f3d3c3b3a39373635343331303f2e2
d2c2a2928272624232221202e1d1d1c1c1c1c1c1c1c1c1b1b1b1b1b1b1b1b1b1a1a1a1a1a1a1a1a1a1919191919191919191ff00ff00ff00ff00ff00ff00ff00
4a00800a9084dd00802a90847d00805890110a211590845d00805890112a2128902d00803890117a2128906900802990847a00803890118a2128905900804990
841a00805890119821a0a990a4682128902900803890112921159084f900805890119821a0b990115821a0901019008038901149212890e900809011b821a058
90102900144890115821a09010f8008038901179212890e9002890b821a05890103900803890115821a09010f800803890118921159084d90028906821a05890
107900803890115821a09010f800803890115821a00990a43821159084c900289068211558907900803890115821a09010f800803890115821a0299048212890
84b900289078211548906900803890115821a09010f8008090117821a09010e8001428904821389084a9001490a47821153890844900803890115821a09010f8
008090117821a0901009001490a43821153890b900143890a45821153890842900803890115821a09010d800803890115821a038901029001490a43821152890
c900143890a45821153890840900803890115821a09010d800803890115821a0389010490028904821159084c900143890a47821153890848800805890115821
a0389010d800803890115821a0389010590028905821159084c900143890a47821153890846800805890115821a0389010d800803890115821a0389010690014
90a458212890f900143890a4582115a890119821a0389010d8008090117821a09010a9001490a4482128900a00143890a45821158890119821a0389010d80080
90117821a09010c9002890a438211590840a00143890a46921a0389010d8008090117821a09010d9003890a43821159084f9008048905921a0389010d8008090
117821a09010e9001438904821289084b900807890e821a0789010f8008090115821a03890100a001428904821389084990080789011e8217890100900289058
21a03890102a001490a438211538902900807890115921489010390028904821a090106a001490a4382115289019008078901169211538908429008090114821
28908a002890482128900900809011f821a08890a45821153890840900809011582128908a0028904821289009002890f821a0a890a4582115389084e8008028
905821a090108a00289048212890090028908821a07890106800143890a47821159084c8008038904821a090109a00289048212890090028907821a078901088
00143890a478212890c8003890113821a028909a008090113821a02890090028902821a05890100900143890a468212890c8002890113821a038908a00809011
3821a0389009002890282158901019008038901168212890b80080901148213890107a00802890482138901009002890282128901019008038901198212890a8
0080901158212890107a00803890482128901019002890282128901900803890119821a09010a80028905821a090107a00803890113821a09010290028902821
289009008090119821a0389010b80028904821a090105900805990113821a090103900289028212890f8008090119821a0389010c80028904821289059008059
9011482128904900289028212890e8008090117821a0389010f800289048212890e8008078901199212890490028902821159084c8008090117821a038901009
00289048212890d800807890119921a09010490028903821159084a8008090115821a03890103900289048212890a80080389011f821a0199010590028904821
289098008090115821a03890104900289048212890a800389011f821a0199010690028904821289088008090115821a03890105900289048212890a800289011
9821a07890107a0028904821289078008090115821a03890106900289048212890a80028909821a07890108a0028904821289068008090115821a03890107900
289048212890a80028904821a0589010fa00289048212890680028905821a03890108900289048212890a800289048215890100b002890482128906800289048
21a03890109900289048212890a800289048212890103b0028904821289068002890482115389084990028904821289098008090113821a028904b0028904821
2890680028907821159084890028904821289088008090113821a038904b0028904821289068001490a478211590847900289048212890880028904821389010
4b002890482128907800143890a458211538908449002890482128908800289048212890105b002890482128908800143890a458211538908439002890482128
9088002890482128906b002890482128909800143890a4582115389084290028904821289088002890482128906b00289048212890a800143890a45821153890
8419002890482115908478002890482128906b00289048212890d8001490a4782115908409002890582115908468002890482128906b00289048212890e80014
90a47821159084f8001490a45821289068002890482128906b00289048212890f800143890a4582115389084d8001490a44821289068002890482128906b0028
904821289084f800143890a4582115389084d80028904821289068002890482128906b0028904821589084d800143890a4782115589084880028904821289068
002890482128906b002890482115589084d800143890a4782115589084780028904821289068002890482128906b00289098211578908498001490a4b8211538
9084480028904821289068002890482128906b001490a498211578908498001490a4b8211538908428008028904821289068002890482128907b001490a4f821
155890845800145890a498211568904821289068002890482128908b001490a4f821155890845800145890a498211558904821289068002890482128909b0014
7890a4b821155890845800145890a49821152890482128906800289048212890ab00147890a4b82115589084480080005890a4982115114821289068001490a4
38211590840c00145890a4b82115589000104800143890a4c821289078001490a438211590840c00145890a4b821155890845800143890a4b821289088002890
482128905c00145890a4b8211590847800145890a46821289088002890482128906c00145890a4b8211590847800145890a44821a02890880028904821289084
ac00145890a478211538908498001488908800289048217990846b00145890a4782115389084980014689010880028904821157990848b00145890a458211538
908489002890b921159890840b00145890a458211538908479002890c921159890844b001490a47821159084690028905a211590844b001490a4782115908459
001490a45a2128905b00143890a45821159084590014b990a4a82128906b00143890a458212890690014b990a4982128907b00143890a4482128901b00145890
a4482128908b00143890482128902b00145890482128909b00142890482128905b00142890482128909b00802890482128906b002890482128908b0080389048
2128906b002890482128908b003890113821a090105b00809011482128908b002890113821a090105b00809011582128908b002890482128906b0028905821a0
90108b002890482128906b0028904821a090108b008090113821a028906b002890482128902a00807990113821a038906b002890482128901a00807990114821
3890106b00289048212890e90080389011b9212890107b00289048212890d90080389011c92128907b0080901148212890890080589011f92128906b00809011
58212890790080589011f921a090106b0028905821a090106900809011b821a0999010390080a89084890028904821a0901079002890b821a0999010390080c8
9084690080901148212890890028906821a05890109a0080589011882115b89084a800809011582128908900289068215890109a0080589011a82115b8908488
008028905821a090108900289068212890107a0080589011a9211538908448008038904821a0901099001490a458212890846a0080589011c921153890843800
3890113821a02890b9001490a448213890842a0080389011b821a08890a4d8211538908428002890113821a03890c9002890a438211538901a0080389011b821
a0a890a4d821153890848090114821389010c9003890a438211528900a00803890117821a05890108800149890a47821152890115821289010d9001438904821
159084e900803890117821a0589010a800149890a4782115115821a09010f9001428905821157890844900803890117821a03890108900143890a4a821a09010
1a001490a45821157890842900803890117821a0389010a900143890a4982128903a001490a4b821154990117821a03890100a00143890a4682128904a001490
a4b821152990117821a03890102a00143890a44821a090105a001490a42a21a03890106a00148890107a001490a40a21a03890108a00146890109a00147890a4
8921a0389010bd00147890a46921a03890103e00148990107e0014699010ff00ff00ff00ff00ff00ff00ff00ff00ff006d001048774643547776667776287767
1628007877274066287706503877571328002302382241342138004268772662287766247038774602c80010226058772662487776387723f800205877266248
__map__
77837761910047776446664004668377827764860012332185000221820002422002228200224782777775208400011257777520840034418a00048277776386003783774183000345738b002577754186000383774183001567748b000134632086000157827741830015776283000234218600233187000157777520830015
7772830047777520850001122087002333218400267760820002837741860026648700208500025777208200028377632084000467762085000186000366628300248477648400267776208a00222000228300046684777620830026777620830002228400046664850046857776208300268277840004664083002677762083
0015867776208300077776208300157762830026777620830015867776840026777620830015776283002777762082000137777684777284002677762082000137776283004777748200225582776224466777628400267664830055827740820002677772820001378277208200466640830022246220820001377774820026
8377628300157774830002228300046664208400157740000484776283000574208800267776208400157482002784774083000240850002228200277776208400156282002784774087000222004666400067777620830011376082002784774083000282000267664267776424677776830024557720820006837773108300
372000478277746782776782777683002682774082000483777184005760028b77208200046555768200268377718300015772068a77762083000111354000278377508300015762278a7776222085001000678377718300137762268a7776266486000282777604758300266640268b77777620850005827772004620820002
2200048b77777620850015827762000220850004555789777776208500378277408600222022111384776447837777764420830005827774860004666482000124677776402683778377508300678277608600267776208200026777740026827746837771820002837720860026777683000282776200046664268377758200
078377870026777620820006827762820022202784773111578277748700078277830026827760840027847775558477860022468277408200278277648400078a774084000484777482004783777082002246827767877740830024678477762000847774000466778377766447847740820004877740028477740026827783
777420024783778300278777740284777400278277837762830022644083002788772267837770004782778377628900278877206782776482002782778277764088000227887720677776208200268277827742890004678377666467777402837720820004667777748a000357837722200244200267777683000244267774
8500022200108200158377208400026777728300466677827720840046664631820001837720850046664082000283778277208300026782777530820047827760850002228300068377827720830002678377518200068277748a00278377777484000267837773820002837772208800268377764285004666446664820002
678377648800268277224666408400022200222083004783777620222022208300268277224777628c0006847724666766648200046683778277728c0002847746837776822226847782777484002242208322144312678777768366847782777202220004677767667767827776678f77827762577740269477666783777667
5682777446947762248377724784776667897721478777764024837727070911051f06170c0512051706210d2315261a2a17300b3106330a38123a19391e3625332e3533383731392930272828261f27182f1637163a1134072b08220f1e161f1e202514200c1a101516133a0509010a09012006011b08010311010714010523
01082001182b011b2a010c0f01100d010f1101131001183301233201243501333a01323601372a013a2801272a01292601261701291a013711013c11012127011e2301171102181302161502141602101802111b020a0d020d10020e0c02120f021410021011020b24020d21021a35021c380226310226340231050233050235
05023507023607023709023109022e0902320a02330b02340b02000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000100031b340146400e7700c070100700c070100700c070100700c070100700c070100700c070100700c070100700d070100700c070100700c070100700c070100700c070100700c070100700c070100700c070
000900001e7501e7001d7001d7001c7001b7001a70018700167001570013700117000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000500002075020750207402073020730207202071020710127001d7001d700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200040e3500e6500e3500e6500c350076500435001650013500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
