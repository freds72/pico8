pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- globals
local time_t,time_dt=0,1/30
local actors,ground_actors,parts,v_light,cam,plyr,active_ground_actors={},{},{},{0,1,0}
local track,ghost

local physic_actors={}
-- physic thresholds
local k_small=0.001
local k_small_v=0.01
local k_depth=0.05
-- baumgarte
local k_bias=0.2
local k_slop=0.05

-- world units
local ground_shift,hscale=1,4
local ground_scale=2^ground_shift
local ground_left,ground_right,ground_far,ground_near=-7,7,5,-7
local v_grav={0,-1,0}
local world={}

local good_side,bad_side,any_side,no_side=0x1,0x2,0x0,0x3

function nop() return true end

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
		local d=v_dot(n,cam_pos)
		if d>=model.cp[i] then
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
	fillp(0xa5a5.f)
	-- v_light dir in object space
	local l=v_clone(v_light)
	m_inv_x_v(m,l)
	
	-- faces
	local p={}
	for i=1,#model.f do
		local f,n=model.f[i],model.n[i]
		-- viz calculation
		if f.double_sided or v_dot(n,l)<0 then
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

function bezier(t,b,c)
 local ts,tc=t*t,t*t*t
 return b+c*(-3.1*tc*ts+2.85*ts*ts+9.9*tc-17.1*ts+7.85*t)
end

local out_cubic=function(t)
	return bezier(mid(t,0,1),0,1)*0.9
end

function ssprt(self,s)
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
	local brake,turn,traction,rpm,max_rpm=0,0,0,0,32

	local add_tireforce=function(self,offset,right,fwd,brake,rpm)
		-- force to world
		right=m_x_v(self.m,right)
		fwd=m_x_v(self.m,fwd)
		
		-- application point (world space)
		local pos,slide=v_clone(self.pos),false
		v_add(pos,m_fwd(self.m),offset)

		-- point velocity
		local relv=self:pt_velocity(pos)
		local relv_len=v_dot(relv,relv)
		-- slip angle
		local sa=0
		plyr.slip_angles[rpm and 2 or 1]=0
		if relv_len>k_small then
			-- 
			sa=-v_dot(right,relv)
			-- limiting factor (normalized unit)
			local t=acos(abs(sa)/sqrt(relv_len))
			plyr.slip_angles[rpm and 2 or 1]=t
			sa*=out_cubic(t)
		end

		-- long. slip
		relv_len=v_dot(fwd,relv)
			-- convert rpm to rps
		local sr=(brake*(rpm or relv_len)-relv_len)
		if abs(relv_len)>k_small then
			sr/=abs(relv_len)
		end
		plyr.slip_ratio[rpm and 2 or 1]=sr
		if sr>0.8 then
			slide=true
		end
		
		sr*=out_cubic(abs(sr))
	
		--sr=mid(sr,-0.25,0.25)

		-- limit overall enveloppe
		--[[
		local scale=sa*sa+sr*sr
		if scale>2 then
			scale=smoothstep(scale/2)
			sa*=scale
			sr*=scale
		end
  ]]
  

		-- impulse factors
		sa*=self.traction_ratio*plyr.mass
		if abs(sa)>k_small then
			v_scale(right,sa)
			self:add_impulse(right,pos)
		end
		
		sr*=32*self.traction_ratio
		if abs(sr)>k_small then
			v_scale(fwd,sr)
			self:add_force(fwd,pos)
		end
	
		-- smoke only for rear wheels	
		if rpm and slide then
			pos=v_clone(pos)
			v_add(pos,m_right(self.m),rnd(2)-1)
			--add(pos,v_up)
			make_part("smoke",pos)
		end
	end

	local a={
		mass=32,
		hardness=0.02,
		traction_ratio=0,
		pos=v_clone(p),
		q=q,
		-- init orientation
		m=m_from_q(q),
		draw=function(self)
			draw_model(model,self.m,self.pos)
		end,
		draw_shadow=function(self)
			draw_model_shadow(model,self.m,self.pos)
		end,
		control=function(self)
			local angle,z=0,0
			if(btn(0)) angle=1
			if(btn(1)) angle=-1

		--[[
			if(btn(2)) z=-1
			if(btn(3)) z=1
			plyr.pos[1]-=turn/4
			plyr.pos[3]+=z/4
		]]
		
			turn+=angle
			-- brake (full lock:0)
			if btn(3) then
				brake=max(brake-0.1)
				rpm=max(rpm-0.9)
			else
				brake=1
			end
			-- accelerate
			if btn(2) then
				rpm=min(rpm+1.933,max_rpm)
			else
				rpm=max(rpm-0.3)
			end

			-- steering angle
			angle=0.25+0.05*turn
			-- debug
			self.angle=angle
			
			-- front wheels
			local c,s=cos(angle),sin(angle)
			add_tireforce(self,1,{-s,0,c},{c,0,s},brake)
			-- rear wheels
			add_tireforce(self,-1.2,v_right,v_fwd,brake,rpm)

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
			turn*=0.7
			
			self.rpm_ratio=rpm/max_rpm

			-- sound
			local speed=rpm*(0.8+0.2*rnd())
			local sspd = speed*2
			if (sspd>=1) sspd=speed*1.2
			if (sspd>=1) sspd=speed*0.7
			if (sspd>=1) sspd=speed*0.49
			sspd=sspd-flr(sspd)+speed/6
			poke(0x3200, sspd*2)
			poke(0x3202, sspd*8)

			return true
		end
	}
	return add(actors,make_rigidbody(a,all_models["205gti_bbox"]))
end

function make_ghost()
	local model=all_models["205gti"]
	local k,best,hist,score=1,{},{},0
	-- listen to track event
	track.on_new_lap=function(t,best_t)
		-- new best?
		if t<=best_t then
			best,hist=hist,{}
		else
			-- ghost wins
			score+=1
		end
		-- restart replay
		k=1
	end
	
	return add(actors,{
		pos=v_zero(),
		q=make_q(v_up,0),
		get_score=function(self)
			return score
		end,
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
		-- obj to world space
		pt_toworld=function(self,p)
			p=m_x_v(self.m,p)
			v_add(p,self.pos)
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
			v_scale(self.omega,1/(1+dt*0.4))
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
				local p=self:pt_toworld(v)
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
		-- return if p is colliding with rigidbody bbox
		is_colliding=function(self,p)
			if(sqr_dist(self.pos,p)>9) return
			-- to self space
			p=make_v(self.pos,p)
			m_inv_x_v(self.m,p)
			for i=1,#bbox.f do
				local n=bbox.n[i]
				-- front facing?
				local d=v_dot(n,p)
				if(d>=bbox.cp[i]) return
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
-- best lap time (max 90s)
function make_track(best_t,segments)
	-- "close" track
	add(segments,segments[#segments])
	-- reset segment time
	foreach(segments,function(v)
		v.best_t,v.dt=32000,0
	end)
	-- active index
	local checkpoint,checkpoint_t=0,0
 -- lap_time
	local lap_t,remaining_t=0,best_t
	return {	
	 -- lap callback
		on_new_lap=nop,
		get_startpos=function(self)
			return segments[1].pos
		end,
		get_dir=function(self,pos)
			local v=make_v(pos,segments[checkpoint%#segments+1].pos)
		 local angle=atan2(v[1],v[3])
		 angle=(angle+1)%1
		 return angle,sqrt(v_dot(v,v))
		end,
		-- time penalty
		penalty=function(self,t)
			lap_t+=t
		end,
		update=function(self)
			checkpoint_t+=1
			lap_t+=1
			remaining_t-=1
			local p=segments[checkpoint%#segments+1]
			if sqr_dist(plyr.pos,p.pos)<64 then
				checkpoint+=1
				if checkpoint%#segments==0 then
					checkpoint=0
					best_t=min(lap_t,best_t)
					self.on_new_lap(lap_t,best_t)
					-- next time 
					lap_t,remaining_t=0,best_t
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
		printb("lap time",2,2,7)
		printb(time_tostr(lap_t),3,8,6)
		printb("best time",127,2,10,-1)
		printb(time_tostr(best_t),126,8,6,-1)
		
		if remaining_t<900 then
			print(time_tostr(remaining_t),64,32,9)
		end
 
		local angle,dist=self:get_dir(plyr.pos)
			spr(116+flr(8*angle),60,2)
			print(flr(dist).."m",64-6,11,7)
		end
	}
end

-- camera
function make_cam(focal)
	-- camera rotation
	local cc,ss=1,0
	return {
		pos={0,6*hscale,0},
		lookat={0,0,-7*16},
		dist=shl(8,ground_shift),
		track=function(self,pos,angle)
			self.pos=v_clone(pos)
			self.lookat=v_clone(pos)
			cc,ss=cos(angle),-sin(angle)
			v_add(self.pos,{0,self.dist*ss,self.dist*cc})
		end,
		project=function(self,v)
			local x,y,z=v[1]-self.lookat[1],-self.lookat[2],v[3]-self.lookat[3]
			z,y=cc*z+ss*y,-ss*z+cc*y

		local xe,ye,ze=x,y,z-self.dist

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
    chkpt={
        draw_shadow=draw_part_shadow,
        frames={
            3
        },
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

function draw_ground(self)
	local colors={11,4,9}
	local shade=function(lvl,c)
		c=colors[c+1]
		return bor(shl(sget(max(lvl-1)+16,c),4),sget(lvl+16,c))
	end

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

function _update()
	time_t+=1

	zbuf_clear()
	
	if plyr then
		plyr:control()
	end

 -- update active ground objects	
	update_ground()
	
	-- physic update
	world:update()
	
	-- game logic update
	zbuf_filter(actors)
	zbuf_filter(parts)
	
	if plyr then
		-- do not track dead player
		if not plyr.disabled then
			-- update cam
			local lookat=v_clone(plyr.pos)
			v_add(lookat,m_fwd(plyr.m),3)
			-- keep altitude
			lookat[2]=plyr.pos[2]+2
			cam.dist=mid(sqrt(v_dot(plyr.v,plyr.v)),8,15)
			cam:track(lookat,0.15)
		end
	end
	
	track:update()
end

local padding_mask="00"
function padding(n)
	n=tostr(flr(min(n,99)))
	return sub(padding_mask,1,2-#n)..n
end

function time_tostr(t)
 if(t==32000) return "--"
 -- frames per sec
 local s=padding(flr(t/30)%60).."''"..padding(flr(10*t/3)%100)
 -- more than a minute?
 if(t>1800) s=padding(flr(t/1800)).."'"..s
	return s
end

-- print box
function printb(s,x,y,c,rev)
	local len=4*#s+1
	-- righ justified?
	if(rev) x-=len
	rectfill(x,y,x+len,y+6,c)
	print(s,x+1,y+1,0)
	-- shade
	line(x,y+7,x+len,y+7,1)
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

function draw_curve(s,x,y,t,curve)
	t=min(abs(t),1)
	local h=y+24
	rectfill(x,y,x+32,h,1)
	print(s,x+1,y+1,7)
	for i=0,32 do
		pset(x+i,h-16*curve(i/32),13)
	end
	line(x+32*t,y+6,x+32*t,h,8)
end

function draw_wheel(x,y,angle,col)
	rectfill(x,y,x+16,y+16,0)
	local c,s=cos(angle),-sin(angle)
	line(x+8+8*c,y+8-8*s,x+8-8*c,y+8+8*s,col)
end

function _draw()
	cls(0)

	zbuf_sort()
	draw_ground()
	
	draw_hud()	

	draw_curve("f.sa",1,20,plyr.slip_angles[1],out_cubic)
	draw_curve("r.sa",1,46,plyr.slip_angles[2],out_cubic)

	draw_curve("f.sr",94,20,plyr.slip_ratio[1],out_cubic)
	draw_curve("r.sr",94,46,plyr.slip_ratio[2],out_cubic)
	
	if plyr.angle then
		draw_wheel(1,72,plyr.angle,8)
		draw_wheel(1,72,plyr.slip_angles[1],2)
	end
	
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
		tmp_hmap[i]/=4
		tmp_hmap[i+1]/=4
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
 -- time to beat
	track=make_track(900,unpack_track())
	-- read actors
	unpack_actors()
	
	local pos=track:get_startpos()
	plyr=make_plyr({pos[1],pos[2]+4,pos[3]},0.6)
	plyr.slip_angles={0,0}
	plyr.slip_ratio={0,0}
	
	ghost=make_ghost()
	
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
function unpack_track()
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

	-- upper trapeze
	trapezefill(v0,v1,v0,v02,y0,y1)
	-- lower trapeze
	trapezefill(v1,v2,v02,v2,y1,y2)
end
__gfx__
1ca9b3450001510000000000eeeeeeeeeeeeeeee0000000000000000bb88856599777777777777777777777777777777777777788756566bb000000000000000
0000000001dc770001000000eeeeeeeeeeeeeeee0000000000000000b888856599766666777777777711666667666677777777788756666bb000000000000000
4950000012e7770052000000eeeeee7eee56eeee0000000000000000b8888565657666667777777777116666677777666666667aa756776bb000000000000000
000000003333b70013000000eeee7777ee66eeee0000000000000000b8888565657666667777777777116666677777777777776aa756776bb000000000000000
000000002497a70024000000eee777766eeeeeee0000000000000000bb0555656586666677777777771166666777777887667765a756776bb000000000000000
0000000015d7670015000000ee7777565eeeeeee0000000000000000bb0555656c866666777777777711666665777788877766656756556bb000000000000000
000000001567670054000000ee57655556eeeeee0000000000000000bb505565cc866666777777777711666665777780877777656756666bb000000000000000
000000001556670067000000eee6565655eeeeee0000000000000000bb055565cc866666777777777711666665777787877777656756556bb000000000000000
5c0000002288ee0028000000eee56565566eeeee0000000000000000bb505565ca766666777777777711666665777788877777656756666bb000000000000000
00000000499ffa0054000000eeee55556e56eeee0000000000000000bb056565aa766666777777777711666665777778877777656756556bb000000000000000
0000000099aaa7009a000000eeeee566ee6eeeee0000000000000000bb566565a1766666777777777711666665777777777777765756666bb000000000000000
0000000055533b003b000000eeeeeeeeeeeeeeee0000000000000000bb50656511766666777777777711666665777777778877567756556bb000000000000000
000000001dcc7c001c000000eeeeeee5e56eeeee0000000000000000bb56656511766666777777777711666665777778888877656756666bb000000000000000
00000000115dd6001d000000eeeeeeeee6eeeeee0000000000000000bb05656518766666777777777711666665777788111177656756556bb000000000000000
000000001122ee002e000000eeeeeeeeeeeeeeee0000000000000000bb5055658876666677777777771166666577778111aa77656756666bb000000000000000
0000000022eeff00ef000000eeeeeeeeeeeeeeee0000000000000000bb05556585788666777777777711666665777781aacc77656756556bb000000000000000
00000000eeeeeeeeeeeeeeeeeeeee777777eeeee0101010000000000bb50556565788866777777777711666665777781acc777656756666bb000000000000000
00000000eeeeeeeeeeeeeeeeeeeee777777eeeee1010101000000000bb05556565780866777777777711666665777781ac7766656756556bb000000000000000
009bb000eeeeeeeeeeeeeeeeeeeee777777eeeee0101010000000000bb05556565787866777777777711666667777781a7667765a756776bb000000000000000
004bbbb0eeeeeeeeeeeeeeeeeeeee665566eeeee0000000000000000b8888565657888667777777777116666677777777777776aa756776bb000000000000000
004bbb00eeeee9aaa9eeeeeeeeeee665566eeeee0000000000000000b8888565657886667777777777116666677777666666667aa756776bb000000000000000
00400000eeee9aaaaa9eeeeeeeeee556655eeeee0000000000000000b888856599766666777777777711666667666677777777788756666bb000000000000000
00400000eeee99aaa99eeeeeeeeee556655eeeee0000000000000000bb88856599777777777777777777777777777777777777788756566bb000000000000000
00111110eeeee40404eeeeeeeeeee665566eeeee0000000000000000bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb000000000000000
00000000eeeeeefffeeeeeeeeeeee665566eeeeeeee2eeee00000000bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb000000000000000
00000000eeeee88888eeeeeeeeeee556655eeeeeee282eee00000000bbbbbbbb7777777777777777777777777bbbbbbbbbbbbbbbbbbbbbbbb000000000000000
00988000eeeef8fff8feeeeeeeeee556655eeeeee27872ee00000000bbbbbbb777777666666676666666666667bbbbbbbbbbbbbbbbbbbbbbb000000000000000
00488880eeeef28f82feeeeeeeeee665566eeeeee28782ee00000000bbbbbb777887666666667666666666666676bbbbbbbbbbbbbbbbbbbbb000000000000000
00488800eeeee28882eeeeeeeeee16655661eeee2688862e00000000bbbbb7778aa86666666676677777666666676bbbbbbbbbbbbbbbbbbbb000000000000000
00400000eeeee88888eeeeeeeeee15566551eeee2877782e00000000bbbb77778aa866666666766766666666666676bbbbbbbbbbbbbbbbbbb000000000000000
00400000eeee1182811eeeeeeeee15566551eeee1288821e00000000bbb777777887666666667667777766666666676bbbbbbbbbbbbbbbbbb000000000000000
00111110eeeee11111eeeeeeeeee11111111eeeee11111ee00000000bb77777777776666666676666666666666666776bbbbbbbbbbbbbbbbb000000000000000
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
ab584040302010000838f64050608070000808b9401060504000089458600648080a48080878f9080608080a08082806ff00ff00ff00ff00ff00ff00ff00ff00
ff00ff00ff002b00802a9084bd00804a90847d00803890112a211590845d00803890114a2128902d00803890117a2128901d00803890118a212890ec00803890
11ba212890dc0080389011ba21a09010ac0080389011d821a04990a48821a0389010590080489084fa0080389011d821a05990117821a0389010590080689084
da00809011d821a0389010e800144890117821a03890103900803890114821159084ca002890d821a0389010f800803890117821a03890103900803890116821
2890ca002890a821a03890101900803890117821a038901019008038901198212890ca0028909821a03890101900803890117821a0389010190080389011a821
159084ba0028906821a038901039008090119821a0389010f80080389011e8211590849a00809011682138901039008090119821a0389010f800803890110921
28908a00809011782128901039008090119821a09010d80080589011392128908a0028907821a0901039008090119821a09010d80080589011492128908a0028
906821a090101900803890119821a09010b80080589011d821a02890a4682128908a002890682128901900803890119821a09010b80080589011d821a0489068
211590847a002890682128900900803890117821a0389010980080589011d821a038901014289078211590846a00289068212890f800803890117821a0389010
980080589011d821a038901028001490a4782128906a00289068212890e800803890117821a0389010780080389011d821a058901068001490a4682128906a00
289068212890d800803890117821a0389010780080389011d821a05890108800289068211590845a00289068212890c800803890117821a03890105800803890
11d821a0589010b800289078211590843a008090115821a09010b800803890118821489084480080389011d821a0589010c8001490a4782128902a0080901158
21a09010b800809011b821a89011d821a058901009001490a4682128902a00289068212890c8002890c82115889011d821a058901029002890682128902a0028
9068212890c80028900a21a038901079002890682128902a00289068212890c8002890f921a03890108900289068211590841a00289068212890c8002890c921
a0389010b900289078211590840a00289068212890c8001490a4a921a0389010c9001490a4782128900a00289068212890d80014b890a4c821a03890100a0014
90a468212890f90080901168212890e80014b890a4a821a03890101a0080289068212890e90080901178212890990014c890103a0080389068212890e9002890
7821a09010a90014a890104a003890115821a09010e90028906821a09010bc002890115821a09010f900289068212890cc002890682128900a00289068212890
cc002890682128900a0028906821159084ac00809011682128900a00289078211590848c00809011782128900a001490a478211590847c0028907821a090101a
001490a478211590846c0028906821a090103a002890a4782128906c002890682128904a003890a4682128906c002890682128904a0014389068211590844c00
8090115821a090105a0014289078211590842c008090115821a090107a001490a478212890841c002890682128909a001490a468213890840c00289068212890
aa0028906821153890fb0080901168212890aa0028907821152890eb0080901178212890aa001490a47821159084db0028907821a09010ba001490a478211590
84cb0028906821a09010da001490a478212890cb00289068212890fa001490a468212890cb002890682128900b0028906821159084ab008090115821a028900b
00289078211590848b008090115821a038900b001490a478211590846b0080289068213890101b001490a478211590844b0080389068212890103b002890a478
2128904b00389011682128904b003890a4682128904b00289011782128904b0014389068211590842b008090117821a090105b0014289078211590840b008090
117821a090107b001490a47821289084ea008090117821a090109b001490a46821389084ca008090117821a09010bb0028906821153890ba008028907821a028
90cb0028907821152890aa008038906821a03890cb001490a478211590848a00803890116821389010db001490a47821155890842a00803890117821289010fb
001490a47821155890841a003890117821a090101c001490a4b82115589084c9002890117821a090103c002890c82115589084a9008090117821a090104c0028
9019211558908449008090117821a090105c001490a419211558908429008090117821a028907c00145890a41921154990117821a038908c00145890a4192115
2990117821a0389010dc00145890a46a21a0389010fc00145890a45a213890105d00145890a40a212890107d00145890a4e921a09010dd00145890a48921a090
10fd00145890a46921a090105e00148990107e0014699010ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00
ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00cc00f90002007004c90074707747030021283354045900753877665565672866045900754877
475355212258001101f80074387747018800204424f80075387737980051775701e80076387706980062287702e80075387702980062287704e8007338770198
0061287715e80070287747a80030287737e80020287716a8001075776701d80030287703b8005334440268001011580031764701590032115800317657014900
10230158005177570149001015680051775701490030156800517767024900201568006228770149002037113101380071776702590076557502380062775701
59007467660438005177570159007335330338005177570159006202000238005177360159005001001138003166350159003001005138003165466900500200
620128004066350159004035337505280031754601680021e80020676677172800317567035800205701e800762877372800317677135800715701e800702877
4728003076771558007757f800402877472800107677264800707757c80010110040287747280030287715480028775701b80062660270287757280042287714
38005028775701a8001028771772287747001073287726380074387703b800337747742877270010742877460100304877380011980070487707001063567767
03005138770728002054048800304877032800100276771400513877012800517715880010753877013800007477150041287737380072772698003138774800
00747726002075770338007277570200022800303800107577374800007477152800211248006128774643340200523701280053771748000075671398007628
77667656547677152800107305480000754702980020757877373800515800007557780002380010283243334465771498000075470158004056014900007357
02580062570149000061772458007277130139000050774501480072775735012900004077670348006028775701290000302877145800762877139800048800
00307677175800206277570378004017880000207677376800207677371138002810117377017800001075772768002076287755352811555755287757056800
280076772668001075487755756877076800001075660468001075c877025800102800362278001075b877476800012800142800112812380030b87767026800
10280012113254764601280040575588772602880038004138776703380010115355587726a800380010762877670378007177676622b8004800617767660378
0010462202c800580063351389000a000a00c04192a091d0c0518042806141f15123c06361133292c202d2fcb0b010e0e01090904080a04070c04080b04090b0
4090c04090d040f0f04001e040010140f0114011f04021e040311140212140214140215140416140517140618140518140417140216140718140a18140f10140
32404062504062704052504062804072804082704072904042b04072b04072a04082904092a04082b04062b040d04240e05240e07240d07240d08240e08240f0
824001924001a240e0b240e09240f0b24001c240310240312240412240413240513240415240515240414240615240614240713240716240516240d02220d032
20e04220f0622001722011022021122021322031422031522041622021b21051721070612070712070912012502022502042702052702052902032c02022d020
d10120c12120a13110414110734120631120d2c020f2b02053f02083612053d02023a01003f010f2121033421092a210a2f21072924082824072a240922340b2
1340b20340c2f240c2034011813011a130e08130e05130f0213051e03071d03081c03091a130d1a130228130428130925130a25130c23130e22130f211300341
30136130039130f2c130d2e130a24230c23230228230f17230816230a1723031e23081f230a10330b12330610330b13330f14330123330521330522330d11330
224330425330e2d23023a23033823013b23053623053123073f13083d13083a13052e03032f030120130700130602130603130801130b07030e08030f0703011
5030214030715030a14030c14030f12030622030823030a25030a29030b2a030e2703060e130a0023090223070023021f230b11230b15230d1723053903083b0
3083e030a31130a33130731130a3d030a37130b39130b35130000000000000000000000000000000000000000000000000000000000000000000000000000000
__gff__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000002d0000000000000000000000000000007300
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000100021b04013040100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070
