pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- mini-flightsim
-- by freds72

-- game globals
local time_t=0

-- register json context here
function nop() return true end

-- player
local plyr
local actors,world={},{}
-- ground constants 
local ground_shift,ground_colors,ground_level=2,{1,13,6}
-- camera
local cam
-- light direction (sun)
local light_n={-0.707,-0.707,0}

-- zbuffer (kind of)
local drawables={}
function zbuf_clear()
	drawables={}
end
function zbuf_draw()
	local objs={}
	-- steps:
	-- collect visibles faces (world space)
	-- clip (camera)
	-- clip (shadow volumes)
	-- project cam space
	-- sort
	-- render
	local faces,light_faces={},{}
	for _,d in pairs(drawables) do
		if d.model then
			collect_faces(d.model,d.pos,d.m,faces,light_faces)
		else
			-- other objects (particles, sprites...)
			local p=d.pos
			local x,y,z,w=cam:project(p[1],p[2],p[3])
			add(objs,{self=d,key=z,draw=d.draw,x=x,y=y,z=z,w=w})
		end
	end
	-- clip faces with light volumes
	for _,sf in pairs(light_faces) do
		-- clip against whole plane
		local faces1={}
		for _,f in pairs(faces) do
			-- don't self clip
			if #f.v>2 and sf.id!=f.id then
				local in_light,in_shadow={},{}
				local v0=f.v[#f.v]
				for k=1,#f.v do
					local v1=f.v[k]
					plane_ray_intersect(sf.n,sf.v[1],v0,v1,in_light,in_shadow)
					v0=v1
				end
				-- register new faces				
				add(faces1,{id=f.id,v=in_light,c=f.c,light=false})
				add(faces1,{id=f.id,v=in_shadow,c=f.c,light=true})
			else
				f.light=true
				add(faces1,f)
			end
		end
		faces=faces1

		-- clip against edges
		local pv0=sf.v[#sf.v]
		for i=1,#sf.v do
			local pv1=sf.v[i]
			local pn=make_v(pv0,pv1)
			v_normz(pn)
		 	pn=make_v_cross(pn,light_n)

			-- clip each visible face
			local faces2={}
			for _,f in pairs(faces) do
				-- don't self clip
				if sf.id!=f.id then
					local in_light,in_shadow={},{}
  					local v0=f.v[#f.v]
  					for k=1,#f.v do
  						local v1=f.v[k]
  						plane_ray_intersect(pn,pv0,v0,v1,in_light,in_shadow)
  						v0=v1
  					end
  					-- register new faces				
  					add(faces2,{id=f.id,v=in_light,c=f.c,light=true})
					add(faces2,{id=f.id,v=in_shadow,c=f.c,light=false})
				else
					f.light=true
					add(faces2,f)
				end
			end
			-- use new faces for new shadow caster edge
			faces=faces2		 
		 	pv0=pv1
			-- debug
			-- if(true) break
		end	
	
	end

	-- project in cam space
	for _,f in pairs(faces) do
		if(f.light==false) f.c=1
		if #f.v>0 then
	 		-- project into cam space
 			local z=0
	 		for i=1,#f.v do 			
 				f.v[i]=m_x_v(cam.m,make_v(cam.pos,f.v[i]))
 				z+=f.v[i][3]
 			end
 			z/=#f.v
 			f.key=z		
 			add(objs,f)
		end
	end

	-- z-sorting
	sort(objs)

	-- actual draw
	for i=1,#objs do
		local o=objs[i]
		if o.draw then
			o.self:draw(o.x,o.y,o.z,o.w)
		else
			polyfill(o.v,o.c)
		end
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

-- collect visible faces
function collect_faces(model,pos,m,out,out_light)
	-- cam pos in object space
	local cam_pos=v_clone(cam.pos)
	m_inv_x_v(m,cam_pos)

	-- light dir in object space
	local l=v_clone(light_n)
	m_inv_x_v(m,l)
	
	--
	local v_cache={} 
	for i=1,#model.f do
		local f,n=model.f[i],model.n[i]
		-- light facing?
		local cam_facing,light_facing=v_dot(n,cam_pos)>=model.cp[i],v_dot(n,l)<0
		-- viz calculation
		local vertices={}
		if cam_facing or light_facing then
			-- project vertices
			for k=1,#f.vi do
				local vi=f.vi[k]
				local v=v_cache[vi]
				if not v then
					v=m_x_v(m,model.v[vi])
					v_add(v,pos)
					v_cache[vi]=v
				end
				add(vertices,v)
			end
		end
		if cam_facing then
			-- register face
			local c=f.c
			--if(light_facing) c=sget(9,c)
			add(out,{v=vertices,c=c,id=f.id})
		end
		-- shadow caster
		if light_facing then
			add(out_light,{v=vertices,id=f.id,n=m_x_v(m,n)})
		end
	end
end

function clone(src,dst)
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
function lerparray(a,t)
	return a[mid(flr((#a-1)*t+0.5),1,#a)]
end

-- https://github.com/morgan3d/misc/tree/master/p8sort
function sort(data)
 for num_sorted=1,#data-1 do 
  local new_val=data[num_sorted+1]
  local new_val_key,i=new_val.key,num_sorted+1

  while i>1 and new_val_key>data[i-1].key do
   data[i]=data[i-1]   
   i-=1
  end
  data[i]=new_val
 end
end

-- edge cases:
-- a: -23	-584	-21
-- b: 256	-595	256
function sqr_dist(a,b)
	local dx,dy,dz=b[1]-a[1],b[2]-a[2],b[3]-a[3]
	if abs(dx)>128 or abs(dy)>128 or abs(dz)>128 then
		return 32000
	end
	local d=dx*dx+dy*dy+dz*dz 
	-- overflow?
	return d<0 and 32000 or d
end

-- world axis
local v_fwd,v_right,v_up={0,0,1},{1,0,0},{0,1,0}

function make_v(a,b)
	return {
		b[1]-a[1],
		b[2]-a[2],
		b[3]-a[3]}
end
function make_v_cross(a,b)
	local ax,ay,az=a[1],a[2],a[3]
	local bx,by,bz=b[1],b[2],b[3]
	return {ay*bz-az*by,az*bx-ax*bz,ax*by-ay*bx}
end
function v_clone(v)
	return {v[1],v[2],v[3]}
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

-- matrix functions
function m_x_v(m,v)
	local x,y,z=v[1],v[2],v[3]
	return {
		m[1]*x+m[5]*y+m[9]*z,
		m[2]*x+m[6]*y+m[10]*z,
		m[3]*x+m[7]*y+m[11]*z}
end
function m_x_xyz(m,x,y,z)		
	return
		m[1]*x+m[5]*y+m[9]*z+m[13],
		m[2]*x+m[6]*y+m[10]*z+m[14],
		m[3]*x+m[7]*y+m[11]*z+m[15]
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
-- inplace 3x3 matrix multiply invert
function m_inv_x_v(m,v)
	local x,y,z=v[1]-m[13],v[2]-m[14],v[3]-m[15]
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
-- right vector
function m_right(m)
	return {m[1],m[2],m[3]}
end

-- models
local all_models={}

function draw_actor(self,x,y,z,w)	
	-- distance culling
	draw_model(self.model,self.m,x,y,z,w)
end

-- little hack to perform in-place data updates
local draw_session_id=0
local znear,zfar=1,64
local planes_p={
		{0,0,zfar},
		{0,0,znear},
		--{0,0,0},
		--{0,0,0},
		--{0,0,0},
		--{0,0,0}
}
local planes_n={
	{0,0,1},
	{0,0,-1},
	{0.707,0,-0.707},
	{-0.707,0,-0.707},
	{0,0.707,-0.707},
	{0,-0.707,-0.707}}
function draw_model(model,m,x,y,z,w)
	draw_session_id+=1

	-- project in cam space
	local p={}
	for _,f in pairs(model.f) do
		-- edges loop
		local v={}
		for i=1,#f.vi do
			local ak=f.vi[i]
			-- edge positions
			local a=p[ak]
			-- not in cache?
			if not a then
				local pv=make_v(cam.pos,model.v[ak])
				m_x_v(cam.m,pv)
				a,p[ak]=pv,pv
			end
			add(v,a)
		end
		-- clip loop
		local out={}
		for i=1,#planes_p do
			local pp,pn=planes_p[i],planes_n[i]
			local v0=v[#v]
			for k=1,#v do
				local v1=v[k]
				plane_ray_intersect(pn,pp,v0,v1,out)
				v0=v1
			end
			-- previous clipped points
			v,out=out,{}
		end
		polyfill(v,f.c)	
	end
end

-- 
function shadow_volume(model,pos,m)
 local volume={}
	-- project in m space
	local p={}
	for _,e in pairs(model.e) do
		-- edges loop
		local v={}
		for i=1,#e do
			local ak=e[i]
			-- edge positions
			local a=p[ak]
			-- not in cache?
			if not a then
				local pv=make_v(pos,model.v[ak])
				m_inv_x_v(m,pv)
				a,p[ak]=pv,pv
			end
			add(v,a)
		end
		-- clip loop
		local out={}
		for i=1,#planes_p do
			local pp,pn=planes_p[i],planes_n[i]
			local v0=v[#v]
			for k=1,#v do
				local v1=v[k]
				plane_ray_intersect(pn,pp,v0,v1,out)
				v0=v1
			end
			-- previous clipped points
			v,out=out,{}
		end
		-- convert point back to 3d space
				
  add(volume,v)
	end
	return volume
end

function plane_ray_intersect(n,p,a,b,inside,outside)
	local pa=make_v(a,p)
	if(v_dot(pa,n)>0) add(inside,a)
		
	local r=make_v(a,b)
	local den=v_dot(r,n)
	-- no intersection
	if abs(den)>0.001 then
		local t=v_dot(pa,n)/den
		if t>0.001 and t<=1 then
			-- intersect pos
			v_scale(r,t)
			v_add(r,a)
			add(inside,r)
			if(outside) add(outside,r)
		end
	end
	if(outside and v_dot(make_v(b,p),n)<=0) add(outside,b)
end

draw_plyr=function(self,x,y,z,w)
	-- draw_model(self.model,self.m,x,y,z,w)
end

update_plyr=function(self)
	-- damping
	self.roll*=0.8
	self.pitch*=0.85
	return true
end

local all_actors={
	tree={
		model="tree",
		update=nop
	},
	ground={
		model="ground",
		update=nop
	},
	shader={
		model="plane",
		update=function(self)
			--local q=make_q(v_up,time_t/300)
			--self.q=q
			--self.m=m_from_q(q)
			self.pos[1]=2*cos(time_t/300)
			self.pos[3]=2*sin(time_t/300)
			return true
		end
	}
}

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
		end
	}
	
	-- register rigid bodies
	return add(world,clone(rb,a))
end

-- physic world
function world:update()
	local contacts={}
	for _,a in pairs(self) do
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
	for _,a in pairs(self) do
		a:integrate(time_dt)
	end
end

function make_plyr(x,y,z,angle)
	local p={
		acc=0.05,
		pos={x,y,z},
		q=make_q(v_up,angle or 0),
		roll=0,
		pitch=0,
		draw=nop,
		update=update_plyr
	}
	local m=m_from_q(p.q)
	m_set_pos(m,p)
	p.m=m
	add(actors,p)
	return p
end

function make_actor(src,p,q)
	-- instance
	local a=clone(src,{
		pos=v_clone(p),		
		q=q or make_q(v_up,0)
	})
	a.draw=a.draw or draw_actor
	a.model=all_models[src.model]
	-- init orientation
	local m=m_from_q(a.q)
	m_set_pos(m,p)
	a.m=m
	return add(actors,a)
end

function make_cam(x0,y0,focal)
	local c={
		pos={0,0,3},
		q=make_q(v_up,0),
		update=function(self)
			self.m=m_from_q(self.q)
			m_inv(self.m)
		end,
		track=function(self,pos,q)
			self.pos,q=v_clone(pos),q_clone(q)
			self.q=q
		end,
		project=function(self,x,y,z)
			-- world to view
			x-=self.pos[1]
			y-=self.pos[2]
			z-=self.pos[3]
			x,y,z=m_x_xyz(self.m,x,y,z)
			-- too close to cam plane?
			if(z<1) return nil,nil,-1,nil
			-- view to screen
	 		local w=focal/z
 			return x0+x*w,y0-y*w,z,w
		end,
		-- project cam-space points into 2d
		project2d=function(self,v)
			-- view to screen
 		local w=focal/v[3]
 		return x0+v[1]*w,y0-v[2]*w
		end
	}
	return c
end

local sky_gradient={0x77,0xc7,0xc6,0xcc}
local sky_fillp={0xffff,0xa5a5,0xa5a5,0xffff}
function draw_ground(self)

	-- draw horizon
	local zfar=-256
	local x,y=-64*zfar/64,64*zfar/64
	local farplane={
			{x,y,zfar},
			{x,-y,zfar},
			{-x,-y,zfar},
			{-x,y,zfar}}
	-- ground normal in cam space
	local n=m_x_v(cam.m,{0,1,0})

	for k=0,#sky_gradient-1 do
		-- ground location in cam space	
		local p=m_x_v(cam.m,{0,cam.pos[2]-10*k*k,0})
	
		local v0=farplane[#farplane]
		local sky={}
		for i=1,#farplane do
			local v1=farplane[i]
			plane_ray_intersect(n,p,v0,v1,sky)
			v0=v1
		end
		-- complete line?
		fillp(sky_fillp[k+1])
		polyfill(sky,sky_gradient[k+1])
	end

	-- sun
	local x,y,z,w=cam:project(cam.pos[1],cam.pos[2]+89,cam.pos[3]+20)
	if z>0 then
		circfill(x,y,7+rnd(2),0xc7)
		circfill(x,y,5,0x7a)
	end
	fillp()

 
	local cy=cam.pos[2]

	local scale=4*max(flr(cy/32+0.5),1)
	scale*=scale
	local x0,z0=cam.pos[1],cam.pos[3]
	local dx,dy=x0%scale,z0%scale
	
	for i=-4,4 do
		local ii=scale*i-dx+x0
		for j=-4,4 do
			local jj=scale*j-dy+z0
			local x,y,z,w=cam:project(ii,0,jj)
			if z>0 then
				pset(x,y,3)
			end
 		end
	end
end

-- handle player inputs
function control_plyr(self)
	
	local pitch,roll,input=0,0,false
	if(btn(0)) roll=1 input=true
	if(btn(1)) roll=-1 input=true
	if(btn(2)) pitch=-1
	if(btn(3)) pitch=1
		 		
	local boost=0
	if(btn(5)) boost=5
	if(btn(4)) boost=-5
	
	self.roll+=roll
	self.pitch+=pitch
	
	local q=make_q(m_fwd(plyr.m),self.roll/256)
	q_x_q(q,make_q(m_right(plyr.m),-self.pitch/256))
	q_x_q(q,plyr.q)
	-- avoid matrix skew
	q_normz(q)

	local m=m_from_q(q)
	local fwd=m_fwd(m)
	v_add(self.pos,fwd,boost*self.acc)

	m_set_pos(m,self.pos)
	self.m,self.q=m,q
end

-- play loop
function _update()
	time_t+=1

	zbuf_clear()
	
	control_plyr(plyr)

	--[[
 	light_n={
 		cos(time_t/300),
 		-1,
 		sin(time_t/300)
 	}
 	v_normz(light_n)
	]]

	-- update cam
	cam:track(plyr.pos,plyr.q)
	
	zbuf_filter(actors)
	
	-- must be done after update loop
	cam:update()
end

function _draw()
	cls()

 -- draw horizon
	-- todo
	
	--clip(0,0,128,32)
	draw_ground()	
	zbuf_draw()
	--clip()
	
	print(stat(1),2,2,8)
end

function _init()

	unpack_models()

	cam=make_cam(64,64,64)

	make_actor(all_actors.shader,{0,5,0})
	make_actor(all_actors.ground,{0,0,0})
	plyr=make_plyr(0,5,-10,0.15)
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
local face_id=0
function unpack_models()
	-- for all models
	for m=1,unpack_int() do
		local model,name,scale={},unpack_string(),unpack_int()
		printh("model:"..name)
		
		-- vertices
		model.v={}
		for i=1,unpack_int() do
			add(model.v,{unpack_float(scale),unpack_float(scale),unpack_float(scale)})
		end
		printh("v:"..#model.v)
		
		-- faces
		model.f={}
		for i=1,unpack_int() do
			face_id+=1
			local f={id=face_id,ni=i,vi={},c=unpack_int(),double_sided=unpack_int()==1}
			-- vertex indices
			for i=1,unpack_int() do
				add(f.vi,unpack_int())
			end
			printh("f.v:"..#f.vi)
			add(model.f,f)
		end
		printh("f:"..#model.f)
		
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
			add(model.cp,v_dot(n,model.v[f.vi[1]]))
		end			

		-- merge with existing model
		all_models[name]=clone(model,all_models[name])
	end
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

function polyfill(p,c)
	if #p>2 then
		local x0,y0=cam:project2d(p[1])
		local x1,y1=cam:project2d(p[2])
		for i=3,#p do
			local x2,y2=cam:project2d(p[i])
			trifill(x0,y0,x1,y1,x2,y2,c)
			x1,y1=x2,y2
		end
	end
end
function poly(p,c)
	if #p>1 then
		color(c)
		local x0,y0=cam:project2d(p[#p])
		for i=1,#p do
			local x1,y1=cam:project2d(p[i])
			line(x0,y0,x1,y1)
			x0,y0=x1,y1
		end
	end
end

__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000001c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000002e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000003b0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000490000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000560000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000670000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000770000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000008e0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000009a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000a70000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000bb0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000cc0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000d60000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000ef0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000f70000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
3040f1d10101308068083808c8080808a7086a08a7083808a8f8d8a88737a8876040003010203040003030205030003060407040003050201030003070408030
003080406060b98817568817b988f80888f90888165688f86021d1a10291f0a0400608060a080606080a0a080a103000401020403010080a0850b171c0910110
400a080a0a080606080606080a106000401040302010080a08