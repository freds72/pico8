pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- math
local vec={}
vec.__index=vec
setmetatable(vec,{
	__call=function(cls,...)
		return cls:new(...)
	end
})
function vec:new(x,y)
 return setmetatable({x=x,y=y},vec)
end
function vec:__add(a)
	return vec(self.x+a.x,self.y+a.y)
end
function vec:__sub(a)
	return vec(self.x-a.x,self.y-a.y)
end
function vec:__unm()
	return vec(-self.x,-self.y)
end
function vec:__mul(a)

	-- scalar mul
	if type(self)=="table" and type(a)=="number" then
		return vec(a*self.x,a*self.y)
	elseif type(self)=="number" and type(a)=="table" then
		return vec(self*a.x,self*a.y)
	end

	assert() --use dot
end
function vec:__div(a)
	-- scalar div
	if type(self)=="table" and type(a)=="number" then
		return vec(self.x/a,self.y/a)
	elseif type(self)=="number" and type(a)=="table" then
		return vec(a.x/self,a.y/self)
	end
	assert()
end

function vec:len()
	return sqrt(self:lensqr())
end
function vec:lensqr()
	return self.x*self.x+self.y*self.y
end

function vec:normz()
	local l=self:len()
	if l>0.001 then
		self.x/=l
		self.y/=l
	end
end

function v_dot(a,b)
	return a.x*b.x+a.y*b.y	
end
function v_distsqr(a,b)
	local v=b-a
	return v_dot(v,v)
end

function v_min(a,b)
	return vec(min(a.x,b.x),min(a.y,b.y))
end
function v_max(a,b)
	return vec(max(a.x,b.x),max(a.y,b.y))
end
function v_cross(a,b)
	return a.x*b.y-a.y*b.x
end
function vec:ortho(a)
	a=a or 1
	return vec(-a*self.y,a*self.x)
end

function vec:tostr()
	return "("..self.x..","..self.y..")"
end

-- matrix class
local mat={}
mat.__index=mat
setmetatable(mat,{
	__call=function(cls,...)
		return cls:new(...)
	end
})
function mat:new()
	local m={1,0,0,1}
 return setmetatable(m,mat)
end
function mat:make_r(angle)
	local c,s=cos(angle),-sin(angle)
	local m={c,-s,s,c}
 return setmetatable(m,mat)
end

function mat:abs()
	local m={}
	for i=1,4 do
		add(m,abs(self[i]))
	end
 return setmetatable(m,mat)	
end

function mat:xaxis()
	return vec(self[1],self[3])
end
function mat:yaxis()
	return vec(self[2],self[4])
end
function mat:transpose()
	local m={self[1],self[3],self[2],self[4]}
 return setmetatable(m,mat)	
end
function mat:__mul(a)
	if a[4] then
		-- matrix
		local m={
			self[1]*a[1]+self[2]*a[3],self[1]*a[2]+self[2]*a[4],
			self[3]*a[1]+self[4]*a[3],self[3]*a[2]+self[4]*a[4]		
		}
	 return setmetatable(m,mat)	
	else 
		-- vector
		return vec(self[1]*a.x+self[2]*a.y,self[3]*a.x+self[4]*a.y)
	end
end

-- misc math
function is_gt(a,b)
	return a>=b*0.95+a*0.01
end
function sqr(a)
	return a*a
end
function lerp(a,b,t)
	return a*(1-t)+b*t
end
function rndlerp(a,b)
	return lerp(b,a,1-rnd())
end

-- global vectors
local v_gravity=vec(0,-98)
local k_dt=1/60

function project(v)
	return 64+v.x,64-v.y
end
function unproject(x,y)
	return vec(x-64,-y+64)
end


-->8
-- polygon shape
function make_polygon(v,d)
	local c={
		kind=1, --polygon
		u=mat(),
		v=v,
		-- local to world
		apply=function(self,v)
			return self.u*v+self.body.pos
		end,
		rotate=function(self,angle)
			self.u=mat:make_r(angle)
		end,
	 inside=function(self,v0)
	 	for i=1,#self.v do
	 		local v=v0-self.v[i]
	 		if(v_dot(self.n[i],v)>0) return false
	 	end
	 	return true
	 end,
		draw_normal=function(self,v0,v1,i)
			local n,m=self.n[i],0.5*(v0+v1)
			local x0,y0=project(self:apply(m))
			local x1,y1=project(self:apply(m+4*n))
			line(x0,y0,x1,y1,8)
			-- print(i,x1+2,y1,8)
		end,
		draw=function(self,c,debug)
			local v=self.v[#self.v]
			local x,y=project(self:apply(v))
			color(self.body.m==0 and 5 or c)
			for i=1,#self.v do
				local v1=self.v[i]
				local x1,y1=project(self:apply(v1))
				line(x,y,x1,y1)
				if debug then
					self:draw_normal(v,v1,i)
				end
				x,y=x1,y1
				v=v1
			end
		end,		
		draw_face=function(self,i)
			local v0,v1=self.v[i-1==0 and #self.v or i-1],self.v[i]
			local x0,y0=project(self:apply(v0))
			local x1,y1=project(self:apply(v1))
			line(x0,y0,x1,y1,9)
		end,
		init=function(self,b)
		 -- calculate normals
			local v=self.v[#self.v]
			self.n={}
			for k=1,#self.v do
				local v1=self.v[k]
				local n=v-v1
				n:normz()
				add(self.n,n:ortho())
				v=v1
			end
			
		 -- compute mass from density
			local c=vec(0,0)
			local area=0
			local i=0
			local inv3=1/3
			
			for k=1,#self.v do
			 -- triangle vertices, third vertex implied as (0, 0)
				local v1,v2=self.v[k-1==0 and #self.v or k-1],self.v[k]
				local d=v_cross(v1,v2)
				local tarea=0.5*d
				area+=tarea
				c+=tarea*inv3*(v1+v2)
				
				local x,y=v1.x*v1.x+v2.x*v1.x+v2.x*v2.x,v1.y*v1.y+v2.y*v1.y+v2.y*v2.y
				i+=0.25*inv3*d*(x+y)
			end
			-- translate vertices to centroid (make the centroid (0, 0)
			c*=1/area
			for k=1,#self.v do
				self.v[k]-=c
			end
			
			b.m=d*area
			b.im=1/b.m
			b.i=i*d
			b.ii=1/b.i
			
			self.body=b
		end,
		support=function(self,d)
			local maxp,maxv=-32000
			for i=1,#self.v do
				local v=self.v[i]
				local p=v_dot(v,d)
				if p>maxp then
					maxp,maxv=p,v
				end
			end
			return maxv
		end,
  leastpenetration=function(self,b)
 		local maxd,maxi=-32000
  	for i=1,#self.v do
  		local n,v=self.n[i],self.v[i]
  		local nw,vw=self.u*n,self:apply(v)-b.body.pos
  		local but=b.u:transpose()
  		n,v=but*nw,but*vw
  		
  		local s=b:support(-n)
  		
  		local d=v_dot(n,s-v)
  		
  		if d>maxd then
  			maxd,maxi=d,i
  		end		
  	end
  	return maxd,maxi
  end,
  incidentface=function(self,b,ni)
  	local n=self.u*self.n[ni]
  	n=b.u:transpose()*n
  	local mini,mindot=-1,32000
  	for i=1,#b.n do
  		local d=v_dot(n,b.n[i])
  		if d<mindot then
  			mindot,mini=d,i
  		end
  	end
  	return {
  		b:apply(b.v[mini]),
  		b:apply(b.v[mini-1==0 and #b.v or mini-1])}
  end
	}
	return c
end


-->8
-- body
local bodies={}
function make_body(shape,x,y)
	local b={
		shape=shape,
		pos=vec(x,y),		
		v=vec(0,0), -- velocity
		f=vec(0,0), -- force
		i=0,
		ii=0,
		m=1,
		im=1,
		sfriction=0.4,
		dfriction=0.2,
		restiutiont=0.2,
		angularv=0,
		torque=0,
		angle=0,
		-- static body
		static=function(self)
			self.i,self.ii=0,0
			self.m,self.im=0,0
		end,
		apply_force=function(f)
			self.f+=f
		end,
		apply_impulse=function(self,impulse,c)
			self.v+=self.im*impulse
			self.angularv+=mid(self.ii*v_cross(c,impulse),-1,1)
		end,
		integrate_forces=function(self,dt)
			if(self.im==0) return
			self.v+=(self.im*self.f+v_gravity)*dt*0.5
			self.angularv+=self.torque*self.ii*dt*0.5
		end,
		integrate_v=function(self,dt)
			if(self.im==0) return
			self.pos+=dt*self.v
			self.angle+=dt*self.angularv
			self.shape:rotate(self.angle)
			
			self:integrate_forces(dt)
		end,
		reset=function(self)
			self.f[1],self.f[2]=0,0
			self.torque=0
		end
	}
	shape:init(b)
	return add(bodies,b)
end
-->8
-- game loop
local time_t=0
local mousex,mousey=0,0
-- selected body
local sel_body
local show_debug=false
local manifolds={}
function _init()
	poke(0x5f2d,1)
	local r=10
	local v={
			vec(r,r),
			vec(-r,r),
			vec(-r,-r),
			vec(r,-r)}
	local b=make_body(make_polygon(v,1),15,5)
	b:static()

 v={
			vec(63,5),
			vec(-64,5),
			vec(-64,-5),
			vec(63,-5)}
	b=make_body(make_polygon(v,1),0,-30)
	b:static()
	
	v={}
	local n,r=rndlerp(3,6),rndlerp(10,15)
	local angle,da=0,1/n
	for i=1,n do
		add(v,vec(r*cos(angle),-r*sin(angle)))
		angle+=da
	end

	sel_body=make_body(make_polygon(v,1),0,35)
	sel_body.angle=rnd()
end

local cpu_stats={}
function _draw()
	cls()
	for _,b in pairs(bodies) do
		local c=sel_body==b and 9 or 7
		b.shape:draw(c)
		if show_debug then
			local x,y=project(b.pos)
			print(b.shape.isref==true and "ref" or "inc",x-8,y,1)
		end
	end
	
	-- 
	rectfill(0,0,127,9,1)
	local cpu=flr(100*stat(1))
	cpu_stats[time_t%128+1]=cpu
	for i=1,128 do
		local c,s=11,cpu_stats[(time_t+i)%128+1]
		if s then
			if(s>100) c=8 s=100
			pset(i-1,9-9*s/100,c)
		end
	end
	print(cpu.."%",2,2,7)

	if show_debug then
 	for _,m in pairs(manifolds) do
 		m:draw()
 	end
	end
	
	spr(2,mousex,mousey)
end

function _update60()
	mousex,mousey=stat(32),stat(33)
	
	-- world mouse coords
	local mw=unproject(mousex,mousey)
	
	if stat(34)==2 then
 	sel_body=nil
 	for _,b in pairs(bodies) do
 		if b.shape:inside(mw) then
 			sel_body=b
 			break
 		end
 	end
 end
 	
	if sel_body and stat(34)==1 then
		sel_body.pos=mw
		sel_body:reset()
		sel_body.v=vec(0,0)
		sel_body.angularv=0
	end
		
	if btnp(4) then
		show_debug=not show_debug
	end
	
	-- resolve collisions
	manifolds={}
	-- find contact points
	for i=1,#bodies do
		local a=bodies[i]
		for j=i+1,#bodies do
			local b=bodies[j]
			if (a.im==0 and b.im==0)==false then
				local m=make_manifold(a,b)
				if #m.contacts>0 then
					add(manifolds,m)
				end
			end
		end
	end
	
	for _,a in pairs(bodies) do
		a:integrate_forces(k_dt)
	end
	
	for _,m in pairs(manifolds) do
		m:init()
	end

	for i=1,10 do
		for _,m in pairs(manifolds) do
			m:apply_impulse()
		end
	end

	for _,a in pairs(bodies) do
		a:integrate_v(k_dt)
 end
 
	for _,m in pairs(manifolds) do
		m:fix_pos()
	end

	for _,a in pairs(bodies) do
		a:reset()
	end
	
	time_t+=1
end

-->8
-- collision resolution

function face_clip(n,c,f)
	local sp,out=0,{f[1],f[2]}
	
	local d1,d2=v_dot(n,f[1])-c,v_dot(n,f[2])-c
	if(d1<=0) out[sp+1]=f[1] sp+=1
	if(d2<=0) out[sp+1]=f[2] sp+=1
	
	if d1*d2<0 then
		local t=d1/(d1-d2)
		out[sp+1]=f[1]+t*(f[2]-f[1])
		sp+=1
	end
	
	assert(sp!=3)
	
	f[1],f[2]=out[1],out[2]
	
	return sp
end

function poly2poly(m,a,b)
	m.contacts,m.penetration={},0

	--check for a separating axis with a's face planes
	local pa,fa=a:leastpenetration(b)
	if(pa>=0) return
	
	--check for a separating axis with b's face planes
	local pb,fb=b:leastpenetration(a)
	if(pb>=0) return
	
	-- always point from a to b
	local ref,inc,i,flip
	-- determine which shape contains reference face
	if is_gt(pa,pb) then
		ref,inc,i,flip=a,b,fa,false
	else
		ref,inc,i,flip=b,a,fb,true
	end
	-- debug
	ref.isref,inc.isref=true,false
	
	local incf=ref:incidentface(inc,i)
	
	-- setup reference face vertices
	local v1,v2=ref.v[i],ref.v[i-1==0 and #ref.v or i-1]
	-- transform vertices to world space
	v1,v2=ref:apply(v1),ref:apply(v2)
	
	-- calculate reference face side normal in world space
	local sn=v2-v1
	sn:normz()
	
	local refn=vec(-sn.y,sn.x)

	-- ax + by = c
  	-- c is distance from origin
	local refc=v_dot(refn,v1)
	local negside,posside=-v_dot(sn,v1),v_dot(sn,v2)
		
	-- clip incident face to reference face side planes
	-- due to floating point error, possible to not have required points
	if(face_clip(-sn,negside,incf)<2) return
	if(face_clip(sn,posside,incf)<2) return
	
	m.n=flip==true and -refn or refn
		
	local cp=0
	local sep=v_dot(refn,incf[1])-refc
	if sep<=0 then
		cp+=1
		add(m.contacts,incf[1])
		m.penetration=-sep
	end
	
	sep=v_dot(refn,incf[2])-refc
	if sep<=0 then
		cp+=1
		add(m.contacts,incf[2])
		m.penetration+=-sep
		m.penetration/=cp
	end
end

-->8
-- manifold
-- penetration allowance
-- penetration percentage to correct
local k_slop,k_pct=0.05,0.4

function make_manifold(a,b)
	local m={
		a=a,
		b=b,
		apply_impulse=function(self)
  	-- early out and positional correct if both objects have infinite mass
  	if abs(a.im+b.im)<0.001 then
  		a.v,b.v=vec(0,0),vec(0,0)
  		return
  	end
  	
  	for _,c in pairs(self.contacts) do
  		-- calculate radii from center of mass to contact
  		local ra,rb=c-a.pos,c-b.pos
  		-- relative velocity
  		local rv=b.v+rb:ortho(b.angularv)-(a.v+ra:ortho(a.angularv))
  		
  		-- relative velocity along the normal
				local cv=v_dot(rv,self.n)
				--  do not resolve if velocities are separating
				if(cv>0) return
				
				local racn,rbcn=v_cross(ra,self.n),v_cross(rb,self.n)
				local invmass=a.im+b.im+sqr(racn)*a.ii+sqr(rbcn)*b.ii
				
				-- calculate impulse scalar
				local j=-(1+self.e)*cv
				j/=invmass
				j/=#self.contacts
				
				-- apply impulse
				local impulse=j*self.n
				a:apply_impulse(-impulse,ra)
				b:apply_impulse(impulse,rb)
				
				-- friction impulse
				rv=b.v+rb:ortho(b.angularv)-(a.v+ra:ortho(a.angularv))
				local t=rv-v_dot(rv,self.n)*self.n
				t:normz()
				
				-- j tangent magnitude
			 local jt=-v_dot(rv,t)
			 jt/=invmass	
			 jt/=#self.contacts
			 -- don't apply tiny friction impulses
			 if(abs(jt)<0.01) return
			 		
			 -- coulomb's law
			 local timpulse
			 if abs(jt)<j*self.sf then
			 	timpulse=t*jt
			 else
			 	timpulse=t*-j*self.df
 		 end
 		 -- apply friction impulse
				a:apply_impulse(-timpulse,ra)
				b:apply_impulse(timpulse,rb)
  	end
		end,
		fix_pos=function(self)
			local c=(max(self.penetration-k_slop,0)/(a.im+b.im))*k_pct*self.n
			a.pos-=a.im*c
			b.pos+=b.im*c
		end,
		draw=function(self)
			for _,cp in pairs(self.contacts) do
				local x,y=project(cp)
				circfill(x,y,2,8)
				local x1,y1=project(cp+4*self.n)
				line(x,y,x1,y1,11)
			end
		end,
		init=function(self)
			-- calculate average restitution
			self.e=min(a.restitution,b.restitution)
			--  calculate static and dynamic friction
			self.sf=sqrt(a.sfriction*b.sfriction)
			self.df=sqrt(a.dfriction*b.dfriction)
				
  	for _,c in pairs(self.contacts) do
  		local ra,rb=c-a.pos,c-b.pos
  		local rv=b.v+rb:ortho(b.angularv)-(a.v+ra:ortho(a.angularv))
  		-- determine if we should perform a resting collision or not
				-- the idea is if the only thing moving this object is gravity,
    -- then the collision should be performed without any restitution
  		if rv:lensqr()<(k_dt*v_gravity):lensqr()+0.01 then
  			self.e=0
  		end	
  	end		
		end
	}
	-- solve
	poly2poly(m,a.shape,b.shape)
	return m
end
__gfx__
00000000000000007777000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000005577007000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700055577707077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000055577707077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000077755500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700077755500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000007755000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
