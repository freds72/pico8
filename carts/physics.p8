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

-- global vectors
local v_gravity=vec(0,-9)

function project(v)
	return 64+v.x,64-v.y
end
-->8
-- cube shape
function make_cube(r,d)
	local c={
		kind=1, --polygon
		u=mat(),
		v={
			vec(r,r),
			vec(-r,r),
			vec(-r,-r),
			vec(r,-r)},
		-- local to world
		apply=function(self,v)
			return self.u*v+self.body.pos
		end,
		rotate=function(self,angle)
			self.u=mat:make_r(angle)
		end,
		draw_normal=function(self,v0,v1,i)
			local n,m=self.n[i],0.5*(v0+v1)
			local x0,y0=project(self:apply(m))
			local x1,y1=project(self:apply(m+4*n))
			line(x0,y0,x1,y1,8)
		end,
		draw=function(self)
			local v=self.v[#self.v]
			local x,y=project(self:apply(v))
			for i=1,#self.v do
				local v1=self.v[i]
				local x1,y1=project(self:apply(v1))
				line(x,y,x1,y1,7)
				self:draw_normal(v,v1,i)
				x,y=x1,y1
				v=v1
			end
		end,
		init=function(self,b)
		 -- calculate normals
			local v=self.v[#self.v]
			self.n={}
			for k=1,#self.v do
				local v1=self.v[k]
				local n=v-v1
				n:normz()
				add(self.n,vec(-n.y,n.x))
				v=v1
			end
			
		 -- compute mass from density
			local c=vec(0,0)
			local area=0
			local i=0
			local inv3=1/3
			
			for k=1,#self.v do
				local v1,v2=self.v[k],self.v[(k%#self.v)+1]
				local tarea=0.5*v_cross(v1,v2)
				area+=tarea
				c+=tarea*inv3*(v1+v2)
				
				local x,y=v1.x*v1.x+v2.x*v1.x+v2.x*v2.x,v1.y*v1.y+v2.y*v1.y+v2.y*v2.y
				i+=0.25*inv3*(x+y)
			end
			
			b.m=d*area
			b.im=1/b.m
			b.i=i*d
			b.ii=1/b.i
			
			self.body=b
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
		v=vec(0,0),
		force=vec(0,0),
		i=0,
		ii=0,
		m=1,
		im=1,
		sfriction=0.5,
		dfriction=0.3,
		restit=0.2,
		angularv=0,
		torque=0,
		angle=0,
		apply_forces=function(self,dt)
			if(self.im==0) return
			self.v+=(self.im*self.force+v_gravity)*dt*0.5
			self.angularv += self.torque*self.ii*dt*0.5
		end,
		apply_v=function(self,dt)
			if(self.im==0) return
			self.pos+=dt*self.v
			self.angle+=dt*self.angularv
			self.shape:rotate(angle)
			
			self:apply_forces(dt)
		end,
		reset=function(self)
			self.force=vec(0,0)
			self.torque=0
		end
	}
	shape:init(b)
	return add(bodies,b)
end
-->8
-- game loop
function _init()
	make_body(make_cube(5,1),5,5)
end

function _draw()
	cls()
	for _,b in pairs(bodies) do
		b.shape:draw()
	end
	
	rectfill(0,0,30,9,1)
	print(flr(100*stat(2)).."%",2,2,7)
end

function _update60()
	for _,b in pairs(bodies) do
		b:apply_forces(1/60)
		b:apply_v(1/60)
		b:reset()
	end
end

-->8
-- collision resolution
function leastpenetration(a,b)
	for i=1,#a.v do
		local n=a.n[i]
		local nw=a.u*n
		local but=b.u:transpose()
	end
end

function incidentface(a,b)
end

function poly2poly(m,a,b)
	m.contacts={}
	
end

__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000005577000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700055577700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000055577700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000077755500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700077755500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000007755000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
