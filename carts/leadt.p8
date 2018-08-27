pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
local actors={}
local particles={}
local cam
local time_t=0
local target

function make_v(a,b)
	return {b[1]-a[1],b[2]-a[2]}
end
function v_add(a,b,scale)
	scale=scale or 1
	a[1]+=scale*b[1]
	a[2]+=scale*b[2]
end
function v_clone(v)
	return {v[1],v[2]}
end
function sqr_dist(a,b)
	local dx,dy=b[1]-a[1],b[2]-a[2]
	local d=dx*dx+dy*dy
	return d<0 and 32000 or d
end
function v_dot(a,b)
	return a[1]*b[1]+a[2]*b[2]
end
function v_normz(v)
	local d=v_dot(v,v)
	if d>0 then
		d=sqrt(d)
		v[1]/=d
		v[2]/=d
	end
end
function v_scale(v,scale)
	v[1]*=scale
	v[2]*=scale
end

function make_actor(x,y)
 local angle=rnd()
	local a={
		r=4,
		pos={x,y},
		fwd={cos(angle),sin(angle)},
		acc=0.6,
		die=function(self)
			self.disabled=true
			del(actors,self)
		end,
		update=function(self)
			v_add(self.pos,self.fwd,self.acc)
			if not cam:is_viz(self.pos) then
				self:die()
			end
		end,
		draw=function(self)
			local x,y=cam:project(self.pos)
			spr(1,x-4,y-4)
		end
	}
	return add(actors,a)
end

function make_turret(x,y)
	local a={
		pos={x,y},
		fire_t=0,
		update=function(self)
			if target and not target.disabled and self.fire_t<time_t then
				make_laser(self,target)
				self.fire_t=time_t+8
			end
		end,
		draw=function(self)
			local x,y=cam:project(self.pos)
			circfill(x,y,3,9)
		end
	}
	add(actors,a)
end

local blt_acc=3
function make_laser(self,target)
	local v=v_clone(target.pos)
	local lead_t=0
  for i=1,2 do
		lead_t=sqrt(sqr_dist(v,self.pos))/blt_acc-lead_t
		-- not converging
		-- if(lead_t<0) break
		v_add(v,target.fwd,lead_t*target.acc)
	end
	v_add(v,self.pos,-1)
	v_normz(v) 
		
	make_blt(self.pos,v)
end

function blt_obj_col(self)
	for _,a in pairs(actors) do
		local r=a.r
		if r then
			r*=r
			local hit=false
			-- edge case: base or tip inside sphere
			if sqr_dist(self.pos,a.pos)<r or sqr_dist(self.prev_pos,a.pos)<r then
				hit=true
			else
				-- point to sphere
				local ps=make_v(self.pos,a.pos)
				-- projection on ray
				local t=v_dot(self.fwd,ps)
				if t>=0 and t<=self.acc then
					-- distance to sphere?
					local p=v_clone(self.fwd)
					v_scale(p,t)
					hit=sqr_dist(p,a.pos)<r
				end	
			end
			if hit then
				return true
			end
		end
	end
	return false
end

function make_blt(pos,u)
	local a={
		pos=v_clone(pos),
		fwd=u,
		acc=blt_acc,
		t=time_t+95,
		die=function(self)
			del(particles,self)
		end,
		update=function(self)
			if self.t<time_t then
				self:die()
				return
			end
			self.prev_pos=v_clone(self.pos)
			v_add(self.pos,self.fwd,self.acc)
			
			if blt_obj_col(self) or not cam:is_viz(self.pos) then
				self:die()
			end
		end,
		draw=function(self)
			local x,y=cam:project(self.pos)
			local v1=v_clone(self.pos)
			v_add(v1,self.fwd)
			local x1,y1=cam:project(v1)
			line(x,y,x1,y1,8)
		end
	}
	add(particles,a)
end

function make_cam()
	return {
		project=function(self,v)
			return 64+v[1],64-v[2]
		end,
		is_viz=function(self,v)
			return v[1]>-64 or v[1]<64 or v[2]>-64 or v[2]<-64
		end
	}
end

function forall(a,fn)
	for _,a in pairs(a) do
		a[fn](a)
	end	
end

function _update()
	time_t+=1
	forall(actors,"update")
	forall(particles,"update")
	
	if btnp(4) then
		target=make_actor(rnd(96)-48,rnd(96)-48)
	end
end

function _draw()
	cls()
	forall(actors,"draw")
	forall(particles,"draw")
	
	print(#particles,2,2,7)
end

function _init()
 cam=make_cam()
 make_turret(0,0)
end
__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000c70000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0070070000cccc000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000770000333b7300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0007700033333b730000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700011111100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000001111000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
