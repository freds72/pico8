pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
local actors={}
local particles={}
local cam
local time_t=0
local target
local use_lead=true

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
	if abs(dx)>128 or abs(dy)>128 then
		return 32000
	end
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
 local angle=flr(4*rnd())/4
	local a={
		r=4,
		pos={x,y},
		fwd={cos(angle),sin(angle)},
		acc=0.4,
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
				self.fire_t=time_t+45
				make_flash(self.pos)
			end
		end,
		draw=function(self)
			local x,y=cam:project(self.pos)
			spr(2,x-4,y-4)
		end
	}
	add(actors,a)
end

local blt_acc=0.8
function make_laser(self,target)
	local v=v_clone(target.pos)
	local leads={}
	if use_lead then
 	local lead_t=0
  for i=1,2 do
 		lead_t=sqrt(sqr_dist(v,self.pos))/blt_acc-lead_t
 		-- not converging
 		if(lead_t<0) break
 		v_add(v,target.fwd,lead_t*target.acc)
 		add(leads,v_clone(v))
 	end
	end
 v_add(v,self.pos,-1)
 v_normz(v) 
		
	local b=make_blt(self.pos,v)
	b.leads=leads
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

function make_flash(pos)
	local p={
		pos=v_clone(pos),
		t=24,
		update=function(self)			
			if self.t<0 then
				del(particles,self)
			end
			self.t-=1
		end,
		draw=function(self)
			local x,y=cam:project(self.pos)
			circfill(x,y,self.t/8,7)
		end
	}
	return add(particles,p)
end

function make_blt(pos,u)
	local a={
		pos=v_clone(pos),
		fwd=u,
		acc=blt_acc,
		t=time_t+180,
		die=function(self,hit)
			del(particles,self)
			if hit then
				make_flash(self.pos)
			end
		end,
		update=function(self)
			if self.t<time_t then
				self:die()
				return
			end
			self.prev_pos=v_clone(self.pos)
			v_add(self.pos,self.fwd,self.acc)
			
			local hit=blt_obj_col(self)
			if hit or not cam:is_viz(self.pos) then
				self:die(hit)
			end
		end,
		draw=function(self)
			local x,y=cam:project(self.pos)
			local v1=v_clone(self.pos)
			v_add(v1,self.fwd)
			local x1,y1=cam:project(v1)
			line(x,y,x1,y1,8)
			
			-- draw lead positions
			for i=1,#self.leads do
			 local l=self.leads[i]
				x,y=cam:project(l)
				pset(x,y,9)
				print(i,x-2,y+4,7)
			end
		end
	}
	return add(particles,a)
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

function _update60()
	time_t+=1
	forall(actors,"update")
	forall(particles,"update")
	
	if btnp(4) then
		target=make_actor(rnd(96)-48,rnd(96)-48)
	end
	if btnp(5) then
		use_lead=not use_lead
	end
end

function _draw()
	cls()
	forall(actors,"draw")
	forall(particles,"draw")
	
 local s="lead:"..(use_lead and "on" or "off")
 for i=-1,1,2 do
 	for j=-1,1,2 do
		 print(s,2+i,2+j,1)
		end
	end
	print(s,2,2,6)
end

function _init()
 cam=make_cam()
 make_turret(0,0)
end
__gfx__
00000000000000000055550000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000c70000566775000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0070070000cccc005666667500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000770000333b7305668e67500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0007700033333b735668866500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700011111105666666500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000001111000566665000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000055550000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
