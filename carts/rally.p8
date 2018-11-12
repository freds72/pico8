pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- globals
local time_t,time_dt=0,1/30
local actors,ground_actors,parts,v_light,cam,plyr,active_ground_actors={},{},{},{0,1,0}
local track,ghost

local physic_actors={}
-- physic thresoholds
local k_small=0.001
local k_small_v=0.01
local k_depth=0.05
-- baumgarte
local k_bias=0.2
local k_slop=0.05

local k_coll_none,k_coll_pen,k_coll_coll,k_coll_rest=0,1,2,4

-- world units
local ground_shift,hscale=1,4
local ground_scale=2^ground_shift
local ground_left,ground_right,ground_far,ground_near=-7,7,5,-7
local v_grav={0,-1,0}
local world={}

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
local dither_pat=json_parse'[0b1111111111111111,0b0111111111111111,0b0111111111011111,0b0101111111011111,0b0101111101011111,0b0101101101011111,0b0101101101011110,0b0101101001011110,0b0101101001011010,0b0001101001011010,0b0001101001001010,0b0000101001001010,0b0000101000001010,0b0000001000001010,0b0000001000001000,0b0000000000000000]'
local dither_pat2=json_parse'[0xffff,0xa5a5,0x0000]'

function draw_model(model,m,pos,outline)
	-- cam pos in object space
	local cam_pos=v_clone(cam.pos)
	v_add(cam_pos,pos,-1)
	m_inv_x_v(m,cam_pos)
	-- v_light dir in object space
	local l=v_clone(v_light)
	m_inv_x_v(m,l)
	
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
					-- avoid rehash
					p[vi]={x,y,z,w,0,0}
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
		p0[4],p0[5]=uv0[1],uv0[2]
		for i=2,#f.vi-1 do
			local p1,p2=p[f.vi[i]],p[f.vi[i+1]]
			if outline then
				fillp(0xf0f0.f)
				trifill(p0[1],p0[2],p1[1],p1[2],p2[1],p2[2],0x1)
				fillp()
			else
				local uv1,uv2=f.uv[i],f.uv[i+1]
				p1[4],p1[5]=uv1[1],uv1[2]
				p2[4],p2[5]=uv2[1],uv2[2]
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
	return b+c*(33*tc*ts-106*ts*ts+126*tc-67*ts+15*t)
end

local out_elastic=function(t)
	return bezier(t,0,24)
end

function add_tireforce(self,v,offset,scale,isrear)
 -- force to world
	v=m_x_v(self.m,v)
	
		-- application point (world space)
	local pos=v_clone(self.pos)
	v_add(pos,m_fwd(self.m),offset)

	local ratio,slide=scale,false
	if not ratio then
		-- point velocity
		local relv=self:pt_velocity(pos)
		-- perfect impulse
		ratio=-v_dot(v,relv)
		ratio*=self.traction_ratio
  -- impulse factors
		ratio*=plyr.mass
				
		-- sliding?
		local fwd=m_fwd(self.m)
		local slip_angle=atan2(v[1],v[3])-atan2(relv[1],relv[3])-0.25		
		plyr.slip_angles[isrear and 2 or 1]=slip_angle
		local max_traction=out_elastic(slip_angle)
		if abs(ratio)>max_traction then
			slide=true
			--ratio=mid(ratio,-max_traction,max_traction)
		end
		-- clamp
		-- ratio*=max_traction
  
		v_scale(v,ratio)
		self:add_impulse(v,pos)
	else
 	-- wheels on ground?
 	ratio*=self.traction_ratio
  v_scale(v,ratio)
 	
 	-- apply 
 	self:add_force(v,pos)
 end
 
	-- smoke only for rear wheels
	if isrear and slide then
	 pos=v_clone(pos)
	 v_add(pos,m_right(self.m),rnd(2)-1)
		--add(pos,v_up)
		make_part("smoke",pos)
	end
end

local rpm_curves={
 -- low gear: out quadratic
	[1]=function(dt)
	 return -2*dt+2
	end,
	-- hi gear: linear
	[2]=function(dt)
 	return 1
	end
}

_g.control_plyr=function(self)
	local turn,z=0,0
	if(btn(0)) turn=1
	if(btn(1)) turn=-1

 --[[
	if(btn(2)) z=-1
	if(btn(3)) z=1
	plyr.pos[1]-=turn/4
	plyr.pos[3]+=z/4
 ]]
 
	self.turn+=turn
	
	if v_dot(self.v,self.v)>k_small then
		-- steering angle
		local angle=0.25+0.05*self.turn/2.33
		add_tireforce(self,{-sin(angle),0,cos(angle)},1)
		-- rear wheels
		add_tireforce(self,v_right,-1.2,nil,true)
	end

	-- accelerate
	if btn(2) then
		self.rpm=min(self.rpm+0.1*rpm_curves[self.gear](time_dt),self.max_rpm[self.gear])
	else
		self.rpm=max(self.rpm-0.3)
	end
	add_tireforce(self,v_fwd,-1,self.rpm)

	-- brake
	if btn(3) then
		add_tireforce(self,v_fwd,-1,-8)
	end
	
	if btn(4) then
		local pos=v_clone(self.pos)
		v_add(pos,m_up(self.m),3)
		local force=v_clone(v_up)
		v_scale(force,12)
		self:add_force(force,pos)
	end
	
	if btnp(5) then
		self.gear+=1
		if(self.gear==3) self.gear=1
	end
end

_g.update_plyr=function(self)
	
	self.traction+=self:up_ratio()
	self.traction_ratio=self.traction/20
	
	-- time decay
	self.traction*=0.8
	self.turn*=0.7

 -- normalized length
	-- sound
	local speed=plyr.rpm*(0.8+0.2*rnd())
	local sspd = speed*2
	if (sspd>=1) sspd=speed*1.2
	if (sspd>=1) sspd=speed*0.7
	if (sspd>=1) sspd=speed*0.49
	sspd=sspd-flr(sspd)+speed/6
	poke(0x3200, sspd*2)
	poke(0x3202, sspd*8)

	-- hit ground actors?
	-- todo

	return true
end

_g.init_ghost=function(self) 
	local k,best,best_t,hist=1,{},32000,{}
	self.serialize_plyr=function()
		-- capture current pos
		serialize(plyr.pos,hist)
		serialize(plyr.q,hist)
	end
	self.replay_best=function()
		local n=#best
		if(n==0) return
		k=deserialize(best,k,self.pos)
		k=deserialize(best,k,self.q)
		self.m=m_from_q(self.q)
		-- loop
		if(k>n) k=1
	end
	-- listen to track event
	track.on_new_lap=function()
	 -- new best?
		if track.lap_t<best_t then
			best,best_t,hist=hist,track.lap_t,{}		 
		end
		-- restart replay
		k=1
	end
end
_g.update_ghost=function(self)
	if time_t%2==0 then
		self:serialize_plyr()
		-- replay best track
		self:replay_best()
	end
	return true
end

_g.draw_plyr_shadow=function(self)
	draw_model_shadow(self.model,self.m,self.pos)
end

_g.draw_spr_actor=function(self)
	palt(0,false)
	palt(14,true)
	local x,y,z,w=cam:project(self.pos)
	w*=3
	local sx,sy=band(self.frame*8,127),8*flr(self.frame/16)
	sspr(sx,sy,16,16,x-w/2,y-w,w,w)
	pal()
end

local all_actors=json_parse'{"plyr":{"model":"205gti","rigid_model":"205gti_bbox","hardness":0.02,"mass":32,"rpm":0,"gear":1,"max_rpm":[8,12],"turn":0,"traction":0,"traction_ratio":0,"control":"control_plyr","update":"update_plyr","draw_shadow":"draw_plyr_shadow"},"ghost":{"model":"205gti","init":"init_ghost","update":"update_ghost","outline":true},"tree":{"update":"nop","draw":"draw_spr_actor","rnd":{"frame":[37,96,66,98]}}}'

function draw_actor(self)
	draw_model(self.model,self.m,self.pos,self.outline)
end

function make_actor(src,p,angle)
	-- instance
	local a=clone(all_actors[src],{
		pos=v_clone(p),
		q=make_q(v_up,angle or 0)
	})
	a.model,a.draw=all_models[a.model],a.draw or draw_actor
	-- init orientation
	a.m=m_from_q(a.q)
	-- constructor?
	if(a.init) a:init()
	return add(actors,a)
end

-- note: limited to a single actor per tile
function make_ground_actor(src,i,j)
	local x,z=shl(i+rnd(),ground_shift),shl(j+rnd(),ground_shift)
	local a=clone(all_actors[src],{
		pos={x,0,z},
		q=make_q(v_up,0)
	})
	-- adjust pos
	a.pos[2]=get_altitude_and_n(a.pos)
	a.m=m_from_q(a.q)
	-- register
	ground_actors[i+j*128]=a
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
-- note:actor must have a 3d model
function make_rigidbody(a)
	local force,torque=v_zero(),v_zero()
	-- bounding box
	local bbox=all_models[a.rigid_model]
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
			-- register a force
		add_force=function(self,f,p)
			v_add(force,f,self.mass)
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
			local i=0
			for _,v in pairs(bbox.v) do
				-- to world space
				local p=m_x_v(self.m,v)
				v_add(p,self.pos)
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
	return add(physic_actors,clone(rb,a))
end

-- physic world
function world:check_coll()
	self.contacts={}
	for _,a in pairs(physic_actors) do
		a:update_contacts(self.contacts)
	end
end

function world:update()
	local dt=1/30
	
	-- collect contacts
	self:check_coll()

	-- 
	for _,a in pairs(physic_actors) do
		a:prepare(dt)
	end
	-- solve contacts
	for _,c in pairs(self.contacts) do
		-- multiple iterations
		-- required to fix deep contacts
		for i=1,5 do
			if(c()==false) break
		end
	end
	
	-- move bodies
	for _,a in pairs(physic_actors) do
		a:integrate(dt)
	end
end

-- track
function make_track(segments)
	-- "close" track
	add(segments,segments[#segments])
	-- reset segment time
	foreach(segments,function(v)
		v.best_t,v.dt,v.blink_t=32000,0,-1
	end)
	-- active index
	local checkpoint,checkpoint_t=0,0
	local laps=1
	return {	
	 -- best lap time
	 best_t=32000,
	 -- lap_time
		lap_t=0,
	 -- lap callback
		on_new_lap=nop,
		get_startpos=function(self)
			return segments[1].pos
		end,
		get_dir=function(self,pos)
			local v=make_v(pos,segments[checkpoint%#segments+1].pos)
		 local angle=atan2(v[1],v[3])
		 angle=(angle+1)%1
		 return flr(8*angle),sqrt(v_dot(v,v))
		end,
		is_over=function(self)
			return flr(checkpoint/#segments)>laps
		end,
		update=function(self)
 		checkpoint_t+=1
			self.lap_t+=1
			local p=segments[checkpoint%#segments+1]
			p.blink_t-=1
			if sqr_dist(plyr.pos,p.pos)<64 then
				checkpoint+=1
				if checkpoint%#segments==0 then
					checkpoint=0					
					laps+=1		
					self.best_t=min(self.lap_t,self.best_t)
					self:on_new_lap()
					self.lap_t=0
				else
				 -- don't reset timer for start checkpoint
					if checkpoint_t<p.best_t then
						-- record delta time
						p.dt=p.best_t-checkpoint_t
						-- record segment duration
						p.best_t=checkpoint_t
						p.blink_t=10
					end
					checkpoint_t=0
				end
			end
		end,
		-- debug draw
		draw=function(self)
			for i=1,#segments do
		 	local v=segments[i]
				local x,y,z,w=cam:project({v.pos[1]+4*cos(time()),get_altitude_and_n(v.pos),v.pos[3]-4*sin(time())})
	 		pset(x,y,8)
				x,y,z,w=cam:project({v.pos[1],get_altitude_and_n(v.pos),v.pos[3]})
				spr(i<=checkpoint and 32 or 48,x-4,y-4)
				
				if laps>1 and i==checkpoint and v.blink_t>0 then
					local s=time_tostr(v.dt)
					print("-"..s,x-3*#s,y-2*v.blink_t,10)
				end
			end
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
_g.update_part=function(self)
	if(self.t<time_t or self.r<0) return false
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
	v_scale(self.v,self.dv)
	
	self.r+=self.dr
	-- animation frame
	self.frame+=self.df
	
	return true
end

_g.draw_part=function(self)
	local x,y,z,w=cam:project(self.pos)
	-- behind camera
	if(z>=0) return
	
	-- simple part
	if self.kind==0 then
		local s=self.frames[flr(self.frame*(#self.frames-1)+0.5)]
		if s then
			palt(0,false)
			palt(14,true)
			local sx,sy=band(shl(s,3),127),shl(shr(s,4),3)
			w*=2
		  	sspr(sx,sy,8,8,x-w/2,y-w/2,w,w)			
			pal()
		end
	end
end

all_parts=json_parse'{"smoke":{"frames":[64,80,81,65],"r":1,"rnd":{"dly":[8,20],"g_scale":[-0.03,-0.05]},"frame":0,"dv":0.9,"dr":0,"kind":0}}'

function make_part(part,p,v)
	local pt=add(parts,clone(all_parts[part],{pos=v_clone(p),v=v and v_clone(v) or v_zero(),draw=_g.draw_part}))
	pt.t,pt.update=time_t+pt.dly,pt.update or _g.update_part
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
		for _,d in pairs(bucket) do
			d=d.obj
			
			-- draw shadow
			if (d.draw_shadow) d:draw_shadow()
			d:draw()
		end
	end
end

function update_ground()
	local pos=plyr and plyr.pos or cam.lookat
	
	local i0,j0=flr(shr(pos[1],ground_shift)),flr(shr(pos[3],ground_shift))
	for i=i0+ground_left,i0+ground_right do
		local cx=band(i,0x7f)
		for j=j0+ground_near,j0+ground_far do
			local cy=band(j,0x7f)
			local t=ground_actors[cx+cy*128]
			if t then
				t:update(i,j)
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
 n=min(n,99)
 local s=tostr(flr(n))
	return sub(padding_mask,1,2-#s)..s
end

function time_tostr(t)
 if(t==32000) return "--"
 -- frames per sec
 local s=padding(flr(t/30)%60).."''"..padding(flr(10*t/3)%100)
 -- more than a minute?
 if(t>1800) s=padding(flr(t/1800)).."'"..s
	return s
end

function printb(s,x,y,c)
	local len=4*#s+2
	rectfill(x,y,x+len,y+6,c)
	print(s,x+1,y+1,0)
	-- shade
	line(x,y+7,x+len,y+7,1)
end

function draw_hud()
	circ(15,111,16,7)
	local rpm=1-plyr.rpm/plyr.max_rpm[2]
	rpm*=0.75
	color(8)
	line(15,111,15+10*cos(rpm),111+10*sin(rpm))
	circfill(15,111,3)
	
	print("gear:"..(plyr.gear==1 and "lo" or "hi"),99,121,7,5)

 local angle,dist=track:get_dir(plyr.pos)
	spr(116+angle,60,2)
	print(flr(dist).."m",64-6,11,7)
end

function _draw()
	cls(0)

	zbuf_sort()
	draw_ground()
 
	track:draw()
	
	draw_hud()

	-- print((30*sqrt(v_dot(plyr.v,plyr.v))/3.6).."km/h",2,18,7)
	printb("lap time",2,2,8)
	printb(time_tostr(track.lap_t),2,8,6)
	printb("best time",127,2,10)
	printb(time_tostr(track.best_t),127,8,6)
 
 --print("front:"..(360*plyr.slip_angles[1]),2,18,7)
 --print("rear:"..(360*plyr.slip_angles[2]),2,24,7)

	local slip=plyr.slip_angles[1]
 for i=0,127 do
  local t=i/127
  color(t<slip and 2 or 14)
 	pset(i,64-32*t)
 	pset(i,64-32*out_elastic(t))
 end
  
	--[[	
	for _,a in pairs(physic_actors) do
		a.bbox:draw(a.m,a.pos)
	end
	]]


	--rectfill(0,0,127,8,8)
	--print("mem:"..stat(0).." cpu:"..stat(1).."("..stat(7)..")",2,2,7)
 
 --print(plyr.acc,2,18,7)
 
	-- print(plyr.traction,2,18,7)	
	
	-- debug
	--[[
	if plyr then
		draw_vector(plyr.m,plyr.pos,plyr.v,11)
	 
	 local i=2
	 for _,k in pairs(plyr.forces) do
			draw_vector(plyr.m,k.pos,k.force,i)
			i+=1
		end	
		-- debug
		plyr.forces={}
	end
	]]
end

-- main

function _init()
 sfx(0,3,0)
	-- q binary layout
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

	local max_tree=1000
	for j=0,63 do
		for i=0,63 do
			local di,dj=i-32,j-32
			local d=abs(sqrt(di*di+dj*dj)-11)
			if flr(d)==2 then
				if rnd()>0.5 and max_tree>0 then --and is_solid(i,j,4)==1 then
					make_ground_actor("tree",2*i,2*j)
					max_tree-=1
				end
			end
		end
	end

	cam=make_cam(96)
	
	track=make_track(unpack_track())
	local pos=track:get_startpos()
  
	plyr=make_rigidbody(make_actor("plyr",{pos[1],pos[2]+4,pos[3]},0.5))
	plyr.slip_angles={0,0}
	
	ghost=make_actor("ghost",v_zero())
	
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

-->8
-- trifilltex
-- 
function trapezefill(l,dl,r,dr,start,finish)
	local l,dl={l[1],l[4],l[5],r[1],r[4],r[5]},{dl[1],dl[4],dl[5],dr[1],dr[4],dr[5]}
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
		--rectfill(l[1],j,r[1],j,11)
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
	-- x u v
	trapezefill(v0,v1,v0,v02,y0,y1)
	-- lower trapeze
	trapezefill(v1,v2,v02,v2,y1,y2)
end
__gfx__
1ca9b34500015100000000007700770077007700eeeeeeeeeee77777bb88856599777777777777777777777777777777777777788756566bb000000000000000
0000000001dc7700010000007700770077007700eeeeeeee7775e5e7b888856599766666777777777711666667666677777777788756666bb000000000000000
4950000012e77700520000000077007700770077eeeeee775e7e5e57b8888565657666667777777777116666677777666666667aa756776bb000000000000000
000000003333b700130000000077007700770077eeeee7e5e5e5e5e5b8888565657666667777777777116666677777777777776aa756776bb000000000000000
000000002497a700240000007700770077007700eeee7e5e5e5e5e5ebb0555656586666677777777771166666777777887667765a756776bb000000000000000
0000000015d76700150000007700770077007700eee7e7e5e5e5e5e5bb0555656c866666777777777711666665777788877766656756556bb000000000000000
0000000015676700540000000077007700770077ee7e5e7e5e5e5e5ebb505565cc866666777777777711666665777780877777656756666bb000000000000000
0000000015566700670000000077007700770077ee75e5e5e5e5e5e5eb055565cc866666777777777711666665777787877777656756556bb000000000000000
5c0000002288ee00280000000000000000000000e75e5e5e5e5e5e5e5b505565ca766666777777777711666665777788877777656756666bb000000000000000
00000000499ffa00540000000000000000000000e7e5e5e5e5e5e5e5eb056565aa766666777777777711666665777778877777656756556bb000000000000000
0000000099aaa7009a0000000000000000000000e77e5e5e5e5e5e5e5b566565a1766666777777777711666665777777777777765756666bb000000000000000
0000000055533b003b000000000000000000000075e5e5e5e5e5e5e5eb50656511766666777777777711666665777777778877567756556bb000000000000000
000000001dcc7c001c00000000000000000000007e5e5e5e5e5e5e5e5b56656511766666777777777711666665777778888877656756666bb000000000000000
00000000115dd6001d000000000000000000000075e5e5e5e5e5e5e5eb05656518766666777777777711666665777788111177656756556bb000000000000000
000000001122ee002e00000077777777777777777e5e5e5e5e5e5e5e5b5055658876666677777777771166666577778111aa77656756666bb000000000000000
0000000022eeff00ef00000077777777777777777775e5e5e5e5e5e5eb05556585788666777777777711666665777781aacc77656756556bb000000000000000
00000000eeeeeeeeeeeeeeee2222222222220000eeee5e5e5e5e5e5e5b50556565788866777777777711666665777781acc777656756666bb000000000000000
00000000eeeeeeeeeeeeeeee2888888888820000eeeeeeeeeeeeeeeebb05556565780866777777777711666665777781ac7766656756556bb000000000000000
009bb000eeeeeeeeeeeeeeee28eeeeeeee820000eeeeeeeeeeeeeeeebb05556565787866777777777711666667777781a7667765a756776bb000000000000000
004bbbb0eeeeeeeeeeeeeeee28e3e3e3ee820000eeeeeeeeeeeeeeeeb8888565657888667777777777116666677777777777776aa756776bb000000000000000
004bbb00eeeeeeeeeeeeeeee28e333e3ee820000eeeeeeeeeeeeeeeeb8888565657886667777777777116666677777666666667aa756776bb000000000000000
00400000eeeeeeeeeeeeeeee28e3e3e3ee820000eeeeeeeeeeeeeeeeb888856599766666777777777711666667666677777777788756666bb000000000000000
00400000eeeeeeeeeeeeeeee28eeeeeeee820000eeeeeeeeeeeeeeeebb88856599777777777777777777777777777777777777788756566bb000000000000000
00111110eeeeeeeeeeeeeeee2811111111820000eeeeeeeeeeeeeeeebbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb000000000000000
00000000eeeeee0000eeeeee2812112221820000eeeeeee2eeeeeeeebbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb000000000000000
00000000eeee0077770eeeee2812112121820000eeeeee282eeeeeeebbbbbbbb7777777777777777777777777bbbbbbbbbbbbbbbbbbbbbbbb000000000000000
00988000eee0667777700eee2812212221820000eeeee27872eeeeeebbbbbbb777777666666676666666666667bbbbbbbbbbbbbbbbbbbbbbb000000000000000
00488880ee066666666660ee2811111111820000eeeee28782eeeeeebbbbbb777887666666667666666666666676bbbbbbbbbbbbbbbbbbbbb000000000000000
00488800ee0666665555550e2888888888820000eeee2688862eeeeebbbbb7778aa86666666676677777666666676bbbbbbbbbbbbbbbbbbbb000000000000000
00400000ee1055550000001e2222222222220000eeee2877782eeeeebbbb77778aa866666666766766666666666676bbbbbbbbbbbbbbbbbbb000000000000000
00400000eee10000111111ee0000000000000000eeee1288821eeeeebbb777777887666666667667777766666666676bbbbbbbbbbbbbbbbbb000000000000000
00111110eeee11111eeeeeee0000000000000000eeeee11111eeeeeebb77777777776666666676666666666666666776bbbbbbbbbbbbbbbbb000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000b77777777777777777777777777777777777777777777777777bbbbbb000000000000000
ee9994eeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000777888888888888888777777777777777777888888888888888777bbb000000000000000
e999994eee99eeeeeeeeeeeeeeeeeeee000000000000000000000000778888111111111111177775575555555577711111111111888877777000000000000000
e999994eee94eeeeeeeee777eeeeeeee000000000000000000000000558881111aaaaaaaaaaa77755575755757777aaaaaaaa111188887888000000000000000
e499944eeeeeeeeeeeee49994eeeeeee00000000000000000000000086688111aaaccccc6656677575757575557777ccccccaa111188878a8000000000000000
e444444eeeeee4eeeeeef111feeeeeee00000000000000000000000086660055500ccccc66566777777777777777777cccccca00555007787000000000000000
ee4444eeeeeeeeeeeeee15051eeeeeee000000000000000000000000556055555550ccccc66566777777777777777777ccccc055555550777000000000000000
eeeeeeeeeeeeeeeeeeeef070feeeeeee0000000000000000000000006605555555550cccc66566777878777787877777cccc0555555555055000000000000000
eeeeeeeeeeeeeeeeeeeef505feeeeeee0000000000000000000000006655555755555cccccccccc778888888878777777ccc5555575555566000000000000000
ee9994eeee994eeeeeee20000eeeeeee0000000000000000000000006655556665555cccccccccc777777777777777777ccc5555666555566000000000000000
e999994ee99994eeeeee24442eeeeeee0000000000000000000000006655576667555ccccccccccc777777777777777777cc5557666755565000000000000000
e999444ee49944eeeeeee252eeeeeeee000000000000000000000000b655556665555ccccccccccc111111111111111111cc5555666555566000000000000000
e4944eeeee444eeeeeeee888eeeeeeee000000000000000000000000bb55555755555cccccccccccc111111111111111111c5555575555566000000000000000
e444eeeeeeeeeeeeeeeee8e8eeeeeeee000000000000000000000000bbb555555555055555555555555555555555555555550555555555bbb000000000000000
ee44ee94eeeee4eeeee1151511eeeeee000000000000000000000000bbbb5555555bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb5555555bbbb000000000000000
eeeeee44eeeeeeeeeeee11111eeeeeee000000000000000000000000bbbbb55555bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb55555bbbbb000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeee888eeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeebbeeeeeeeeee82228eeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeebbbb3eeeeeeeee48884eeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeebb38bb3eeeeeeeef040feeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeebbbbbb333eeeeeeeefffeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eee3b8bbb3233eeeeeeee222eeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eee33bb33335eeeeeeee28882eeeeeee000070000000000000007000000000000007000000000700007770000070000000000000000000000000000000000000
eeee333355511eeeeeeff282ffeeeeee077777000077770000077700007777000077777000707770007770000777070000000000000000000000000000000000
eee115551111eeeeeeeff161ffeeeeee077777700007770000777770007770000777777000777700007770000077770000000000000000000000000000000000
eeee1111eeeeeeeeeeeeeccceeeeeeee077777000077770000077700007777000077777000777000077777000007770000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeef0feeeeeeee000070000777070000077700007077700007000000777700007770000077770000000000000000000000000000000000
eeeeeeeeeeeeeeeeeee1150511eeeeee000000000070000000077700000007000000000000000000000700000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeee11111eeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
206040207021f141100156f67456d874b9f674b9d87456f69956d899b9f699b9d89956f6fb5698fbb9f6fbb998fbb6b9e559b9e5b6b97859b978d04010206050
4000030002720272035607e74030402010400071a071a00000000874e7406080c0a04012001271e271e20008cab84030708040400003720372020002b907e740
90a0c0b0409300e200e271937108fbc7405060a09040720372029322930356cad7408070b0c0407202720393039322b9cad7401090b03040b1b282b282d2b1d2
0838f640d0e001f04011001171917191000837b940d02040e0401100a000a071117108354940e0408001409091000272020291891749408060f0014012711200
9100917108094940d0f06020409091029172020002861749d0060808080806080a380a080808080a0608080a0808080608080a0808b9f6e9c80808994926c808
b040207021f14110d0d0a1321080b9f674b9f6fb56f6fb56f67456b974b9b97456b9fbb9b9fb60404050703000563858402080601000b9385840307080200008
fb584040302010000838f64050608070000838b9401060504000087458600608080a080808080a080608080a08080806ff00ff00ff00ff00ff00ff00ff00ff00
ff00ff00ff00ec00806890847f00808890849c0080889084c9008078901168211590847c0080a89084a9008078901188212890845c00809011882115b8908488
0080789011f8213890844c002890a82115b8908468008078901109211538904c00289059211588901189211528904c002890692115689011a9211590843c0028
900b21a0a478211590841c00809011fa21a02890a47821289084fb008090117821a0a40a21a0a890a46821389084eb0028907821a02890a4e921a0c890682115
3890eb0028906821a0901014b890a4c821a078901088001428907821152890eb00289068212890280014b890a4a821a0789010a8001490a47821159084db0028
9068212890d80014c8901029001490a47821159084cb00289068212890e80014a8901049002890a47821289084bb00289068212890ea003890a46821389084ab
00289068212890ea001438906821153890ab00289068212890fa0014289078211528909b008090115821a090100b001490a478211590847b008090115821a090
102b001490a478211590846b002890682128904b002890a478212890845b002890682128904b003890a468213890844b002890682128904b0014389068211538
904b002890682128905b0014289078211528904b002890682128906b001490a478211590843b002890682128907b001490a478211590841b0080901168212890
8b002890a47821289084fa00809011782128908b003890a46821389084ea0028907821a090108b001438906821153890ea0028906821a09010ab001428907821
152890ea00289068212890cb001490a47821159084da00289068212890db001490a47821159084ca0028906821159084db002890a47821289084ba0028907821
159084cb003890a46821389084aa001490a47821289084bb001438906821153890ba001490a46821389084bb001428907821152890ca002890a45821153890cb
001490a47821159084ba003890a45821152890db001490a47821159084aa001438906821159084db002890a478212890ba001428907821159084cb003890a468
212890ca001490a478212890cb0014389068212890da001490a468212890db0014289068212890ea0028906821159084db0028906821159084da002890782115
9084cb0028907821159084ca001490a47821289084bb001490a478212890da001490a46821389084bb001490a468212890ea002890a45821153890cb00289068
212890ea003890a45821152890cb00289068212890ea001438906821159084bb00289068212890fa001428907821159084ab002890682128900b001490a47821
2890849b00289068211590840b001490a468213890848b00289078211590840b00289068211538908b001490a4782128900b00289078211528909b001490a468
2128900b001490a478211590849b002890682128901b001490a478211590848b002890682128902b001490a478212890847b002890682128903b001490a46821
3890846b002890682128904b002890a458211538906b002890682128904b003890a458211528906b002890682128904b0014389068211590844b008090115821
a090105b0014289078211590842b008090115821a090107b001490a478211590841b002890682128909b001490a4782115389084ea00289068212890ab002890
a4782115389084da00289068212890ab003890a4982115389084aa00289068212890ab00143890a821153890849a00289068212890bb00142890d82115389084
6a00289068212890cb001490a4d821153890845a00289068212890db00143890a4d821153890842a00289068212890eb00143890a4d821153890840a00809011
5821a090101c00143890a4d82115389084c9008090115821a090103c00143890a4d82115389084a900802890682128907c00143890a4d82115b89084c8008058
90682128908c00143890a4d82115b89084a8008058901168212890bc00143890a4592115c89011b8212890cc00143890a4592115a89011c8212890fc00143890
a4aa2128900d00143890a48a21a090103d00143890a40a21a05890105d00143890a4e921a05890109d0014b890a4e821a0589010fd0014b890a4c821a0589010
be0014e890101f0014c89010ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00
ff00ff00ff00ff00ff008c00f90002007004c90074707747030021283354045900753877665565672866045900754877475355212258001101f8007438774701
8800204424f80075387737980051775701e80076387706980062287702e80075387702980062287704e80073387701980061287715e80070287747a800302877
37e80020287716a8001075776701d80030287703b800533444026800101158003176470159003211580031765701490010230158005177570149001015680051
77570149003015680051776702490020156800622877014900203711310138007177670259007655750238006277570159007467660438005177570159007335
33033800517757015900620200023800517736015900500100113800316635015900300100513800316546690050020062012800406635015900403533750528
0031754601680021e80020676677172800317567035800205701e800762877372800317677135800715701e8007028774728003076771558007757f800402877
472800107677264800707757c80010110040287747280030287715480028775701b8006266027028775728004228771438005028775701a80010287717722877
47001073287726380074387703b80033774774287727001074287746010030487738001198007048770700106356776703005138770728002054048800304877
03280010027677140051387701280051771588001075387701380000747715004128773738007277269800313877480000747726002075770338007277570200
02280030380010757737480000747715280021124800612877464334020052370128005377174800007567139800762877667656547677152800107305480000
75470298002075787737380051580000755778000238001028324333446577149800007547015800405601490000735702580062570149000061772458007277
13013900005077450148007277573501290000407767034800602877570129000030287714580076287713980004880000307677175800206277570378004017
88000020767737680020767737113800281011737701780000107577276800207628775535281155575528775705680028007677266800107548775575687707
6800001075660468001075c877025800102800362278001075b877476800012800142800112812380030b8776702680010280012113254764601280040575588
772602880038004138776703380010115355587726a800380010762877670378007177676622b80048006177676603780010462202c800580063351389000a00
0a00b0f0029061c09081b0428003a1232203c2a2e202d24172000000000000000000000000000000000000000000000000000000000000000000000000000000
__gff__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000f08e0c022d000000000000000000000000006e637300
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000100021b04013040100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070100700c070
