pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- globals
local time_t,time_dt=0,1/30
local actors,ground_actors,parts,v_light,cam,plyr,active_ground_actors={},{},{},{0,1,0}
local track

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
local all_models=json_parse'{"205gti":{}}'
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
					local v=model.v[vi]
					local x,y,z,w=v[1],v[2],v[3]
					x,y,z,w=cam:project(m[1]*x+m[4]*y+m[7]*z+pos[1],m[2]*x+m[5]*y+m[8]*z+pos[2],m[3]*x+m[6]*y+m[9]*z+pos[3])
					p[vi]={x,y,z,w}
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
		for _,f in pairs(faces) do
			f=f.face
			local p0=p[f.vi[1]]
			for i=2,#f.vi-1 do
				local p1,p2=p[f.vi[i]],p[f.vi[i+1]]
		 		for i=-1,1,2 do
		 			for j=-1,1,2 do
				 	trifill(p0[1]+i,p0[2]+j,p1[1]+i,p1[2]+j,p2[1]+i,p2[2]+j,0)
				 end
				end
			end	
		end
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
					local x,y,z,w=cam:project(v[1],v[2],v[3])
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
		ratio=-v_dot(v,relv)
		ratio*=8--self.mass_inv*30
		-- sliding?

		local max_traction=isrear and 12 or 12
		if abs(self.traction_ratio*ratio)>max_traction then
			slide=true
			-- clamp
		 ratio=mid(ratio,-max_traction,max_traction)
		end
	end
	-- wheels on ground?
	ratio*=self.traction_ratio
 v_scale(v,ratio)
	
	-- apply 
	self:add_force(v,pos)
	
	-- smoke only for rear wheels
	if isrear and slide then
	 pos=v_clone(pos)
	 v_add(pos,m_right(self.m),rnd(2)-1)
		--add(pos,v_up)
		make_part("smoke",pos)
	end
end

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
		add_tireforce(self,{-sin(angle),0,cos(angle)},5)
		-- rear wheels
		add_tireforce(self,v_right,-1,nil,true)
	end

	-- accelerate
	if btn(2) then
		add_tireforce(self,v_fwd,-1,12)
	end
	-- brake
	if btn(3) then
		add_tireforce(self,v_fwd,-1,-8)
	end
	
	if btn(4) or btn(5) then
		local pos=v_clone(self.pos)
		v_add(pos,m_up(self.m),3)
		local force=v_clone(v_up)
		v_scale(force,12)
		self:add_force(force,pos)
	end
end

local ghost={}
_g.update_plyr=function(self)
	local up=m_up(self.m)
	-- time decay
	self.traction*=0.8
	self.turn*=0.7
	
	for i=1,4 do
		local v=self.bbox.v[i]
		if v.contact_t and time_t-v.contact_t<5 then
			-- contact quality
			local r=max(v_dot(up,v.n))
			self.traction+=r
		end
	end
	self.traction_ratio=self.traction/20
	
	-- record pos and orientation
	if time_t%4==0 then
		serialize(self.pos,ghost)
		serialize(self.q,ghost)
	end
	
	-- hit ground actors?
	
	return true
end

_g.draw_plyr_shadow=function(self)
	draw_model_shadow(self.model,self.m,self.pos)
end

_g.draw_spr_actor=function(self)
	palt(0,false)
	palt(14,true)
	local x,y,z,w=cam:project(self.pos[1],self.pos[2],self.pos[3])
	w*=3
	local sx,sy=band(self.frame*8,127),8*flr(self.frame/16)
	sspr(sx,sy,16,16,x-w/2,y-w,w,w)
	pal()
end

local all_actors=json_parse'{"plyr":{"model":"205gti","hardness":0.02,"mass":32,"turn":0,"traction":0,"traction_ratio":0,"control":"control_plyr","update":"update_plyr","draw_shadow":"draw_plyr_shadow"},"tree":{"update":"nop","draw":"draw_spr_actor","rnd":{"frame":[37,96,66,98]}}}'

function draw_actor(self)
	draw_model(self.model,self.m,self.pos)
end

function make_actor(src,p)
	-- instance
	local a=clone(all_actors[src],{
		pos=v_clone(p),
		q=q or make_q(v_up,0.25)
	})
	a.model,a.draw=all_models[a.model],a.draw or draw_actor
	-- init orientation
	a.m=m_from_q(a.q)
	return add(actors,a)
end

-- note: limited to a single actor per tile
function make_ground_actor(src,i,j)
	local x,z=shl(i+rnd(),ground_shift),shl(j+rnd(),ground_shift)
	local a=clone(all_actors[src],{
		pos={x,0,z}
	})
	a.model=all_models[a.model]
	a.draw=a.draw or draw_actor
	-- adjust pos
	a.pos[2]=get_altitude_and_n(a.pos)
	-- any angle defined in instance?
	local q=make_q(v_up,a.angle or 0)
	local m=m_from_q(q)
	a.m=m
	-- register
	ground_actors[i+j*128]=a
	return a
end

-- registers a ground collision
function make_ground_contact(a,p,n,d)
	local padot=a:pt_velocity(p)
	local vrel=v_dot(n,padot)
	-- resting condition?
	if(d<k_small and vrel>-k_small_v) return
			
	local c={
		-- body
		a=a,
		-- normal
		n=n,
		-- body contact point
		-- world position
		p=p,
		-- penetration
		d=d,
		-- relative velocity
		v=vrel,
		-- 
		nimpulse=0,
		pre_solve=function(self,dt)
			local a,n=self.a,self.n
			
			local ra=make_v(a.pos,self.p)
			local racn=make_v_cross(ra,n)

			local nm=a.mass_inv
			nm+=v_dot(racn,m_x_v(a.i_inv,racn))
			self.nm=1/nm
			
			-- baumgarte
			local bias=-k_bias*max(self.d+k_slop)/dt

			-- restitution bias
			local va=v_clone(a.v)
			v_add(va,make_v_cross(a.omega,ra))
			local dv=-v_dot(va,n)
			-- todo:find out unit??
			if dv<-1 then
				bias-=a.hardness*dv
			end
			self.bias=bias
			self.ra=ra
		end,
		solve=function(self)
			local a,dv,n=self.a,v_clone(self.a.v),v_clone(self.n)
			v_add(dv,make_v_cross(a.omega,self.ra))

			local vn=v_dot(dv,n)
			local lambda=-self.nm*(vn+self.bias)
  	
			local tempn=self.nimpulse
			self.nimpulse=max(tempn+lambda)
			lambda=self.nimpulse-tempn
			
			-- impulse too small
			if(lambda<k_small) return
  	-- correct linear velocity
			v_scale(n,lambda)			
			v_add(a.v,n,a.mass_inv)
			-- correct angular velocity
			v_add(
				a.omega,
				m_x_v(
					a.i_inv,
					make_v_cross(self.ra,n)
				))
		end
	}
	return c
end

-- rigid body extension for a given actor
-- note:actor must have a 3d model
function make_rigidbody(a,bbox)
	local rb={
	 -- debug
		forces={},
		force=v_zero(),
		torque=v_zero(),
		-- bounding box
		bbox=bbox,
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
			-- debug
			add(self.forces,{force=f,pos=p})
			
			v_add(self.force,f,self.mass)
			v_add(self.torque,make_v_cross(make_v(self.pos,p),f))
		end,
		-- apply forces & torque for iteration
		prepare=function(self,dt)
			-- add gravity
			v_add(self.force,{0,-16*self.mass,0})
		
			-- inverse inertia tensor
			self.i_inv=m_x_m(m_x_m(self.m,self.ibody_inv),m_transpose(self.m))
	
			-- velocity
			v_add(self.v,self.force,self.mass_inv*dt)
	
			-- angular velocity
			v_add(self.omega,m_x_v(self.i_inv,self.torque),dt)
			
			-- friction
			v_scale(self.v,1/(1+dt*0.4))
			v_scale(self.omega,1/(1+dt*0.4))
		end,
		integrate=function(self,dt)
			v_add(self.pos,self.v,dt)
			q_dydt(self.q,self.omega,dt)
			self.m=m_from_q(self.q)
			-- clear forces
			self.force,self.torque=v_zero(),v_zero()
		end,
		update_contacts=function(self,contacts)
			local i=0
			for _,v in pairs(self.bbox.v) do
				-- to world space
				local p=m_x_v(self.m,v)
				v_add(p,self.pos)
				local h,n=get_altitude_and_n(p,true)
				local depth=h-p[2]
				if depth>k_small then
					depth=v_dot(n,{0,depth,0})
					-- deep enough?
					if depth>-k_small then
						local ct=make_ground_contact(self,p,n,depth)
						if ct then
							add(contacts,ct)
							-- record contact time
							v.contact_t=time_t
							v.n=n
						end
					end
				end
			end
		end
	}
	
	-- compute inertia tensor
	local size=v_sqr(get_modelsize(bbox))
	local ibody=make_m(size[2]+size[3],size[1]+size[3],size[1]+size[2])
	m_scale(ibody,a.mass/12)
	
	-- invert 
	rb.ibody_inv=m_inv(ibody)
	
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
		c:pre_solve(dt)
		-- multiple iterations
		-- required to fix deep contacts
		for i=1,5 do
			c:solve()
		end
	end
	
	-- move bodies
	for _,a in pairs(physic_actors) do
		a:integrate(dt)
	end
end

-- track
function make_track(laps,segments)
	local t={
		i=0, -- active index
		segments=segments,
		t=0, -- total time
		chrono={}, -- intermediate times
		laps=laps,
		on_new_lap=nop, -- lap callback
		get_startpos=function(self)
			return segments[1].pos
		end,
		is_over=function(self)
			return flr(self.i/self.length)>self.laps
		end,
		update=function(self)
			local p=self.segments[self.i%self.length+1]
			if sqr_dist(plyr.pos,p.pos)<16 then
				self.i+=1
				if self.i%self.length==0 then
					self.chrono={}
					self:on_new_lap()
				end
				if p.chrono then
					-- diff time
					add(self.chrono,self.t-(self.chrono[#self.chrono] or 0))
				end
			end
			self.t+=1
		end,
		-- debug draw
		draw=function(self)
			for _,v in pairs(self.segments) do
				local x,y,z,w=cam:project(v.pos[1],get_altitude_and_n(v.pos),v.pos[3])
				spr(48,x-4,y-4)
			end
		end
	}
	t.length=#segments
	return t
end

-- camera
function make_cam(f)
	local c={
		pos={0,6*hscale,0},
		lookat={0,0,-7*16},
		focal=f,
		dist=shl(8,ground_shift),
		-- camera rotation
		c=1,s=0,
		track=function(self,pos,angle)
			self.pos=v_clone(pos)
			self.lookat=v_clone(pos)
			self.c,self.s=cos(angle),-sin(angle)
			v_add(self.pos,{0,self.dist*self.s,self.dist*self.c})
		end,
		project=function(self,x,y,z)
			x-=self.lookat[1]
			local tmpy=y
			-- fake 3d
			y=-self.lookat[2]
			z-=self.lookat[3]
			z,y=self.c*z+self.s*y,-self.s*z+self.c*y

  			local xe,ye,ze=x,y,z-self.dist

		 	local w=-self.focal/ze
  			return 64+xe*w,64-(tmpy+ye)*w,ze,w
		end
	}
	return c
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
	local x,y,z,w=cam:project(self.pos[1],self.pos[2],self.pos[3])
	-- behind camera
	if(z>=0) return
	
	-- simple part
	if self.kind==0 then
	 --[[
		local s=flr(3*self.frame)
		local s=flr(3*self.frame)
		local c0,c1=sget(s,2),sget(max(s-1),2)
		fillp(lerparray(dither_pat,1-self.frame)+0x0.f)
		circfill(x,y,w*self.r,bor(shl(c1,4),c0))
		fillp()
		]]
		local s=self.frames[flr(self.frame*(#self.frames-1)+0.5)]
		if s then
			palt(0,false)
			palt(14,true)
			spr(s,x-4,y-4)
			pal()	
		end
		
		--[[
		w*=1
		
		palt(0,false)
		palt(14,true)
		sspr(56,0,8,8,x-w/2,y-w/2,w,w)
		pal()		
		]]
	end
end

all_parts=json_parse'{"smoke":{"frames":[64,80,81,65],"r":1,"rnd":{"dly":[8,20],"g_scale":[-0.03,-0.05]},"frame":0,"dv":0.9,"dr":0,"kind":0}}'

function make_part(part,p,v)
	local pt=add(parts,clone(all_parts[part],{pos=v_clone(p),v=v and v_clone(v) or v_zero(),draw=_g.draw_part,c=c}))
	pt.t,pt.update=time_t+pt.dly,pt.update or _g.update_part
	pt.df=1/pt.dly
	if(pt.sfx) sfx_v(pt.sfx,p)
	return pt
end

-- map helpers (marching codes, height map, normal map cache)
local qmap,hmap,ncache={},{},{}
function safe_index(i,j)
	return bor(band(i,0x7f),shl(band(j,0x7f),7))
end
function get_raw_qcode(i,j)
	return qmap[safe_index(i,j)] or 0
end
function get_height(i,j)
	return 0
	-- already scaled
	-- return hmap[safe_index(i,j)]
end
function get_q_colors(q)
	return shr(band(0b00111000,q),3)+1,band(0b111,q)+1
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
	for i=i0-6,i0+6 do
		local cx=band(i,0x7f)
		for j=j0-7,j0+5 do
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

	local imin,imax=-7,7
	local cx,cz=cam.lookat[1],cam.lookat[3]
	-- cell x/z ratio
	local dx,dz=cx%ground_scale,cz%ground_scale
	-- cell coordinates
	local nx,ny=flr(shr(cx,ground_shift)),flr(shr(cz,ground_shift))
	
	-- project anchor points
	local p={}
	-- grid depth extent
	for j=-7,5 do
	 -- project leftmost grid points
		local x,y,z,w=cam:project(-dx+cx+shl(imin,ground_shift),0,-dz+cz+shl(j,ground_shift))
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
		local ni=nx+imin
		local i,di=imin,1
		
		local v1=p[j]
		local w1=v1[4]
		local dw1=shl(w1,ground_shift)
		local x0,x1=v0[1],v1[1]
		
		-- todo: unit for w?
		local h0,h1=get_height(ni,nj),get_height(ni,nj+1)
		local y0,y1=flr(v0[2]-w0*h0),flr(v1[2]-w1*h1)
		local q0=get_raw_qcode(ni,nj)
		while i<=imax do
			local q3=get_raw_qcode(ni+1,nj)
			-- detail tile?
			if i==imin or i>=imax-1 or ni%2==1 or q0!=q3 then
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
				if i==imin or (i==imin-1 and di==2) then
					x0,y0=lerp(x0,x3,dx),lerp(y0,y3,dx)
					x1,y1=lerp(x1,x2,dx),lerp(y1,y2,dx)
				elseif i==imax then
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
					trifill(x1,y1,x3,y3,x0,y0,c_hi)
					trifill(x1,y1,x3,y3,x2,y2,c_lo)
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

 screen_update()

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

function draw_hud()
 palt(14,true)
 palt(0,false)
	spr(5,0,96,2,2)
	spr(5,15,96,2,2,true)
	spr(5,0,111,2,2,false,true)
	spr(5,15,111,2,2,true,true)
	local v=-sqrt(v_dot(plyr.v,plyr.v))/32
	line(15,111,15+10*cos(v),111+10*sin(v),8)
	circfill(15,111,3,7)
	palt()
end

local ghost_k=1
local ghost_p,ghost_m=v_zero()

function _draw()
	cls(0)

	zbuf_sort()
	draw_ground()

 if #ghost>2*60 then
	 if time_t%4==0 then
	  local q={0,0,0,0}
	 	ghost_k=deserialize(ghost,ghost_k,ghost_p)
 		ghost_k=deserialize(ghost,ghost_k,q)
		 ghost_m=m_from_q(q)
 	end
 	if ghost_m then
 		draw_model(all_models["205gti"],ghost_m,ghost_p,true)
 	end
 end
 
	track:draw()
	 
 draw_hud()

	-- print((30*sqrt(v_dot(plyr.v,plyr.v))/3.6).."km/h",2,18,7)
 --[[
	print("done:"..(track:is_over() and "yes" or "no"),2,8,9)
 for i=1,#track.chrono do
 	local c=track.chrono[i]
 	print("chrono "..i..":"..c,2,18+i*6,9)
 end
 ]]
	--[[	
	for _,a in pairs(physic_actors) do
		a.bbox:draw(a.m,a.pos)
	end
	]]
	
	--[[
	for _,c in pairs(world.contacts) do
		local x,y,z,w=cam:project(c.p[1],c.p[2],c.p[3])
  
		circ(x,y,3,8)
		print(c.dv,x+2,y,7)
	end
 ]]
 
	rectfill(0,0,127,8,8)
	print("mem:"..stat(0).." cpu:"..stat(1).."("..stat(7)..")",2,2,7)
	print("pow:"..plyr.traction_ratio.."%",2,19,7)

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
	-- q binary layout
	-- 0b01000000: q code
	-- 0b00111000: hi color
	-- 0b00000111: lo color
	
	local perlin="210000000000000000000000000000100000000000012344566788887788876453210000000000000000000000000010000000000001234455578887778987646542110000000000000000000000000000000000000012345556788777899763766421100000000000000000000000000000000000001234566778888888875377754320000000000000000000000000000000000000123466677899898876538876653110000000000000000000000000000000000012346667889889987643887776421000000000000000000000000000000000001224566788889998764488877643211000000000000000000000000000000000112345577788999865447887775432110000000000000000000000000000000011123457888888876544788776654332100000000000000000000000000000011101235788777766544478876666554420000000000000000000000112210011211123577777766544337887666766542000000000000000000000012321222222112357777766654322777666777654210000000000000000000011233223322111246777776654432266667776655321000000000000000000000122222333222234677666665444336556776555532100000000000000000000001122233333323567766655334444446666655554221000000000000000000000000123443333456776554433566634666555565433210000000000000000000000002344433445666554444457763456654556544321000000000000000000000000123433333466665444446887334565455543333211111111100000000000000000122233334566544556788823455543333344542221222210000000000000000011122223456655566788882345544333234555443333321000000000000000000111223345555556778998134544333223455555544542100000000000000000001222233455556678999813444333222345666665554210000000000000000000232333455556667888882334332221235666665555421000000000000000000123444456655566668888222322222223566665445542100000000000000001223455556655556655677801221112233455556544454211000000000000000123455556665555555456770000001234444445554444321100000000000000012345545555445554444666000000123444333455444311110000000000000001234444444444444444456600000012334432234444432100000000000000000123333334443333334445660000001223444322344543210111000100000000013444323333332122344555000001222333432234465432111100111000000112344442222222101223344400000223322233223456554222222221000011123334454332222100112223330000012322212222445665544333332100001223444455433322110111122333000001332221112235566554444432100001223444555665543321111101233300000133222112234555554444443220000233344566667776554211122223230000012233211234555554334454443211234444567888999876543333333334000001223211122344443333445555442234555568899baa99876544444444450000012232122222333322344566666544455556789aabba9988765555566556000001233322112222222234678888866666666789aabbbaa9876655555677660000012333221111212223357789aa98777887899aabbbaaa986554455577777000011233321111111112235779aaaa9899a99aaabbbcbaa98754555566777780000122332222221111112457899abbbaabbbbbbcccddcba97644566677677870001233333332221111001357899abbccccdccccdeeedcba876445666776677700013443323222221110024579aaabbccddeddddeffedcba875445666766667700002444322222222111124689aabcccdeeedddeeffedcb9865445666566776600001344322222111111234689abcddddeeddddeeeedcba9865455665567776600112344322222221101234789bcdddcddccccdddeddcb9876544555456776650111233322222232111124579abcdddddccbbbcccdedca9876544454456777650112223321122222222235689bbccddddccbbbbbcdedca9876655444456666540012222211112233333456789bbccccdddcbbbbbcdddca987665544445555543000112210001123444556789aaabbbccddcbbaaabcccba887655554344444432000001110001223456778999aa999abbdccba9aabbbba9877655554443332222000000000001235667899999998889abcccba99aaaa98877665555433222111100000000011234667899aa987778899abcbba99999976665555434422101110000000000122234578999aa9876777899abbba99aa8766544444433321100000000000000122234578899a9987666678899aa99aa98665433444333331000011200000001222334677889aaa9766667778998899a976643223333443310000123000000012334456777899aa9876666668888899a987643221223443221000233000000013445666777899aaa98765555778899aa98765332112344321100023300000002356777777889999999765445778899aaa8766432112333211001123300000002468898778888889998765445677789aaa876654211222222101222330000000247899998998777898877644567789aaaa987653222122333222222210000001357899aa9998766788765544578889aaa998765433322233333322210000000247889abba988765677655444678889aaa988765444433334444321100"
	-- uncompress map
	for i=1,#perlin do
		local c=tonum("0x"..sub(perlin,i,i))
		add(noise,c)
	end
	
	-- read models from gfx/map data
	unpack_models()
	local segments={}
	unpack_map(segments)

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
	
	track=make_track(3,segments)
	local pos=v_clone(track:get_startpos())
	v_add(pos,v_up,4)

	plyr=make_rigidbody(make_actor("plyr",pos),all_models["205gti_bbox"])
	
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
function trifill2(x0,y0,x1,y1,x2,y2,col)
 color(col)
 line(x0,y0,x1,y1)
 line(x1,y1,x2,y2)
 line(x0,y0,x2,y2)
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
function unpack_map(track)
	while #qmap<0x4000 do
		-- peek data
		local len,q=unpack_int()
		-- rle or value?
		if band(0x80,len)>0 then
		 -- run length + value
			len,q=band(0x7f,len),unpack_int()
		else
			-- value only
			len,q=1,len
		end
		for j=1,len do
		 add(qmap,q)
		end
	end
	-- track
	for k=1,unpack_int() do
		add(track,{pos={shl(unpack_int(),ground_shift+1),0,shl(unpack_int(),ground_shift+1)}})
	end	
end

-->8
-- trifilltex
-- 
local function v4_clone(a,xy)
	return {a[xy],a[3],a[4],a[5]}
end
local function trapeze_strip_y(x0,x1,y,u,du,v,dv,w,dw)
	for x=x0,x1 do
		local c=sget(u/w,v/w)
 	if(c!=11)pset(x,y,c)
 	u+=du
 	v+=dv
 	w+=dw
 end 
end
local function trapeze_strip_x(y0,y1,x,u,du,v,dv,w,dw)
	for y=y0,y1 do
		local c=sget(u/w,v/w)
 	if(c!=11)pset(x,y,c)
 	u+=du
 	v+=dv
 	w+=dw
 end 
end
function trapezefill(l,dl,r,dr,start,finish,fn,dir)
	local l,r,dl,dr=v4_clone(l,dir),v4_clone(r,dir),v4_clone(dl,dir),v4_clone(dr,dir)
	local dt=1/(finish-start)
	for k=1,4 do
		dl[k],dr[k]=(dl[k]-l[k])*dt,(dr[k]-r[k])*dt
	end

	-- cliping
	if start<0 then
		for k=1,4 do
			l[k]-=start*dl[k]
			r[k]-=start*dr[k]
		end
		start=0
	end

	-- rasterization
	for j=start,min(finish,127) do
		--rectfill(l[1],y0,r[1],y0,11)
		local len=r[1]-l[1]
		if len>0 then
			local dx,w0,w1=1/len,l[2],r[2]
			local u0,v0=w0*l[3],w0*l[4]
			local du,dv=(w1*r[3]-u0)*dx,(w1*r[4]-v0)*dx
			local dw=(w1-w0)*dx
			fn(l[1],r[1],j,u0,du,v0,dv,w0,dw)
		end

		for k=1,4 do
			l[k]+=dl[k]
			r[k]+=dr[k]
		end
	end
end
function tritex(v0,v1,v2)
	local x0,x1,x2=v0[1],v1[1],v2[1]
	local y0,y1,y2=v0[2],v1[2],v2[2]
	if(y1<y0)v0,v1,x0,x1,y0,y1=v1,v0,x1,x0,y1,y0
	if(y2<y0)v0,v2,x0,x2,y0,y2=v2,v0,x2,x0,y2,y0
	if(y2<y1)v1,v2,x1,x2,y1,y2=v2,v1,x2,x1,y2,y1

	if max(x2,max(x1,x0))-min(x2,min(x1,x0)) > y2-y0 then
		-- mid point
		local v02,mt={},1/(y2-y0)*(y1-y0)
		for k,v in pairs(v0) do
			v02[k]=v+(v2[k]-v)*mt
		end
		if(x1>v02[1])v1,v02=v02,v1

		-- upper trapeze
		-- x w u v
		trapezefill(v0,v1,v0,v02,
			y0,y1,trapeze_strip_y,1)
		-- lower trapeze
		trapezefill(v1,v2,v02,v2,
			y1,y2,trapeze_strip_y,1)
	else
		if(x1<x0)v0,v1,x0,x1,y0,y1=v1,v0,x1,x0,y1,y0
		if(x2<x0)v0,v2,x0,x2,y0,y2=v2,v0,x2,x0,y2,y0
		if(x2<x1)v1,v2,x1,x2,y1,y2=v2,v1,x2,x1,y2,y1

		-- mid point
		local v02,mt={},1/(x2-x0)*(x1-x0)
		for k,v in pairs(v0) do
			v02[k]=v+(v2[k]-v)*mt
		end
		if(y1>v02[2])v1,v02=v02,v1

		-- upper trapeze
		-- x w u v
		trapezefill(v0,v1,v0,v02,
			x0,x1,trapeze_strip_x,2)
		-- lower trapeze
		trapezefill(v1,v2,v02,v2,
			x1,x2,trapeze_strip_x,2)
	end 
end
__gfx__
1ca9b34500015100000000007700770077007700eeeeeeeeeee77777bb88856599777777777777777777777777777777777777788756566bb000000000000000
0000000001dc7700010000007700770077007700eeeeeeee77710007b888856599766666777777777711666667666677777777788756666bb000000000000000
4950000012e77700520000000077007700770077eeeeee7710700007b8888565657666667777777777116666677777666666667aa756776bb000000000000000
000000003333b700130000000077007700770077eeeee71000100000b8888565657666667777777777116666677777777777776aa756776bb000000000000000
000000002497a700240000007700770077007700eeee700000000000bb0555656586666677777777771166666777777887667765a756776bb000000000000000
0000000015d76700150000007700770077007700eee7070000000000bb0555656c866666777777777711666665777788877766656756556bb000000000000000
0000000015676700540000000077007700770077ee71007000000000bb505565cc866666777777777711666665777780877777656756666bb000000000000000
0000000015566700670000000077007700770077ee70000000000000bb055565cc866666777777777711666665777787877777656756556bb000000000000000
5c0000002288ee00280000000000000000000000e710000000000000bb505565ca766666777777777711666665777788877777656756666bb000000000000000
00000000499ffa00540000000000000000000000e700000000000000bb056565aa766666777777777711666665777778877777656756556bb000000000000000
0000000099aaa7009a0000000000000000000000e771000000000000bb566565a1766666777777777711666665777777777777765756666bb000000000000000
0000000055533b003b00000000000000000000007100000000000000bb50656511766666777777777711666665777777778877567756556bb000000000000000
000000001dcc7c001c00000000000000000000007000000000000000bb56656511766666777777777711666665777778888877656756666bb000000000000000
00000000115dd6001d00000000000000000000007000000000000000bb05656518766666777777777711666665777788111177656756556bb000000000000000
000000001122ee002e00000077777777777777777000000000000000bb5055658876666677777777771166666577778111aa77656756666bb000000000000000
0000000022eeff00ef00000077777777777777777770000000000000bb05556585788666777777777711666665777781aacc77656756556bb000000000000000
00000000eeeeeeeeeeeeeeee0000000000000000eeeeeeeeeeeeeeeebb50556565788866777777777711666665777781acc777656756666bb000000000000000
00000000eeeeeeeeeeeeeeee0000000770000000eeeeeeeeeeeeeeeebb05556565780866777777777711666665777781ac7766656756556bb000000000000000
00000000eeeeeeeeeeeeeeee0000000770000000eeeeeeeeeeeeeeeebb05556565787866777777777711666667777781a7667765a756776bb000000000000000
00000000eeeeeeeeeeeeeeee0000000770000000eeeeeeeeeeeeeeeeb8888565657888667777777777116666677777777777776aa756776bb000000000000000
00000000eeeeeeeeeeeeeeee0000000770000000eeeeeeeeeeeeeeeeb8888565657886667777777777116666677777666666667aa756776bb000000000000000
00000000eeeeeeeeeeeeeeee0000000770000000eeeeeeeeeeeeeeeeb888856599766666777777777711666667666677777777788756666bb000000000000000
00000000eeeeeeeeeeeeeeee00000000000000000000000000000000bb88856599777777777777777777777777777777777777788756566bb000000000000000
00000000eeeeeeeeeeeeeeee00000000000000000999999999999990bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb000000000000000
00000000eeeeee0000eeeeee00000000000000000444444444444440bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb000000000000000
00000000eeee0077770eeeee00000007700000000005500000055000bbbbbbbb7777777777777777777777777bbbbbbbbbbbbbbbbbbbbbbbb000000000000000
00988000eee0667777700eee0000000770000000ee0440eeee0440eebbbbbbb777777666666676666666666667bbbbbbbbbbbbbbbbbbbbbbb000000000000000
00488880ee066666666660ee00000007700000000999999999999990bbbbbb777887666666667666666666666676bbbbbbbbbbbbbbbbbbbbb000000000000000
00488800ee0666665555550e00000007700000000444444444444440bbbbb7778aa86666666676677777666666676bbbbbbbbbbbbbbbbbbbb000000000000000
00400000ee1055550000001e00000007700000000005500000055000bbbb77778aa866666666766766666666666676bbbbbbbbbbbbbbbbbbb000000000000000
00400000eee10000111111ee0000000000000000ee0440eeee0440eebbb777777887666666667667777766666666676bbbbbbbbbbbbbbbbbb000000000000000
00000000eeee11111eeeeeee00000000000000001110011111100111bb77777777776666666676666666666666666776bbbbbbbbbbbbbbbbb000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000b77777777777777777777777777777777777777777777777777bbbbbb000000000000000
ee9994eeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000777888888888888888777777777777777777888888888888888777bbb000000000000000
e999994eee99eeeeeeee00000eeeeeee000000000000000000000000778888111111111111177775575555555577711111111111888877777000000000000000
e999994eee94eeeeeee0977790eeeeee000000000000000000000000558881111aaaaaaaaaaa77755575755757777aaaaaaaa111188887888000000000000000
e499944eeeeeeeeeeee0499940eeeeee00000000000000000000000086688111aaaccccc6656677575757575557777ccccccaa111188878a8000000000000000
e444444eeeeee4eeeee0f111f0eeeeee00000000000000000000000086660055500ccccc66566777777777777777777cccccca00555007787000000000000000
ee4444eeeeeeeeeeeee0150510eeeeee000000000000000000000000556055555550ccccc66566777777777777777777ccccc055555550777000000000000000
eeeeeeeeeeeeeeeeee005070500eeeee0000000000000000000000006605555555550cccc66566777878777787877777cccc0555555555055000000000000000
eeeeeeeeeeeeeeeee0ff55055ff0eeee0000000000000000000000006655555755555cccccccccc778888888878777777ccc5555575555566000000000000000
ee9994eeee994eeee0ff00000ff0eeee0000000000000000000000006655556665555cccccccccc777777777777777777ccc5555666555566000000000000000
e999994ee99994eeee002444200eeeee0000000000000000000000006655576667555ccccccccccc777777777777777777cc5557666755565000000000000000
e999444ee49944eeeeee02520eeeeeee000000000000000000000000b655556665555ccccccccccc111111111111111111cc5555666555566000000000000000
e4944eeeee444eeeeeee08880eeeeeee000000000000000000000000bb55555755555cccccccccccc111111111111111111c5555575555566000000000000000
e444eeeeeeeeeeeeeeee08080eeeeeee000000000000000000000000bbb555555555055555555555555555555555555555550555555555bbb000000000000000
ee44ee94eeeee4eeee110d0d011eeeee000000000000000000000000bbbb5555555bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb5555555bbbb000000000000000
eeeeee44eeeeeeeeee110000011eeeee000000000000000000000000bbbbb55555bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb55555bbbbb000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeee00000eeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeee0288820eeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeee00eeeeeeeee0822280eeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeee00bb0eeeeeee004888400eeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeee0b38bb0eeeeee00f040f00eeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeee0bbbbb330eeeeee0fffff0eeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eee0b8bbb3230eeeeeee00000eeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eee03bb33330eeeeeee0288820eeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeee033300011eeeee0ff282ff0eeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eee110001111eeeeee0ff161ff0eeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeee1111eeeeeeeeeee01ccc10eeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeee0f0f0eeeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeee1050501eeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeee1000001eeeeee000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
206040207021f141100156f67456d874b9f674b9d87456f69956d899b9f699b9d89956f6fb5698fbb9f6fbb998fbb6b9e559b9e5b6b97859b978d04010206050
4000030002720272035607e74030402010400071a071a00000000874e7406080c0a04012001271e271e20008cab84030708040400003720372020002b907e740
90a0c0b0409300e200e271937108fbc7405060a09040720372029322930356cad7408070b0c0407202720393039322b9cad7401090b03040b1b282b282d2b1d2
0838f640d0e001f04011001171917191000837b940d02040e0401100a000a071117108354940e0408001409091000272020291891749408060f0014012711200
9100917108094940d0f06020409091029172020002861749d0060808080806080a380a080808080a0608080a0808080608080a0808b9f6e9c80808994926c808
b040207021f14110d0d0a1321080b9f674b9f6fb56f6fb56f67456b974b9b97456b9fbb9b9fb60404050703000563858402080601000b9385840307080200008
fb584040302010000838f64050608070000838b9401060504000087458600608080a080808080a080608080a08080806ff00ff00ff00ff00ff00ff00ff00ff00
ff00ff00ff000d00806890847f00808890849c00804890840a0080389011a8211590847c0080689084e90080389011c82115906c0080c82115589084a8008058
90114921155c0080e821155890848800805890116921843c008090ab2190842c002890ab2115902c002890bb21152c009011cb21841c0011da21a03890101490
a4882190840c00da21a0389010280014908821159084fb008821a06890a46921a0389010a800149821159084db008088218890a44921a0389010c800a4982115
90cb0080908821289010a80014c89010390090a4982115cb00289088219010c80014a8901049001490a4982184bb002890882110ea001490a488219084ab0090
1188210b00149088211590849b001198211b001498211590848b009821a02b00a4982115908b008821a0902b0090a49821157b0080882128902b001490a49821
845b008090882128903b001490a4882190844b002890882190104b001490882115904b0028908821106b00149821154b0090117821a08b00a49821843b001178
21a0908b0090a4882190842b00882128908b00149088211590841b00882128909b00149821159084fa008088219010ab00a498211590ea008090882110bb0090
a4982115ea0028908821cb001490a4982184da0028908821db001490a488219084ca002890882184db0014908821159084ba00289088219084db001498211590
84aa00289088211590eb00a498211590aa002890982115eb0090a4982115aa001490a4982184db001490a4982184aa001490a488219084db001490a488219084
aa0014908821159084db00149088211590ba00149821159084db0014982115ca00a498211590eb00a49821ca0090a4982115eb0090a48821ca001490a4982184
db002890882184ca001490a488219084cb00289088219084ca0014908821159084bb00149088212890da00149821159084bb001488212890ea00a498211590cb
0088212890ea0090a4982115cb0088212890ea001490a4982184bb00a478211590fa001490a488219084ab0090a47821150b00149088211590849b0028908821
1b001498211590848b00289088212b00a4982115908b0014908821841b0090a49821159b0014882190840b001490a821849b00882128901b0014a82190848b00
882128902b001498211590847b00882128903b001498211590846b00882128904b00a498211590845b00882128904b0090a498211590844b00882128904b0014
90a4982115903b0080882128905b001490a49821152b008090882128906b001490a821841b002890882190107b0014a82190840b0028908821109b0014982115
9084fa0028908821bb0014b82115389084aa0028908821cb00a4b821153890849a0090117821a0cb0090a4f8211590846a00117821a090cb001490a4f8211590
845a0088212890db001490a419211590842a0088212890eb001490a419211590840a008088212890fb001490a43921159084c9008090882128900c001490a439
21159084a900802890882190103c001490a439211558908429008038908821105c001490a439211558908409008038901188219c00143890a499211538908448
00809011d821ac00143890a49921153890842800809011d821a0fc001490a4ca21a0900d001490a4ba2190103d001490a48a21105d001490a46a21109d001490
a40a21a09010bd001490a4e921a090103e00145890a4e821a090109e00145890a4c821a090105f00146890109f0014489010ff00ff00ff00ff00ff00ff00ff00
ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff00ff006c00b0f0029061c09081b0428003a1232203c2a2
e202d241720000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__gff__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000f08e0c022d0000000000000000336f206f676e637300
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
3000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000200090765006650066500665006650306500565005650056500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
