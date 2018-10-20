pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- globals
local time_t,time_dt=0,1/30
local actors,ground_actors,parts,v_light,cam,plyr,active_ground_actors={},{},{},{0,1,0}
local track

local physic_actors={}

-- world units
local ground_shift,hscale=1,4
local ground_scale=2^ground_shift

local good_side,bad_side,any_side,no_side=0x1,0x2,0x0,0x3

-- map
local noise={}

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
local dither_pat2=json_parse'[0xffff,0xa5a5,0x0000]'

_g.control_plyr=function(self)
	local turn,z=0,0
	if(btn(0)) turn=1
	if(btn(1)) turn=-1

	if(btn(2)) z=-1
	if(btn(3)) z=1

	plyr.pos[1]-=turn/4
	plyr.pos[3]+=z/4
	
	local h=0
	if btn(4) then
		h=0.25
	elseif btn(5) then
		h=-0.25
	end
	local i,j=shr(plyr.pos[1],ground_shift),shr(plyr.pos[3],ground_shift)
	noise[safe_index(i,j)]+=h
	--[[
	local radius=2
	for ii=-flr(radius),flr(radius) do
		for jj=-flr(radius),flr(radius) do
			local dist=sqrt(ii*ii+jj*jj)
			if dist<radius then
				dist/=radius
				noise[safe_index(i+ii,j+jj)]+=h*smoothstep(1-dist)
			end
		end
	end
	]]
end

_g.draw_spr_actor=function(self)
	palt(0,false)
	palt(14,true)
	local x,y,z,w=cam:project(self.pos[1],self.pos[2],self.pos[3])
	sspr(self.sx,self.sy,16,16,x-8,y-8,16,16)
	pal()
end

local all_actors=json_parse'{"plyr":{"control":"control_plyr","update":"nop",},"tree":{"model":"tree","update":"nop","draw":"draw_spr_actor","sx":56,"sy":16}}'

function draw_actor(self)
	local x,y,z,w=cam:project(self.pos[1],2,self.pos[3])
	spr(49,x-3,y)
	
	local i,j=flr(shr(self.pos[1],1)),flr(shr(self.pos[3],1))
	local h=noise[safe_index(i,j)]

	x,y,z,w=cam:project(2*i,h,2*j)
	circfill(x,y,2,8)	
end

function make_actor(src,p)
	-- instance
	local a=clone(all_actors[src],{
		pos=v_clone(p),
		q=q or make_q(v_up,0.25)
	})
	a.draw=a.draw or draw_actor
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
		dist=shl(16,ground_shift),
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
		local ss={7,23,8}
		local s=ss[flr(self.frame*3+0.5)]
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

all_parts=json_parse'{"smoke":{"rnd":{"r":[0.1,0.2],"c":[5,6,7],"dly":[8,20],"g_scale":[-0.03,-0.05]},"frame":0,"dv":0.9,"dr":0.02,"kind":0}}'

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
	return bor(band(i,0x3f),shl(band(j,0x3f),6))+1
end
function get_raw_qcode(i,j)
return 0x0105.0101
	-- return qmap[safe_index(i,j)]
end
function get_height(i,j)
	return noise[safe_index(shr(i,1),shr(j,1))]
end
function get_q_colors(q)
	return shl(band(0x0.00ff,q),16),shl(band(0x0.ff00,q),8),shr(band(0xf00,q),8)
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

function draw_tex_quad(a,b,sx,sy)
	local t,invdx,wa,wb=0,1/(b[2]-a[2]),a[4],b[4]
	for y=a[2],b[2] do
		local x,w=lerp(a[1],b[1],t),lerp(wa,wb,t)
		-- persp correction
		local u=t*wb/w
		sspr(sx,sy+16*u,16,1,x,y,shl(w,ground_shift),1)
		t+=invdx
	end
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
	local shade=function(lvl,c)
		return bor(shl(sget(max(lvl-1)+16,c),4),sget(lvl+16,c))
	end

	local imin,imax=-8,8
	local cx,cz=cam.lookat[1],cam.lookat[3]
	-- cell x/z ratio
	local dx,dz=cx%ground_scale,cz%ground_scale
	-- cell coordinates
	local nx,ny=flr(shr(cx,ground_shift)),flr(shr(cz,ground_shift))
	
	-- project anchor points
	local p={}
	-- grid depth extent
	for j=-8,8 do
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
		local v1=p[j]
		local w1=v1[4]
		local dw1=shl(w1,ground_shift)
		local x0,x1=v0[1],v1[1]
		local x2,x3=v1[1]+dw1,v0[1]+dw0
		
		-- offset to grid space
		local ni=nx+imin
		local h0,h1=get_height(ni,nj),get_height(ni,nj+1)
		local y0,y1=flr(v0[2]-w0*h0),flr(v1[2]-w1*h1)
		
		for i=imin,imax do
			local q=get_raw_qcode(ni,nj)
			local h2,h3=get_height(ni+1,nj+1),get_height(ni+1,nj)
			local y2,y3=flr(v1[2]-w1*h2),flr(v0[2]-w0*h3)

			-- in screen tile?
			if x3>0 then
				-- left/right cliping
				if i==imin then
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
							
				local c_hi,c_lo,c_dither=get_q_colors(q)

				local strip=(nj%4<2) and 0 or 1
				strip+=((ni%4<2) and 0 or 1)
				c_hi,c_lo=shade(1,c_hi),shade(1,c_lo)

				fillp(dither_pat2[strip+1])

				local q_code=band(q,0xff)
				if q_code==1 or q_code==4 then
					trifill(x0,y0,x2,y2,x1,y1,c_hi)
					trifill(x0,y0,x2,y2,x3,y3,c_lo)
				elseif q_code==9 then			
					draw_tex_quad({x0,y0,0,w0},{x1,y1,0,w1},c_hi,c_lo)
				else
					trifill(x1,y1,x3,y3,x0,y0,c_hi)
					--trifill(x1,y1,x3,y3,x2,y2,c_lo)					
				end
    							
				-- restore values (for clipping)
				x0,y0,x3,y3=xx0,yy0,xx3,yy3
				x1,y1,x2,y2=xx1,yy1,xx2,yy2
			end
					
			-- no need to go further, tile is not visible
			if(x0>127) break
						
			x0,y0,x1,y1=x3,y3,x2,y2
			h0,h1=h3,h2
			x2+=dw1
			x3+=dw0

			ni+=1
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
	
	-- game logic update
	zbuf_filter(actors)
	zbuf_filter(parts)
	
	if plyr then
				-- do not track dead player
		if not plyr.disabled then
			-- update cam
			local lookat=v_clone(plyr.pos)
			-- keep altitude
			lookat[2]=plyr.pos[2]+2
			cam:track(lookat,0.15)
		end
	end	
end

function _draw()
	cls(0)

	zbuf_sort()
	draw_ground()
	
	print(stat(1),2,2,7)
end

-- main
	-- temp array to store the 64x64 noise map
local get_noise=function(i,j)
	return noise[band(i,0x3f)+64*band(j,0x3f)+1]
end
-- returns whether value is above a given level
local is_solid=function(i,j,level,margin)
	i-=32
	j-=32
	local d=sqrt(i*i+j*j)
	return abs(d-level)<=margin and 1 or 0
end
-- converts four corners into a single lookup index
-- cf: https://en.wikipedia.org/wiki/marching_squares
local marching_code=function(i,j,level,margin)
	return
	8*is_solid(i,j,level,margin)+
	4*is_solid(i+1,j,level,margin)+
	2*is_solid(i+1,j+1,level,margin)+
	is_solid(i,j+1,level,margin)
end
-- q binary layout
-- 0x0f00: dither pattern
-- 0x00ff: q code
-- 0x0.00ff: lo color
-- 0x0.ff00: hi color
local set_q_colors=function(q,lo,hi,n)
	return bor(q,bor(shl(n,8),bor(shr(lo,16),shr(hi,8))))
end

function _init()
	
	local idx_offsets=json_parse'[0,1,128,129]'
	local q_codes=json_parse'[[0,0,0,0],[0,0,1,0],[0,0,0,2],[0,0,0x45,0x45],[0,4,0,0],[2,0,0,8],[0,0x15,0,0x15],[2,5,5,5],[8,0,0,0],[0x35,0,0x35,0],[5,1,4,5],[5,1,5,5],[0x25,0x25,0,0],[5,5,5,8],[5,5,4,5],[5,5,5,5]]'	
		
	for i=0,63 do
		for j=0,63 do
			noise[band(i,0x3f)+64*band(j,0x3f)+1]=0
		end
	end
	
	-- create multiple layers
	--[[
	local layers=json_parse'[{"level":16,"margin":2,"hi":2,"lo":1},{"level":16,"margin":1.5,"hi":3}]'

	for l=1,#layers do
		local layer=layers[l]
		for j=0,63 do
			for i=0,63 do
				local q=marching_code(i,j,layer.level,layer.margin)
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
					prev_q=band(0xff,prev_q or 5)
					if prev_q==5 then
						if q==0 then
							hi,lo=lo,lo
						elseif band(0xf,q)==5 then
							hi,lo=hi,hi
						elseif q==1 or q==8 then
							hi,lo=hi,lo
						elseif q==4 or q==2 then
							hi,lo=lo,hi
						end
						q=set_q_colors(q,hi,lo,1)
						qmap[k]=q
					end
				end
			end
		end
	end
	]]

	cam=make_cam(96)
	plyr=make_actor("plyr",{0,0,0})
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
				add(f.uv,{unpack_int(),unpack_int()})
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
__gfx__
1ca9b34500015100000000007700770077007700eeeeeeeeeee77777eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
0000000001dc7700010000007700770077007700eeeeeeee77710007ee9994eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
4950000012e77700520000000077007700770077eeeeee7710700007e999994eee99eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
000000003333b700130000000077007700770077eeeee71000100000e999994eee94eeeeeeeeeeeb5eeeeeeeeeeeeeeb0eeeeeeeeeeeeee00eeeeeee00000000
000000002497a700240000007700770077007700eeee700000000000e499944eeeeeeeeeeeeeebb3355eeeeeeeeeebb3500eeeeeeeeee665500eeeee00000000
0000000015d76700150000007700770077007700eee7070000000000e444444eeeeeeeeeeeebb33333355eeeeeebb33355500eeeeee6655555500eee00000000
0000000015676700540000000077007700770077ee71007000000000ee4444eeeeeeeeeeebb333333333355eebb333335555500ee66555555555500e00000000
0000000015566700670000000077007700770077ee70000000000000eeeeeeeeeeeeeeeeb333333333333335b333333355555550b33333333333333000000000
5c0000002288ee00280000000000000000000000e710000000000000eeeeeeee888222119bb33333333335549bb33333555550049bb333333333355400000000
00000000499ffa00540000000000000000000000e700000000000000ee9994ee99999999999bb33333355444999bb33355500444999bb3333335544400000000
0000000099aaa7009a0000000000000000000000e771000000000000e999994eaa99442149999bb33554444149999bb35004444149999bb33554444100000000
0000000055533b003b00000000000000000000007100000000000000e999444ebb335511e449999b5444411ee449999b0444411ee449999b5444411e00000000
000000001dcc7c001c00000000000000000000007000000000000000e4944eeecc551111eee4499944411eeeeee4499944411eeeeee4499944411eee00000000
00000000115dd6001d00000000000000000000007000000000000000e444eeeedd225511eeeee449411eeeeeeeeee449411eeeeeeeeee449411eeeee00000000
000000001122ee002e00000077777777777777777000000000000000ee44eeeeeedd5511eeeeeee41eeeeeeeeeeeeee41eeeeeeeeeeeeee41eeeeeee00000000
0000000022eeff00ef00000077777777777777777770000000000000eeeeeeeeffdd5511eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
0000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
0000000000000000000000000000000770000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
0000000000000000000000000000000770000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
0000000000000000000000000000000770000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee60eeeeeeeeeeeeee65eeeeeeeeeeeeeeb5eeeeeee00000000
0000000000000000000000000000000770000000eeeeeeeeeeeeeeeeeeeeeeee00eeeeeeeeeee665500eeeeeeeeee665355eeeeeeeeeebb3355eeeee00000000
0000000000000000000000000000000770000000eeeeeeeeeeeeeeeeeeeeee00bb0eeeeeeee6655555500eeeeee6655533355eeeeeebb33333355eee00000000
00000000000000000000000000000000000000000000000000000000eeeee0b38bb0eeeee66555555555500ee66555553333355eebb333333333355e00000000
00000000000000000000000000000000000000000999999999999990eeee0bbbbb330eee65555555555555506555555533333335b33333333333333000000000
00000000777770000000000000000000000000000444444444444440eee0b8bbb3230eee96655555555550049665555533333554966555555555500400000000
00000000666660000000000000000007700000000005500000055000eee03bb33330eeee99966555555004449996655533355444999665555550044400000000
0098800056665000000000000000000770000000ee0440eeee0440eeeeee033300011eee49999665500444414999966535544441499996655004444100000000
00488880066600000000000000000007700000000999999999999990eee110001111eeeee44999960444411ee44999965444411ee44999960444411e00000000
00488800056500000000000000000007700000000444444444444440eeee1111eeeeeeeeeee4499944411eeeeee4499944411eeeeee4499944411eee00000000
00400000005000000000000000000007700000000005500000055000eeeeeeeeeeeeeeeeeeeee449411eeeeeeeeee449411eeeeeeeeee449411eeeee00000000
0040000000000000000000000000000000000000ee0440eeee0440eeeeeeeeeeeeeeeeeeeeeeeee41eeeeeeeeeeeeee41eeeeeeeeeeeeee41eeeeeee00000000
00000000000000000000000000000000000000001110011111100111eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
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
206040207021f141100156f67456d874b9f674b9d87456f69956d899b9f699b9d89956f6fb5698fbb9f6fbb998fbb6b9e559b9e5b6b97859b978d04010206050
4000030002720272035607e74030402010400071a071a00000000874e7406080c0a04012001271e271e20008cab84030708040400003720372020002b907e740
90a0c0b0409300e200e271937108fbc7405060a09040720372029322930356cad7408070b0c0407202720393039322b9cad7401090b03040b1b282b282d2b1d2
0838f640d0e001f04011001171917191000837b940d02040e0401100a000a071117108354940e0408001409091000272020291891749408060f0014012711200
9100917108094940d0f06020409091029172020002861749d0060808080806080a380a080808080a0608080a0808080608080a0808b9f6e9c80808994926c808
b040207021f14110d0d0a132108056f674b9f674b9b97456b97456f6fbb9f6fbb9b9fb56b9fb60401040805000563858406070302000b9385840508070600008
fb584010506020000838f64040307080000838b9402030401000087458600608080a080808080a080608080a0808080600000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
bb88856599777777777777777777777777777777777777788756566bb00000000000000000000000000000000000000000000000000000000000000000000000
b888856599766666777777777711666667666677777777788756666bb00000000000000000000000000000000000000000000000000000000000000000000000
b8888565657666667777777777116666677777666666667aa756776bb00000000000000000000000000000000000000000000000000000000000000000000000
b8888565657666667777777777116666677777777777776aa756776bb00000000000000000000000000000000000000000000000000000000000000000000000
bb0555656586666677777777771166666777777887667765a756776bb00000000000000000000000000000000000000000000000000000000000000000000000
bb0555656c866666777777777711666665777788877766656756556bb00000000000000000000000000000000000000000000000000000000000000000000000
bb505565cc866666777777777711666665777780877777656756666bb00000000000000000000000000000000000000000000000000000000000000000000000
bb055565cc866666777777777711666665777787877777656756556bb00000000000000000000000000000000000000000000000000000000000000000000000
bb505565ca766666777777777711666665777788877777656756666bb00000000000000000000000000000000000000000000000000000000000000000000000
bb056565aa766666777777777711666665777778877777656756556bb00000000000000000000000000000000000000000000000000000000000000000000000
bb566565a1766666777777777711666665777777777777765756666bb00000000000000000000000000000000000000000000000000000000000000000000000
bb50656511766666777777777711666665777777778877567756556bb00000000000000000000000000000000000000000000000000000000000000000000000
bb56656511766666777777777711666665777778888877656756666bb00000000000000000000000000000000000000000000000000000000000000000000000
bb05656518766666777777777711666665777788111177656756556bb00000000000000000000000000000000000000000000000000000000000000000000000
bb5055658876666677777777771166666577778111aa77656756666bb00000000000000000000000000000000000000000000000000000000000000000000000
bb05556585788666777777777711666665777781aacc77656756556bb00000000000000000000000000000000000000000000000000000000000000000000000
bb50556565788866777777777711666665777781acc777656756666bb00000000000000000000000000000000000000000000000000000000000000000000000
bb05556565780866777777777711666665777781ac7766656756556bb00000000000000000000000000000000000000000000000000000000000000000000000
bb05556565787866777777777711666667777781a7667765a756776bb00000000000000000000000000000000000000000000000000000000000000000000000
b8888565657888667777777777116666677777777777776aa756776bb00000000000000000000000000000000000000000000000000000000000000000000000
b8888565657886667777777777116666677777666666667aa756776bb00000000000000000000000000000000000000000000000000000000000000000000000
b888856599766666777777777711666667666677777777788756666bb00000000000000000000000000000000000000000000000000000000000000000000000
bb88856599777777777777777777777777777777777777788756566bb00000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbbb7777777777777777777777777bbbbbbbbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000
bbbbbbb777777666666676666666666667bbbbbbbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000
bbbbbb777887666666667666666666666676bbbbbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000
bbbbb7778aa86666666676677777666666676bbbbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000
bbbb77778aa866666666766766666666666676bbbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000
bbb777777887666666667667777766666666676bbbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000
bb77777777776666666676666666666666666776bbbbbbbbbbbbbbbbb00000000000000000000000000000000000000000000000000000000000000000000000
b77777777777777777777777777777777777777777777777777bbbbbb00000000000000000000000000000000000000000000000000000000000000000000000
777888888888888888777777777777777777888888888888888777bbb00000000000000000000000000000000000000000000000000000000000000000000000
77888811111111111117777557555555557771111111111188887777700000000000000000000000000000000000000000000000000000000000000000000000
558881111aaaaaaaaaaa77755575755757777aaaaaaaa11118888788800000000000000000000000000000000000000000000000000000000000000000000000
86688111aaaccccc6656677575757575557777ccccccaa111188878a800000000000000000000000000000000000000000000000000000000000000000000000
86660055500ccccc66566777777777777777777cccccca0055500778700000000000000000000000000000000000000000000000000000000000000000000000
556055555550ccccc66566777777777777777777ccccc05555555077700000000000000000000000000000000000000000000000000000000000000000000000
6605555555550cccc66566777878777787877777cccc055555555505500000000000000000000000000000000000000000000000000000000000000000000000
6655555755555cccccccccc778888888878777777ccc555557555556600000000000000000000000000000000000000000000000000000000000000000000000
6655556665555cccccccccc777777777777777777ccc555566655556600000000000000000000000000000000000000000000000000000000000000000000000
6655576667555ccccccccccc777777777777777777cc555766675556500000000000000000000000000000000000000000000000000000000000000000000000
b655556665555ccccccccccc111111111111111111cc555566655556600000000000000000000000000000000000000000000000000000000000000000000000
bb55555755555cccccccccccc111111111111111111c555557555556600000000000000000000000000000000000000000000000000000000000000000000000
bbb555555555055555555555555555555555555555550555555555bbb00000000000000000000000000000000000000000000000000000000000000000000000
bbbb5555555bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb5555555bbbb00000000000000000000000000000000000000000000000000000000000000000000000
bbbbb55555bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb55555bbbbb00000000000000000000000000000000000000000000000000000000000000000000003
__map__
3000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000200090765006650066500665006650306500565005650056500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
