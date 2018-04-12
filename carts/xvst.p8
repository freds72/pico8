pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- xwing vs. tie figther
-- by freds72

-- game globals
local time_t,time_dt=0,0
local good_side,bad_side,any_side,no_side=0x1,0x2,0x0,0x3
local before_update,after_draw={},{}

-- register json context here
local _tok={
 ['true']=true,
 ['false']=false}
function nop() end
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
  if(err_if_missing) assert('delimiter missing')
  return pos,false
 end
 return pos+1,true
end
local function parse_str_val(str, pos, val)
	val=val or ''
	if pos>#str then
		assert('end of input found while parsing string.')
	end
	local c=sub(str,pos,pos)
	if(c=='"') return _g[val] or val,pos+1
	return parse_str_val(str,pos+1,val..c)
end
local function parse_num_val(str,pos,val)
	val=val or ''
	if pos>#str then
		assert('end of input found while parsing string.')
	end
	local c=sub(str,pos,pos)
	-- support base 10, 16 and 2 numbers
	if(not match(c,"-xb0123456789abcdef.")) return tonum(val),pos
	return parse_num_val(str,pos+1,val..c)
end
-- public values and functions.

function json_parse(str, pos, end_delim)
	pos=pos or 1
	if(pos>#str) assert('reached unexpected end of input.')
	local first=sub(str,pos,pos)
	if match(first,"{[") then
		local obj,key,delim_found={},true,true
		pos+=1
		while true do
			key,pos=json_parse(str, pos, table_delims[first])
			if(key==nil) return obj,pos
			if not delim_found then assert('comma missing between table items.') end
			if first=="{" then
				pos=skip_delim(str,pos,':',true)  -- true -> error if missing.
				obj[key],pos=json_parse(str,pos)
			else
				add(obj,key)
			end
			pos,delim_found=skip_delim(str, pos, ',')
	end
	elseif first=='"' then
		-- parse a string (or a global object)
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
		assert('invalid json token')
	end
end

-- 0: chase
-- 1: cockpit
-- 2: orbit
local cam_mode,cam=0
local actors,npc_count={},0
local parts={}
local scores,last_score={},0
local cur_screen
-- 0: space
-- 1: surface
-- 2: trenches
local game_mode=1
local start_screen={
	starting=false
}
local game_screen={
	starting=false
}
local gameover_screen={}
local bench_screen={
	angle=0.12,
	dist=-4,
	sel_actor=0
}
function nop() end

-- futures
function futures_update(futures)
	futures=futures or before_update
	for _,f in pairs(futures) do
		if not coresume(f) then
			del(futures,f)
		end
	end
end
function futures_add(fn,futures)
	return add(futures or before_update,cocreate(fn))
end
function wait_async(t,fn)
	local i=1
	while i<=t do
		if fn then
			if not fn(i) then
				return
			end
		end
		i+=time_dt
		yield()
	end
end
local shkx,shky=0,0
function screen_shake(u,v,pow)
	shkx=min(4,shkx+pow*u)
	shky=min(4,shky+pow*v)
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
local drawables={}
function zbuf_clear()
	drawables={}
end
function zbuf_draw()
	local objs={}
	for _,d in pairs(drawables) do
		local p=d.pos
		local x,y,z,w=cam:project(p[1],p[2],p[3])
		if z>0 then
			add(objs,{obj=d,key=z,x=x,y=y,z=z,w=w})
		end
	end
	-- z-sorting
	sort(objs)
	-- actual draw
	for i=1,#objs do
		local d=objs[i]
		d.obj:draw(d.x,d.y,d.z,d.w)
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
function smoothstep(t)
	t=mid(t,0,1)
	return t*t*(3-2*t)
end

function padding(i,n)
	local txt=tostr(i)
 -- padding
 for i=1,n-#txt do
 	txt="0"..txt
 end
 return txt
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

-- models
local all_models={
	title={
		c=10
	},
	deathstar={
		c=3
	},
	turret={
		c=8,
		wp={
			dmg=1,
			dly=12,
			pos={{-0.2,0.8,0.65},{0.2,0.8,0.65}},
			n={{0,0,1},{0,0,1}}
		}
	},
	xwing={
		c=7,
		r=0.8,
		proton_wp={
			dmg=4,
			dly=60,
			pos={0,-0.4,1.5},
			n={0,0,1}
		},
		wp={
		 dmg=1,
			dly=8,
   pos={{2,1,1.6},{2,-1,1.6},{-2,-1,1.6},{-2,1,1.6}},
   n={}
  }
	},
	tie={
		c=5,
		-- collision radius
		r=1,
		wp={
			dmg=2,
			dly=24,
			pos={{0.7,-0.7,0.7},{-0.7,-0.7,0.7}},
			n={{0,0,1},{0,0,1}}
		}
	}
}
function sqr_dist(a,b)
	local dx,dy,dz=b[1]-a[1],b[2]-a[2],b[3]-a[3]
	if abs(dx)>128 or abs(dy)>128 or abs(dz)>128 then
		return 32000
	end
	return dx*dx+dy*dy+dz*dz
end

function make_rnd_v(scale)
	local v={rnd()-0.5,rnd()-0.5,rnd()-0.5}
	v_normz(v)
	return {scale*v[1],scale*v[2],scale*v[3]}
end
function make_rnd_pos_v(a,rng)
	local p=make_rnd_v(8)
	p[3]+=rng
	local d,v=0
	while d==0 do
		v=make_rnd_v(4)
		v_plus_v(v,p,-1)
		d=v_normz(v)
	end
	m_x_v(a.m,p)
	return p,v
end

function make_v_cross(a,b)
	local ax,ay,az=a[1],a[2],a[3]
	local bx,by,bz=b[1],b[2],b[3]
	return {ay*bz-az*by,az*bx-ax*bz,ax*by-ay*bx}
end
local v_fwd,v_right,v_up={0,0,1},{1,0,0},{0,1,0}

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
function v_clamp(v,l)
	local d=v[1]*v[1]+v[2]*v[2]+v[3]*v[3]
	if d>l*l then
		d=sqrt(d)
		for i=1,3 do
			v[i]=l*v[i]/d
		end
	end
end
function v_scale(v,scale)
	v[1]*=scale
	v[2]*=scale
	v[3]*=scale
end
function v_plus_v(v,dv,scale)
	scale=scale or 1
	v[1]+=scale*dv[1]
	v[2]+=scale*dv[2]
	v[3]+=scale*dv[3]
end
function in_cone(p,t,fwd,angle,rng)
	local v=v_clone(t)
	v_plus_v(v,p,-1)
	-- close enough?
	if v_dot(v,v)<rng*rng then
		v_normz(v)
		-- in cone?
		return v_dot(fwd,v)>angle
	end
	return false
end

-- matrix functions
function m_x_v(m,v)
	local x,y,z=v[1],v[2],v[3]
	v[1],v[2],v[3]=m[1]*x+m[5]*y+m[9]*z+m[13],m[2]*x+m[6]*y+m[10]*z+m[14],m[3]*x+m[7]*y+m[11]*z+m[15]
end
-- 3x3 matrix mul (orientation only)
function o_x_v(m,v)
	local x,y,z=v[1],v[2],v[3]
	v[1]=m[1]*x+m[5]*y+m[9]*z
	v[2]=m[2]*x+m[6]*y+m[10]*z
	v[3]=m[3]*x+m[7]*y+m[11]*z
end
function m_x_xyz(m,x,y,z)
	return {
		m[1]*x+m[5]*y+m[9]*z+m[13],
		m[2]*x+m[6]*y+m[10]*z+m[14],
		m[3]*x+m[7]*y+m[11]*z+m[15]}
end
function make_m(x,y,z)
	local m={}
	for i=1,16 do
		m[i]=0
	end
	m[1],m[6],m[11],m[16]=1,1,1,1
	m[13],m[14],m[15]=x or 0,y or 0,z or 0
	return m
end
function make_m_toward(z,up)
 	local x=make_v_cross(up,z)
	-- aligned?
	if v_dot(x,x)<0.001 then
		-- up and z //
		if abs(up[3])==1 then
			z[1]+=0.01
		else
			z[3]+=0.01
		end
		v_normz(z)
		x=make_v_cross(up,z)
	end
		
	v_normz(x)
	local y=make_v_cross(z,x)
	v_normz(y)
 
	return { 
		x[1],x[2],x[3],0,
		y[1],y[2],y[3],0,
  		z[1],z[2],z[3],0,
		0,0,0,1}
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
function make_q_from_v(a,b)
	local r=v_dot(a,b)
	local v
	if r<0.001 then
		r=0
		if abs(a[1])>abs(b[3]) then
			v={-a[2],a[1],0}
		else
			v={0,-a[3],a[2]}
		end
	else
		v=make_v_cross(a,b)
	end
	local q={v[1],v[2],v[3],r}
	q_normz(q)
	return q
end
function q_normz(q)
	local d=1/sqrt(v_dot(q,q)+q[4]*q[4])
	for i=1,4 do
		q[i]*=d
	end	
end
function q_clone(q)
	return {q[1],q[2],q[3],q[4]}
end
function q_x_q(a,b)
	local qax,qay,qaz,qaw=a[1],a[2],a[3],a[4]
	local qbx,qby,qbz,qbw=b[1],b[2],b[3],b[4]
        
	a[1]=qax*qbw+qaw*qbx+qay*qbz-qaz*qby
	a[2]=qay*qbw+qaw*qby+qaz*qbx-qax*qbz
	a[3]=qaz*qbw+qaw*qbz+qax*qby-qay*qbx
	a[4]=qaw*qbw-qax*qbx-qay*qby-qaz*qbz
end
function v_x_q(v,q)
	local x,y,z=v[1],v[2],v[3]
	local qx,qy,qz,qw=q[1],q[2],q[3],q[4]
	-- calculate quat*vector
	local ix=qw*x+qy*z-qz*y
	local iy=qw*y+qz*x-qx*z
	local iz=qw*z+qx*y-qy*x
	local iw=-qx*x-qy*y-qz*z
	
	-- calculate result*inverse quat	
	return {
		ix*qw+iw*-qx+iy*-qz-iz*-qy,
		iy*qw+iw*-qy+iz*-qx-ix*-qz,
		iz*qw+iw*-qz+ix*-qy-iy*-qx}
end
function m_from_q(q)

		local te={}

		local x,y,z,w=q[1],q[2],q[3],q[4]
		local x2,y2,z2=x+x,y+y,z+z
		local xx,xy,xz=x*x2,x*y2,x*z2
		local yy,yz,zz=y*y2,y*z2,z*z2
		local wx,wy,wz=w*x2,w*y2,w*z2

		te[1],te[5],te[9]=1-(yy+zz),xy-wz,xz+wy
		te[2],te[6],te[10]=xy+wz,1-(xx+zz),yz-wx
		te[3],te[7],te[11]=xz-wy,yz+wx,1-(xx+yy)

		-- last column
		te[4],te[8],te[12]=0,0,0

		-- bottom row
		te[13],te[14],te[15],te[16]=0,0,0,1

		return te
end
function m_clone(m)
	local c={}
	for i=1,16 do
		c[i]=m[i]
	end
	return c
end

-- only invert 3x3 part
function m_inv(m)
	m[2],m[5]=m[5],m[2]
	m[3],m[9]=m[9],m[3]
	m[7],m[10]=m[10],m[7]
end
-- same as above, inline matrix invert
function m_inv_x_v(m,v)
	local x,y,z=v[1]-m[13],v[2]-m[14],v[3]-m[15]
	v[1]=m[1]*x+m[2]*y+m[3]*z
	v[2]=m[5]*x+m[6]*y+m[7]*z
	v[3]=m[9]*x+m[10]*y+m[11]*z
end

local dither_pat=json_parse('[0b1111111111111111,0b0111111111111111,0b0111111111011111,0b0101111111011111,0b0101111101011111,0b0101101101011111,0b0101101101011110,0b0101101001011110,0b0101101001011010,0b0001101001011010,0b0001101001001010,0b0000101001001010,0b0000101000001010,0b0000001000001010,0b0000001000001000,0b0000000000000000]')

local ground_colors={5,1,5,1}
local ground_scale=4
function draw_ground(self)
	if(cam.pos[2]<0) return
	
	local v={}
	local x,z=plyr.pos[1],plyr.pos[3]
	local dx,dy=x%ground_scale,z%ground_scale
	
	local c=1
	local imax=game_mode==2 and 3 or -1
	for j=-16,16,ground_scale do
		local jj=j-dy+z
		for i=-16,-2,ground_scale do
			local ii=i-dx+x
			local x,y,z=cam:project(ii,0,jj)
			if z>0 then
				pset(x,y,ground_colors[flr(ii+jj)%2+1])
			end
			c+=1
		end
		for i=imax,16,ground_scale do
			local ii=i-dx+x
			local x,y,z=cam:project(ii,0,jj)
			if z>0 then
				pset(x,y,ground_colors[flr(ii+jj)%2+1])
			end
			c+=1
		end
	end
end
local turrets={}
function make_turret(i,j,y,scalex,scaley)
	scalex,scaley=scalex or ground_scale,scaley or ground_scale
	
	local x,z=i*scalex,j*scaley
	y=y or 0
	local t={
		scalex=scalex,
		scaley=scaley,
		pos={x,y,z},
		m=make_m(x,y,z),
		model=all_models.turret,
		side=bad_side,
		fire_t=0,
		laser_i=0,
		fire=make_laser,
		update=update_turret,
		draw=draw_actor
	}
	turrets[i+j*128]=t
	return t
end
function make_junk(i,j,model)
	local x,y,z=i*ground_scale,0,j*ground_scale
	local t={
		pos={x,y,z},
		m=make_m(x,y,z),
		side=any_side,
		model=model,
		update=function(self,i,j)
			self.pos[1],self.pos[3]=i*ground_scale,j*ground_scale
			local m=self.m
		m[13],m[15]=self.pos[1],self.pos[3]
		end,
		draw=draw_actor
	}
	turrets[i+j*128]=t
	return t
end

function init_ground()
	for i=0,127 do
		for j=0,127 do
			local r=rnd()
			if r>0.99 then
				make_turret(i,j)
			elseif r>0.95 then
				make_junk(i,j,all_models.junk1)
			elseif r>0.9 then
				make_junk(i,j,all_models.junk2)
			end
		end
	end
end

local ground_actors={}
function update_ground()
	ground_actors={}
	local i0,j0=flr(plyr.pos[1]/ground_scale),flr(plyr.pos[3]/ground_scale)
	for i=i0-6,i0+6 do
		local cx=(i%128+128)%128
		for j=j0-6,j0+6 do
			local cy=(j%128+128)%128
			local t=turrets[cx+cy*128]
			if t then
				t:update(i,j)
				add(drawables,t)
				add(ground_actors,t)
			end
		end
	end
end

function update_turret(self,i,j)
	self.pos[1],self.pos[3]=i*self.scalex,j*self.scaley
	local dx,dy=self.pos[1]-plyr.pos[1],self.pos[3]-plyr.pos[3]
	-- in range?
	local angle,m=1,self.m
	if dx*dx+dy*dy<64 then
		angle=atan2(dx,dy)-0.25
		local q=make_q(v_up,angle)
		m=m_from_q(q)
		self.m=m
	end
	m[13],m[14],m[15]=self.pos[1],self.pos[2],self.pos[3]
	
	if abs(angle)<0.2 and self.fire_t<time_t then
		self:fire(plyr.pos)
		self.fire_t=time_t+self.model.wp.dly
	end
	
	return true
end

local trench_scale=6
function make_trench(i)
	local x,y,z=0,-3,i*trench_scale
	local t={
		pos={x,y,z},
		m=make_m(x,y,z),
		side=no_side,
		model=all_models.trench1,
		update=function(self)
			local dz=plyr.pos[3]-plyr.pos[3]%trench_scale
			local z=i*trench_scale+dz
			self.pos[3],self.m[15]=z,z
			return true
		end,
		draw=draw_actor
	}
	add(actors,t)
end
function init_trench(n)
	for i=-n,n do
		make_trench(i)
	end
	for i=-1,1 do
		local ii=(i+128)%128
		for j=0,127 do
			local r=rnd()
			if r>0.9 then
				make_turret(ii,j,-6,2,4)
			end
		end
	end
end
function update_trench()
	ground_actors={}
	local i0,j0=flr(plyr.pos[1]/2),flr(plyr.pos[3]/4)
	for i=i0-2,i0+2 do
		local cx=(i%128+128)%128
		for j=j0-5,j0+5 do
			local cy=(j%128+128)%128
			local t=turrets[cx+cy*128]
			if t then
				t:update(i,j)
				add(drawables,t)
				add(ground_actors,t)
			end
		end
	end
end

local debug_vectors=false
function draw_actor(self,x,y,z,w)
	draw_model(self.model,self.m,x,y,z,w)
	-- debug
	--[[
	if debug_vectors then
		if self.dist then
			print(self.dist,x,y-8,7)
		end
 	if self.target then 
 		local c=12
 		if band(self.side,self.target.side)==0 then
	 		c=8
 		end
 		local pos=v_clone(self.target.pos)
 		v_plus_v(pos,self.pos,-1)
	 	draw_vector(self.m,self.pos,pos,c,"tgt")
 	end
 	if self.follow then
 		local m=self.m
 		local pos=v_clone(self.follow)
 		o_x_v(m,pos)
 		draw_vector(m,self.pos,pos,10,"f")
 	end
 	if self.avoid then
 		local m=self.m
 		local pos=v_clone(self.avoid)
 		o_x_v(m,pos)
 		draw_vector(m,self.pos,pos,1,"a")
 	end
 	if self.wander then
 		local m=self.m
 		local pos=v_clone(self.wander)
 		o_x_v(m,pos)
 		draw_vector(m,self.pos,pos,2,"w")
 	end
 end
 ]]
end
function draw_vector(m,pos,v,c,s)
	local x0,y0,z0,w=cam:project(pos[1],pos[2],pos[3])
	local x1,y1,z1,w=cam:project(pos[1]+v[1],pos[2]+v[2],pos[3]+v[3])
	if z0>0 and z1>0 then
 	line(x0,y0,x1,y1,c)
 	if s then
 		local dx,dy=x1-x0,y1-y0
 		local d=sqrt(dx*dx+dy*dy)
 		dx/=d
 		dy/=d
 		print(s,x1+4*dx,y1-4*dy,c)
 	end
	end
end

local draw_session_id=0

-- unpack models
local mem=0x2000
function compute_cp(model)
	model.cp={}
	for i=1,#model.f do
		local f=model.f[i]
		add(model.cp,v_dot(model.n[i],model.v[f[1]]))
	end
end
function unpack_int()
	local i=peek(mem)
	mem+=1
	return i
end
function unpack_float()
	local f=(peek(mem)-128)/32
	mem+=1
	return f
end
-- valid chars for model names
local itoa='_0123456789abcdefghijklmnopqrstuvwxyz'
function unpack_string()
	local n=unpack_int()
	s=""
	for i=1,n do
		local c=unpack_int()
		s=s..sub(itoa,c,c)
	end
	return s
end
function unpack_models()
	-- how many models?
	local n=unpack_int()
	for m=1,n do
 	local name=unpack_string()
 	local model={}
 	-- vertices
 	n=unpack_int()
 	model.v={}
 	for i=1,n do
 		local v={unpack_float(),unpack_float(),unpack_float()}
 		add(model.v,v)
 	end	
 	-- faces
 	n=unpack_int()
 	model.f={}
 	for i=1,n do
 		local f={unpack_int(),unpack_int()}
 		for k=1,f[2] do
 			add(f,unpack_int())
 		end
 		add(model.f,f)
 	end
 	-- normals
 	n=unpack_int()
 	model.n={}
 	for i=1,n do
 		local v={unpack_float(),unpack_float(),unpack_float()}
 		add(model.n,v)
 	end
 	
 	-- edges
 	n=unpack_int()
 	model.e={}
 	for i=1,n do
 		local e={
 			unpack_int(), -- start
 			unpack_int(), -- end
 			unpack_int()==1 and true or -1
 		}
 		add(model.e,e)
 	end
 	
 	compute_cp(model)
		-- merge with existing model
		all_models[name]=clone(model,all_models[name])
	end
end
unpack_models()

function draw_model(model,m,x,y,z,w)
	draw_session_id+=1

	color(model.c or 1)
	-- camera distance ditheting
	if w then
		local d=lerp(1-smoothstep(w/2),1,#dither_pat)
		fillp(dither_pat[flr(d)]+0b0.1)
	end
	
	-- cam pos in object space
	local cam_pos=v_clone(cam.pos)
	m_inv_x_v(m,cam_pos)

	-- faces
	local f,n
	for i=1,#model.f do
		f,n=model.f[i],model.n[i]
		-- viz calculation
		local d=n[1]*cam_pos[1]+n[2]*cam_pos[2]+n[3]*cam_pos[3]
		if d>=model.cp[i] then
			for k=1,f[2] do
				model.e[f[k+2]][3]=draw_session_id
			end
		end
	end
	-- edges
	local p={}
	for _,e in pairs(model.e) do
		if e[3]==true or e[3]==draw_session_id then
			local ak,bk=e[1],e[2]
			local a,b=p[ak],p[bk]
			if not a then
				v=model.v[ak]
				x,y,z=v[1],v[2],v[3]
				--m_x_v(m,v)
				x,y,z,w=cam:project(m[1]*x+m[5]*y+m[9]*z+m[13],m[2]*x+m[6]*y+m[10]*z+m[14],m[3]*x+m[7]*y+m[11]*z+m[15])
				p[ak]={x,y,z,w}
				a=p[ak]
			end
			if not b then
				v=model.v[bk]
				x,y,z=v[1],v[2],v[3]
				--m_x_v(m,v)
				x,y,z,w=cam:project(m[1]*x+m[5]*y+m[9]*z+m[13],m[2]*x+m[6]*y+m[10]*z+m[14],m[3]*x+m[7]*y+m[11]*z+m[15])
				p[bk]={x,y,z,w}
				b=p[bk]
			end
			if(a[3]>0 and b[3]>0) line(a[1],a[2],b[1],b[2])
		end
	end
	fillp()
end

function die_plyr(self)
	make_blast(plyr.pos)
	-- clear
	for s in all(scores) do
		s.islast=false
	end
	add(scores,{key=plyr.score,islast=true})
	sort(scores)
	if #scores>5 then
		scores[6]=nil
	end
	-- save scores
	dset(0,#scores)
	for i=1,#scores do
		dset(i,scores[i].key)
	end
	last_score=plyr.score
	-- 
	del(actors,plyr)
	plyr=nil
	cur_screen=gameover_screen
	futures_add(function()
		wait_async(240,function()
			if btnp(4) or btnp(5) then
				return false
			end
			return true
		end)
		-- "eat" btnp
		yield()
		cur_screen=start_screen
	end)
end

function die_actor(self)
	make_blast(self.pos)
	self.disabled=true
	npc_count-=1
	del(actors,self)
end

-- offset: position relative to other pos
function follow(pos,other,offset)
	-- offset into world position
	local v=v_clone(offset)
	m_x_v(other.m,v)
	-- line to target
	v_plus_v(v,pos,-1)
	return v
end
function avoid(self,pos,dist)
	local v={0,0,0}
	local n,d2=0,dist*dist
	for _,a in pairs(actors) do
		if a!=self then
			local p=v_clone(a.pos)
			v_plus_v(p,pos,-1)
			local d=v_dot(p,p)
			if d>0 and d<d2 then
				v_plus_v(v,p,-1)
			end
		end
	end
	return v
end
function seek(self,r)
	local fwd={self.m[9],self.m[10],self.m[11]}
	local d2=r*r
	for _,a in pairs(actors) do
		if band(a.side,self.side)==0 and in_cone(self.pos,a.pos,fwd,0.5,r) then
			 -- avoid loops
			if a.target!=self then
				return a
			end
		end
	end
end

-- return a pos in self space
function wander(self)
	local p=make_rnd_v(1)
	p[3]+=15
	return p
end

function update_flying_npc(self)
	-- if npc still in range
	--[[
	if sqr_dist(self.pos,plyr.pos)>16*16 then
		npc_count-=1
		return false
	end
	]]
	-- force application point 
	local acc=self.acc
	local pos={0,0,1}
	local m=self.m
	m_x_v(m,pos)
	-- forces
	local force={acc*m[9],acc*m[10],acc*m[11]}
	local can_fire=false
	local fwd={m[9],m[10],m[11]}

	if self.target then
		-- friendly: formation flight
		local target_pos={0,-4,-10}
		-- enemy: get in sight
		if band(self.target.side,self.side)==0 then
			target_pos={0,0,-15}
			can_fire=true
		end
		self.follow=follow(pos,self.target,target_pos)
		v_plus_v(force,self.follow)
	else
		-- search for target
		if self.side!=good_side then
			self.target=seek(self,24)
		end
	end
	-- nothing to track?
	if not self.target then
		if not self.wander or self.wander_t<time_t then
			-- pick a random location
			self.wander=wander(self)
			self.wander_t=time_t+120+rnd(60)
		end
		v_plus_v(force,follow(pos,self,self.wander))
	else
		-- debug
		self.wander=nil
	end
	local avf=avoid(self,pos,8)
	-- weight avoid more than follow
	v_plus_v(force,avf,4)
	
	local d=v_dot(force,force)
	-- debug
	self.dist=sqrt(sqr_dist(self.pos,plyr.pos))
	self.avoid=avf

	-- clamp acceleration
	v_clamp(force,1.2*acc)

	-- update orientation		
	v_plus_v(pos,force)
	v_plus_v(pos,self.pos,-1)
	v_normz(pos)

 -- good looking but a bit unstable
	--m=make_m_toward(pos,self.target and {self.target.m[5],self.target.m[6],self.target.m[7]} or {m[5],m[6],m[7]})
 m=make_m_toward(pos,{m[5],m[6],m[7]})	
	-- move actor using force
	v_plus_v(self.pos,force)
	
	m[13],m[14],m[15]=self.pos[1],self.pos[2],self.pos[3]
	self.m=m

	-- fire solution?
	local fwd={m[9],m[10],m[11]}
	if can_fire and self.fire_t<time_t and in_cone(self.pos,self.target.pos,fwd,0.92,24) then
 		-- must be in sight for some time
 		if self.lock_t>45 then
 			self.lock_t=45
 			self.fire_t=time_t+self.model.wp.dly
 			self:fire(self.target.pos)
 		end
 		self.lock_t+=2
	end
	-- target memory
 -- self.lock_t=max(self.lock_t-4)

	return true
end

function hit_npc(self,dmg)
	self.hp-=dmg
	if self.hp<=0 then
		self:die()
	end
end
function hit_flying_npc(self,dmg,actor)
	hit_npc(self,dmg)
	-- todo: wait a bit
	if actor==plyr then
		self.target=plyr
	end
end
		
function make_plyr(x,y,z)
	local p={
		score=0,
		hp=6,
		boost=0,
		acc=0.2,
		model=all_models.xwing,
		pos={x,y,z},
		q=make_q({0,0,1},0),
		roll=0,
		pitch=0,
		laser_i=0,
		fire_t=0,
		side=good_side,
		hit=function(self,dmg)
			self.hp-=dmg
			screen_shake(rnd(),rnd(),2)
		end,
		fire=make_laser,
		fire_proton=make_proton,
		die=die_plyr,
		draw=function(self,x,y,z,w)
			if cam_mode==1 then
				return
			end
			draw_model(self.model,self.m,x,y,z,w)
		end,
		update=function(self)
			return true
		end
	}
	add(actors,p)
	return p
end

local _id=0
local npc_xwing={
	hp=8,
	acc=0.2,
	model=all_models.xwing,
	side=good_side,
	update=update_flying_npc,
	hit=hit_npc,
}
local npc_tie={
	hp=4,
	acc=0.2,
	model=all_models.tie,
	side=bad_side,
	update=update_flying_npc,
	hit=hit_flying_npc
}
local npc_turret={
	hp=2,
	model=all_models.turret,
	side=bad_side,
	update=update_ground_npc,
	hit=hit_npc
}
local npc_junk={
	hp=1,
	rnd={model={
		all_models.junk1,
		all_models.junk2,
		all_models.junk2}},
	side=any_side,
	hit=hit_npc
}

function make_npc(p,v,src)
	npc_count+=1
	_id+=1
	local a={
		id=_id,
		pos=v_clone(p),
		q=make_q(v,0),
		wander_t=0,
		lock_t=0,
		fire_t=0,
		laser_i=0,
		fire=make_laser,
		die=die_actor,
		draw=draw_actor
	}
	-- instance
	clone(src,a)
	-- init orientation
	local m=m_from_q(a.q)
	m[13],m[14],m[15]=p[1],p[2],p[3]
	a.m=m
	return add(actors,a)
end

function make_cam(f)
	return {
		pos={0,0,3},
		focal=f,
		q=make_q(v_fwd,0),
		update=function(self)
			self.m=m_from_q(self.q)
			m_inv(self.m)
		end,
		project=function(self,x,y,z)
			-- world to view
			x-=self.pos[1]
			y-=self.pos[2]
			z-=self.pos[3]
			local v=m_x_xyz(self.m,x,y,z)
			-- distance to camera plane
			v[3]-=1
			if(v[3]<0.001) return nil,nil,-1,nil
			-- view to screen
 			local w=self.focal/v[3]
 			return 64+v[1]*w,64-v[2]*w,v[3],w
		end,
		project_v=function(self,v,x0,y0,f)
		f=f or self.focal
			-- world to view
			v_plus_v(v,self.pos,-1)
			m_x_v(self.m,v)
			-- distance to camera plane
			v[3]-=1
			if(v[3]<0.001) v[3]=-1 return
			-- view to screen
 		local w=f/v[3]
 		v[1],v[2],v[4]=x0+v[1]*w,y0-v[2]*w,w
		end
	}
end

function make_laser(self,target)
	local wp=self.model.wp
	local i=self.laser_i%#wp.pos+1
	-- rebase laser in world space
	local p=v_clone(wp.pos[i])
	m_x_v(self.m,p)
	-- direction override?
	local v
	if target then
		v=v_clone(target)
		v_plus_v(v,p,-1)
		v_normz(v) 
	else
		v=v_clone(wp.n[i])
		o_x_v(self.m,v)
	end
	self.laser_i+=1
	-- laser colors
	local c=self.side==good_side and 11 or 8
	add(parts,{
		-- laser owner
		actor=self,
		t=time_t+90,
		acc=0.8,
		pos=p,
		u=v,
		c=c,
		side=self.side,
		dmg=wp.dmg,
		update=update_blt,
		draw=draw_line_part})
	make_flash(p,c)
end

function make_proton(self,target)
	local wp=self.model.proton_wp
	-- rebase laser in world space
	local p=v_clone(wp.pos)
	m_x_v(self.m,p)
	-- fire direction in world space
	v=v_clone(wp.n)
	o_x_v(self.m,v)

	add(parts,{
		-- proton owner
		actor=self,
		target=target,
		t=time_t+90,
		duration=0,
		acc=0.6,
		pos=p,
		u=v,
		side=self.side,
		dmg=wp.dmg,
		update=update_proton,
		draw=draw_proton_part})
	make_flash(p,c)
end
function make_flash(p,c)
	return add(parts,{
		t=time_t+8,
		c=c or 7,
		r=0.4,
		dr=-0.05,
		pos=v_clone(p),
		update=update_part,
		draw=draw_circ_part
	})
end
function make_trail(p,c)
	return add(parts,{
		t=time_t+8,
		c=c or 7,
		r=0.2,
		dr=-0.02,
		pos=v_clone(p),
		update=update_part,
		draw=draw_circ_part
	})
end

function make_blast(p)
	return add(parts,{
		t=time_t+8,
		r=1,
		dr=0.05,
		pos=v_clone(p),
		update=update_part,
		draw=draw_blast_part
	})
end

function update_part(self)
	if(self.t<time_t) return false
	if(self.r<0) return false
	self.r+=self.dr
	return true
end

function update_blt(self)
	if(self.t<time_t) return false
	
	-- ground?
	if game_mode==1 then
		if self.pos[2]<0 then
			self.pos[2]=0
			make_flash(self.pos)
			return false
		end
	end
	-- collision?
	for _,a in pairs(actors) do
		if a.model.r and band(a.side,self.side)==0 and sqr_dist(self.pos,a.pos)<a.model.r*a.model.r then
			a:hit(self.dmg,self.actor)
			make_flash(self.pos)
			return false
		end
	end
	v_plus_v(self.pos,self.u,self.acc)
	return true
end

 function update_proton(self)
 if time_t%2==0 then
 	make_trail(self.pos,10)
 end
 -- update orientation to match target
 if self.target and not self.target.disabled then
		-- old enough?
		local v=v_clone(self.target.pos)
		v_plus_v(v,self.pos,-1)
		-- not too close?
		if v_dot(v,v)>0.25 then
			v_normz(v)
 		-- within cone?
 		if v_dot(self.u,v)>0.6 then
 			v_plus_v(self.u,v,smoothstep(self.duration/60))
 			v_normz(self.u)	
 		end
 	end
 end
 self.duration+=1
 return update_blt(self)
end
		
function draw_line_part(self,x0,y0,z0,w0)
	local x1,y1,z1,w1=cam:project(self.pos[1]+self.u[1],self.pos[2]+self.u[2],self.pos[3]+self.u[3])
	if z1>0 then
		line(x0,y0,x1,y1,time_t%2==0 and 7 or self.c)
	end
end

function draw_circ_part(self,x,y,z,w)
	circfill(x,y,self.r*w,self.c)
end

function draw_blast_part(self,x,y,z,w)
	circfill(x,y,self.r*w,7)
end
function draw_proton_part(self,x,y,z,w)
	-- light effect
	fillp(dither_pat[mid(#dither_pat-flr(w/2),1,#dither_pat)])
	circfill(x,y,(0.5+rnd(1))*w,8)
	fillp()
	circfill(x,y,(0.1+0.2*rnd())*w,10)
end

local turn_t=0
local mousex,mousey=0,0
local dist=0
local sel_actor,sel_t=1,0
local cam_rear=false
function control_plyr(self)
	local pitch,roll=0,0
	
	if(btn(0)) roll=-1 turn_t+=1
	if(btn(1)) roll=1 turn_t+=1
	if(btn(2)) pitch=-1
	if(btn(3)) pitch=1

	turn_t=min(turn_t,8)
	if roll!=0 then
		local r=turn_t/8
		local q=make_q({0,1,0},(1-r)*roll/128)
		q_x_q(plyr.q,q)
		q=make_q({0,0,1},-r*roll/128)
		q_x_q(plyr.q,q)
	else
		turn_t=0
	end
	
	if pitch!=0 then
	 self.pitch-=pitch/256
	 self.pitch=mid(self.pitch,-1/196,1/196)
	end
	self.pitch*=0.9
	local q=make_q({1,0,0},self.pitch)
	q_x_q(plyr.q,q)
	
	-- update pos
	local m=m_from_q(plyr.q)
	local fwd={m[9],m[10],m[11]}
	v_plus_v(plyr.pos,fwd,plyr.acc+plyr.boost)
	-- special cases
	if game_mode==1 then
		plyr.pos[2]=mid(plyr.pos[2],1,4)
	end
	m[13]=plyr.pos[1]
	m[14]=plyr.pos[2]
	m[15]=plyr.pos[3]
	plyr.m=m

	-- boost 
	if btn(4) then
		self.boost=min(self.boost+0.01,0.1)
	else
		self.boost*=0.98
	end
	
	-- cam modes
	if btnp(0,1) then
		cam_mode+=1
		cam_mode%=3
	end
	-- behind look?
	cam_rear=false
	if btn(2,1) then
		cam_rear=true
	end
	
	if cam_mode==0 then
		cam.pos=m_x_xyz(plyr.m,0,2,-8)
		--v_plus_v(cam.pos,plyr.pos,-1)
		cam.q=q_clone(plyr.q)
	elseif cam_mode==1 then
		cam.pos=v_clone(plyr.pos)
		if cam_rear==true then
			local q=q_clone(plyr.q)
			q_x_q(q,make_q(v_up,0.5))
			cam.q=q
		else
			cam.q=q_clone(plyr.q)
		end
	else
		local x,y=stat(32),stat(33)
		local dx,dy=mousex-x,mousey-y
		local q=make_q({0,1,0},dx/128)
		q_x_q(cam.q,q)
		--local q=make_q({0,0,1},dy/128)
		--q_x_q(cam.q,q)
		local m=m_from_q(cam.q)
		dist+=dy/2
		dist=min(dist,-2)
		cam.pos=m_x_xyz(m,0,2,dist)
		if stat(34)==1 and sel_t<time_t then
			sel_actor+=1
			sel_t=time_t+8
		end
		local a=actors[sel_actor%#actors+1]
		v_plus_v(cam.pos,a.pos)
		mousex,mousey=x,y
	end
	
	if btnp(5) then
		plyr:fire()
	end
	
	-- find nearest enemy (360)
	local min_dist,target=32000
	-- is lock stable?
	for _,a in pairs(actors) do
		if band(a.side,plyr.side)==0 then
			local d=sqr_dist(a.pos,plyr.pos)
			if d<min_dist then
				min_dist=d
				target=a
			end
		end
	end
	plyr.target=target
	if target and pitch==0 and roll==0 and in_cone(plyr.pos,target.pos,fed,0.8,48) then
		plyr.lock_t+=1
	else
		plyr.lock_t=0
	end
	if plyr.lock_t>30 and btnp(4) then
		plyr:fire_proton(target)
	end
	
end

local ds_m=make_m()
function draw_deathstar()
	ds_m[13],ds_m[14],ds_m[15]=cam.pos[1],cam.pos[2],6+cam.pos[3]	
	draw_model(all_models.deathstar,ds_m)
end

local stars={}
local stars_ramp={1,5,6,7}
function draw_stars()
 for i=1,#stars do
		local v=stars[i]
		local x,y,z,w=cam:project(v[1],v[2],v[3])
		if z>0 and z<32 then
			w=flr(4*w/12)
			pset(x,y,stars_ramp[min(w+1,#stars_ramp)])
		else
			-- reset pos
			local star=make_rnd_v(32)
			v[1],v[2],v[3]=star[1],star[2],star[3]
			v_plus_v(v,cam.pos)
		end
	end
end

function draw_ag_radar(x,y,r,rng)
	local objs=game_mode==1 and ground_actors or actors
	-- get angle dir
	local angle=atan2(plyr.m[9],plyr.m[11])-0.75
	local c,s=cos(angle),-sin(angle)
	-- draw grid
	local scale=6
	local dx,dy=(plyr.pos[1]/2)%scale,(plyr.pos[3]/2)%scale
	color(3)
	local x0=-dx-scale
	while x0<22+scale do
		local y0=-dy-scale
		while y0<22+scale do
			local xx,yy=x0-11,y0-11
			xx,yy=xx*c-yy*s,xx*s+yy*c
			pset(x+xx,y-yy)
			y0+=scale
		end
		x0+=scale
	end
	-- radar dots
	for _,a in pairs(objs) do
		if a!=plyr then
			local p=v_clone(a.pos)
			v_plus_v(p,plyr.pos,-1)
			v_scale(p,1/2)
			if v_dot(p,p)<64 then
				local px,py=c*p[1]-s*p[3],s*p[1]+c*p[3]
				pset(x+px,y-py,8)
			end
		end
	end
end

local all_locks=json_parse('{"1":{"spr":206,"x":53,"y":107,"flipx":false,"flipy":false},"2":{"spr":206,"x":67,"y":107,"flipx":true,"flipy":false},"4":{"spr":207,"x":60,"y":105,"flipx":false,"flipy":false},"8":{"spr":207,"x":60,"y":109,"flipx":false,"flipy":true}}')
function draw_locks(f)
	for i=1,4 do
		local s=all_locks[shr(f,i)]
		if s then
			spr(s.spr,s.x,s.y,1,1,s.flipx,s.flipy)
		end
	end
end

function draw_aa_radar()
	  rectfill(
	  	64-11,115-11,
	  	64+11,115+11,0)
	  -- draw locks
 		if plyr.target then
 			local v=v_clone(plyr.target.pos)
 			-- todo: project in plyr space
 			cam:project_v(v,64,115,8)
			 if plyr.lock_t>30 then
			  if time_t%4>1 then
					draw_locks(1+2+4+8)
		 		end
				end
			 if v[3]>0 then
			 	local l=0
			 	if v[2]>115 then
	 		  	l+=4
			 	elseif v[2]<115 then
					l+=8
			 	end
			 	if v[1]>64 then
	 		  	l+=1
			 	elseif v[1]<64 then
					l+=2
			 	end
			 	local x,y=mid(v[1],50,70),mid(v[2],100,127)
			 	rectfill(x,y,x+1,y+1,8)
			 	draw_locks(l)
			 end
 		end
end

function draw_text(s,x,y)
	print(s,x,y,7)
end

-- wait loop
function start_screen:update()
	if btnp(0) then
		game_mode-=1
	end
	if btnp(1) then
		game_mode+=1
	end
	game_mode=mid(game_mode,0,2)
	
	if not self.starting and (btnp(4) or btnp(5)) then
		sfx(0)
		-- avoid start reentrancy
		self.starting=true
		-- init game
		futures_add(function()
			wait_async(30)
			game_screen:init()
			cur_screen=game_screen
			start_screen.starting=false
		end)
	end
end
function start_screen:draw()
	cam.pos[3]+=0.1
	cam:update()
	draw_stars()
	local m=m_from_q(make_q({1,0,0},0.75))
	m[13]=-0.85
	m[14]=0.4
	m[15]=2.1+cam.pos[3]
	draw_model(all_models.title,m)
	print("attack on the death star",20,78,12)
	
	-- draw hiscores every 10s
	if time_t%600>300 then	
		local y=32
		draw_text("highscores",32,y,6)
		y+=12
		for i=1,#scores do
			if scores[i].islast==false or time_t%4<2 then
				draw_text(padding(scores[i].key,4),32,y,6)	
			end		
			y+=10
		end
	end
	
	if game_mode==0 then
		print("space",48,96,12)
	elseif game_mode==1 then
		print("ground",48,96,12)
	else
		print("trench",48,96,12)
	end
	if (starting and time_t%2==0) or time_t%24<12 then	
		print("press start",44,118,11)
	end
end

function gameover_screen:update()
end

function gameover_screen:draw()
	draw_text("game over",38,60,6)

	if #scores>0 and scores[1].islast then
		if time_t%4<2 then
			draw_text("new highscore!",24,72,6)
		end
	end
end

-- bench screen
--[[
function bench_screen:init()
	time_t=0
	parts={}
	actors={}
	for i=0,1 do
		for j=0,1 do
			local a=make_junk(2*i,2*j,all_models.junk1)
			a.update=function() return true end
			add(actors,a)
		end
	end
end
function bench_screen:update()
	zbuf_clear()
	
	local x,y=0,0
	
	if(btn(0)) x=-1
	if(btn(1)) x=1
	if(btn(2)) y=-1
	if(btn(3)) y=1
	
	self.angle+=0.01*x
	cam.q=make_q({0,1,0},self.angle)
	--local q=make_q({0,0,1},dy/128)
	--q_x_q(cam.q,q)
	local m=m_from_q(cam.q)
	self.dist=min(self.dist-y/2,-2)
	cam.pos=m_x_xyz(m,0,2,self.dist)
	if btnp(4) then
		self.sel_actor+=1
	end
	local a=actors[(self.sel_actor%#actors)+1]
	v_plus_v(cam.pos,a.pos)

	cam:update()
	zbuf_filter(actors)
end
function bench_screen:draw()
	zbuf_draw()
	print("actors:"..#actors,2,9,7)
end
]]

-- play loop
function game_screen:init()
	time_t=0
	parts={}
	actors={}
	npc_count=0
	plyr=make_plyr(0,0,0)
	
	if game_mode==1 then
		init_ground()
	elseif game_mode==2 then
		init_trench(8)
	end
end

function game_screen:update()
	zbuf_clear()
	
	if plyr then
		control_plyr(plyr)
	end
	cam:update()

	if game_mode==0 then
		if npc_count<=0 then
			local p,v=make_rnd_pos_v(plyr,30)
			local target
			-- friendly npc?
			if rnd()>0.5 then
				target=make_npc(p,v,npc_xwing)
				v_plus_v(p,v,10)
			end
			-- spawn new enemy
			for i=1,flr(1+rnd(2)) do
				local a=make_npc(p,v,npc_tie)
				a.target=target
				target=a
				v_plus_v(p,v,10)
			end
		end
	elseif game_mode==1 then
		update_ground()
	elseif game_mode==2 then
		update_trench()
	end

	zbuf_filter(actors)
	zbuf_filter(parts)
end
function game_screen:draw()
	if game_mode==0 then
		draw_deathstar()
		draw_stars()
	else
		draw_ground()
	end
	
	zbuf_draw()
		
	-- cockpit
	if cam_mode==1 then
	 if cam_rear==false then
	  
 		-- radar
 		if game_mode==0 then
 			draw_aa_radar()
 		else
 			draw_ag_radar(64,115,22,16)
		end
 		palt(0,false)
 		palt(14,true)
 		spr(64,0,32,8,4)
 		spr(64,64,32,8,4,true)
 		spr(8,0,64,8,4)
 		spr(8,64,64,8,4,true)
 		spr(72,0,96,8,4)
 		spr(72,64,96,8,4,true)
 		
 		-- hp
 		local x=23
 		for i=1,plyr.hp do
 			rectfill(x,120,x+1,123,11)
 			x+=3
 		end
 		for i=plyr.hp+1,8 do
 			rectfill(x,120,x+1,123,1)
 			x+=3
 		end
 		-- engines
 		local p=(plyr.acc+plyr.boost)/(0.2)
 		rectfill(82,120,82+22*p,123,9)
 	else
 		spr(0,0,32,8,4)
 		spr(0,64,32,8,4,true)
 		rectfill(0,64,127,127,0)
 		rect(19,64,108,125,1)
 	end
 	pal()
 	palt()
 end

end

function _update60()
	time_t+=1
	time_dt+=1
	futures_update(before_update)
	
	cur_screen:update()
	
	screen_update()
end

function _draw()
	cls(0)

	cur_screen:draw()
	
	futures_update(after_draw)

	time_dt=0

	--[[
	local y=2	
	rectfill(32,y,128-32,y+18,0)
	rect(32,y,128-32,y+18,1)
	spr(192,33,y+1,2,2)
	print("red leader:",50,y+3,9)
	print("help!",50,y+10,7)
	if time_t%4==0 then
		fillp(0b1111000011110000.1)
		rectfill(33,y,128-32,y+23,0)
		fillp()			
 end
	
	if time_t%32>24 then
		rectfill(40,y+13,41,y+14,0)
	elseif time_t%32>16 then
		line(40,y+13,41,y+13,0)
	end
	]]
	rectfill(0,0,127,8,1)
	print(stat(1),2,2,7)
end


function _init()
	-- mouse support
	poke(0x5f2d,1)

	if cartdata("freds72_xvst") then
		n=dget(0)
		for i=1,n do
			add(scores,{key=dget(i),islast=false})
		end
		-- in case...
		sort(scores)
	end
	
	-- compute xwing laser aim
	local wp=all_models.xwing.wp
	for i=1,#wp.pos do
		local v=v_clone(wp.pos[i])
		v={-v[1],-v[2],64-v[3]}
		v_normz(v)
		add(wp.n,v)
	end
	
	-- stars
	for i=1,48 do
		add(stars,make_rnd_v(48))
	end
		
	cam=make_cam(64)

	cur_screen=start_screen
end

__gfx__
111eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee
00011111eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee
0000000011111eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11eeeeeee
100000000000011111eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
e111111100000000001111111111111111111111111111111111111111111111e11000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeee11111110000000000000000000000000000000000000000000000000eee1000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeee11eeeeeee
eeeeeeeeeeeeeee1100001111111111111111111111111111111111111111111eeee1100000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee
eeeeeeeeeeeeeeeee1001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee
eeeeeeeeeeeeeeeee1001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeee1e
eeeeeeeeeeeeeeee10001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeee1eeeee1e
eeeeeeeeeeeeeeee1001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeee111111e
eeeeeeeeeeeeeee10001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeeee1001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeee10001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeee1001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeeee1001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeee10001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeeee1001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000000001eeeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeee10001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000000001eeeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeeeee1001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000000001eeeeeeeeeeeeeeeeeeeeeeeee
eeeeeeeeee110001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000000001eeeeeeeeeeeeeeeeeeeeeeee
eeeeeee11100001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000001eeeeeeeeeeeeeeeeeeeeeee
eeee111000000011eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000001eeeeeeeeeeeeeeeeeeeeee
e1110000000000101eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000001eeeeeeeeeeeeeeeeeeeee
100000000000110001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000001eeeeeeeeeeeeeeeeeeee
0000000001110000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000011111111111111111111
00000001100000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000000000000000000000000
000011100000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000000000000000000000000000
0111000000000000000001111111111111111111111111111111111111111111eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000000000000000000000000000
1000000000000000000001000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000111111111111111111111111
0000000000000000000010000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000001000000000000000000000000
0000000000000000000100111111111111111111111111111111111111111111eeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000010011111111111111111111111
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000100100000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000001001000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000010010000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000100100000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000001001000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000010010000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000000100100000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000001001000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000000010010000000000000000000001111111111
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000000010010000000000000000000001eeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000000000100100000000000000000000001eeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000000001001000000000000000000000001eeeeeeeeee
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000000000010010000000000000000000000001eeeeeeeeee
1eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000000000000100100000000000000000000000001eeeeeeeeee
01eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000000000001001000000000000000000000000001eeeeeeeeee
001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000000000000010010000000000000000000000000001eeeeeeeeee
0001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000000000000100100000000000000000000000000001eeeeeeeeee
00001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000000000000001001000000000000000000000000000001eeeeeeeeee
000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000000000000010010000000000000000000000000000001eeeeeeeeee
0000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000000000000000100100000000000000000000000000000001eeeeeeeeee
00000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000001001000000000000000000000000000000001eeeeeeeeee
000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000010010000000000000000000000000000000001eeeeeeeeee
0000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000100100011111111111111111111111110000001eeeeeeeeee
00000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000001001000100000000000000000000000001000001eeeeeeeeee
000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000010010000100000000000000000000000001000001eeeeeeeeee
0000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000100100000100000000000000000000000001000001eeeeeeeeee
00000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000001001000000100000000000000000000000001000001eeeeeeeeee
000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000010010000000100000000000000000000000001000001eeeeeeeeee
0000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000100100000000100000000000000000000000001000001eeeeeeeeee
00000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee111111e000000001001000000000011111111111111111111111110000001eeeeeeeeee
000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeee1e000000010010000000000000000000000000000000000000000001eeeeeeeeee
0000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee0000001001000000000000000000000000000000000000000000001111111111
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
00000006600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00001156666600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00011157766660000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00111557777766000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00111557777776000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001d999dd999d7000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00199999999797000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00199999999996000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000d999ff999d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000dffffffffd0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00005ffffff500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000005ffff5000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000057750000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
30000003300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
03000030030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00300300003000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00033000000300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000003000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000030000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
08090f100c1f131e1f0c1d1b80a080749e80699780628c806080806274806969807462808060808c62809769809e7480a080809e8c809797808c9e807398806d96806b90806d8a80738880798a807b90807996807683808a8380a0808000001b0201010302010403010504010605010706010807010908010a09010b0a010c0b
010d0c010e0d010f0e01100f01011001121101131201141301151401161501171601181701111801051901191a011a1b01051520191603188080a0698097608080698069808060978069a0808097809780a0a069a09760a08069a06980a06097a069a0a08097a09780b09372b08e6db08072b07280b06d8eb07293b0808eb08e
110804120f11070604140d13050404160b15030204180917010104171012080704110e14060504130c16040304150a18020c04221b210b0a04241923090904232025100f04271e260e0d04281c220c0b04211a240a1004251f270f0e04261d280d12081a1b1c1d1e1f2019119e808c8c806262807474809e8c809e9e80747480
6262808c6893767693988a939898937676936868938a98938a8a936880a080280201000302000403000504000605000706000807000108000a09000b0a000c0b000d0c000e0d000f0e00100f00091000070f00100800050d000e0600030b000c04000109000a0200121100131200141300151400161500171600181700111800
0b1300140c00091100120a001018000e1600170f00150d0005152019160408a060a0a060606060606060a096809696806a6a806a6a80960601040104060205040a0c0b090104030905010204050b07040304070c080605040302080a0680608080a0809f898080896161898080899f0c01020001040001050002030002060003
0400030700040800050600050800060700070800031f1410416672809a72809c80906480909a8e80668e806480709c80704d76804d808a4d8a804d8076b37680b3808ab38a80b38076b380c0b3c8a0b3c860b38040b33860b338a04d80c04dc8a04dc8604d80404d38604d38a08080608b8b649494709797809494908b8b9c90
8064a0808090809c8b7564946c70976980946c908b759c80706480647080608080649080709c7575646c6c706969806c6c9075759c70806460808070809c758b646c94706997806c9490758b9c809064809c7080a080809c9080909c310604060d0b0e0704070e0c0f0104030f0a1004040110090d0504081514160804041712
150204021811170304051613183f045d6727663d0365256840045e66286a3e045c68266741045f6a29692104282e5a4b1f04262f352b2204294b512c2004272b2d2e1e03256b2f03045a36324a08043537302a2504514a333424042d2a313623036b6c372904323d3a3c2704303f383e2a04333c3b402804313e393d26036c6d
3f2c04384641452f043b4844472d04394542492b036d6e462e043a49434834044473724e3204424d4f5030036e6f5233044350717331044152584d37047274565736044f4c545935036f705b25083440474e5760692c0404715955740704585b534c3c0456615f603a0454635d6238037065643b0455625e61390453645c6331
7b97967b976a7b696a7b699685976a85696a8569968597968c9d788388618c9d8889966a8996969d8c8896896a9689969d8c788883619d748896776a9677969d7478887d618c6388896a6a896a968c6378837861776a6a776a967463787d78617463886a7796637478787d616374886a776a6a8996638c787883618080a0638c
886a896a779696749d787d8861749d8877966a740401000302000107000802000503000604000607000805000a0900090c000b0a000b0c00040a000b06000c07000901000e0d00100d000f0e00100f000810000f0500020d00030e001211011312011413011514011615011116011817011918011a19011b1a011c1b01171c01
1d1e001e1f001f2000202100212200270800081f002225000824002024001e2300262700272800282900292a00252a002308002428002326002b2c002c2d002d2e002e2f002e2900282d002c2700262b002f2a00303100313200323300333400312c002b3000342f002e3300322d002903000321000739003107003734000736
00363200032500353000383900393a003a3b003b3c003c37003507003a36002403003835003d3e003e3f003f4000404100413c003b40003f3a00393e003d38001d3d00203f003e1f001e3d004122002140001d23001d26001d2b001d30001d35001d3800360400043700330400043b0005232214191244737557738f5773758d
738f8d8d75578d8f578d758d8d8f8d7a7ae57a87e5867ae58687e57d7dfd7d82fd837dfd8382fd638e5c638e80399d69399d80399db369825c638e5c698286638e8674885c6f935c7488866f93869b8d5c9b8d80c69969c69980c699b390935c9b8d5c9093869b8d868a885c95825c8a888695828663745c6374803965693965
803965b36f6f5c63745c6f6f86637486747a5c69805c747a866980869b755c9b7580c66969c66980c669b395805c9b755c9580869b75868a7a5c906e5c8a7a86906e8626010402030401080407111012070407090a0805040a0c020b030405080b01080406030c090c041019181a070405130e11040406120f14030404140d13
0d04151718160b040e1b16190a040f1a171c09040d1c151b1604232425221804252728261c04282a2b291a042b2d232c180426292c221d0427242d2a2304343536332504363839372904393b3c3a27043c3e343d2504373a3d332a0438353e3b300445464744320447494a4836044a4c4d4b34044d4f454e3204484b4e443704
49464f4c3d04565758553f04585a5b5943045b5d5e5c41045e60565f3f04595c5f5544045a57605d26608080a08082a0808080806080608080a080a0808480608280a0836080828080a0806084809f866080846372808080a09d8e808080608e6380729d80909c808080a07064808080606490809c70807263808080a08e9d80
8080609d7280638e809c90808080a0647080808060709c80906480600301000102000204000403000703000408000807000507000806000605000105000602000a09000b09000a0c000c0b00070b000c08000309000a04000e0d000f0d000e1000100f000b0f00100c00090d000e0a0011120113140112140113110114150118
16001617001719001918001c1800191d001d1c001a1c001d1b001b1a00161a001b17001e1f012021011f2101201e01212201252300232400242600262500292500262a002a29002729002a28002827002327002824002b2c012d2e012c2e012d2b012e2f01323000303100313300333200363200333700373600343600373500
3534003034003531003839013a3b01393b013a38013b3c013f3d003d3e003e4000403f00433f004044004443004143004442004241003d4100423e00051f141f1710bdbc807ab7807ab7807ab7807ab6807ab68079b68079b78079b98077ba8075ba8074ba8074ba8073ba8072ba8071b98071b98071b88070b78070ab8070b3
8074b48074b48074b48075b48075b48075b48075b18078b1807ab0807bb0807cb1807cb1807db1807db2807eb2807eb3807ebc807ea18070a1807eaa807eac807ead807dae807dae807caf807baf807aaf8079af8079ae8078ae8077ad8077ad8076ab8076ac8074b48074b48070ab8070a68075a68070a6807ba68078aa8078
aa8078aa8078ab8078ab8079ab8079ab807aaa807aaa807aaa807aaa807baa807b99807b9780759b80759d807ea180709c80709b80739780739680708a807688807084807081807a7e807a7e807e84807e86807888807e8c807e8e80788f807e8c8070a58080a5808eae808eb0808eb1808db2808db2808cb3808bb38089b380
89b38088b28088b28087b18087b18086af8086b08084b88084b88080af8080aa8085aa8080aa808baa8088ae8088ae8088ae8088ae8088af8089af8089af8089af808aae808aae808aae808aae808bae808b9d808b9b80859f808595808ea0808ea58080a080809f80839b80839a808095808098808a97808e93808a9380808e
80808e808a8a808e88808a87808a87808a87808a8780898780898780898980878a80858a80848b80848b80838a80828a80818a80818980818880808880807b80807b808484808484808484808485808585808585808584808582808881808a81808b81808c81808c81808d82808d82808e83808e84808e8d808e0000b9191a01
7071014c4d01909101a9aa01373801161701757601949501adae017d7e013a3b015051018788012324010f1001959601393a01273c015d5e014f50015e5f01898a01111201b9ba019698015c5d01aaab012e2f014e4f01617601bcbd019394016364013b3c01323301515201717201676801babb015a5b01171801484901090a
019c9d017273012021015360018384011d1e01010201444501abac019d9e017374010e0f01333401a2a301222301080901525301acad01a1a2011b1c01aeaf010607015b5c01696a016d6e018d8e011213016f7001464701a4a501646501012601565701a3a401b8b901797a011f20010506016869012829010b0c016c6d0198
99018e8f01bbbc01040501555601a8a901999a012425019297017b7c010c0d018f9001778501434401a0a101afb0017778017f8001070801292a013e3f011516015455018c8d013536017a7b01a5a6012c2d016162019fa0012122017475017e7f013031011c1d01b5b6013f40010203015758014041012d2e019b9c019e9f01
2b2c014b4c01a7a8013132016263013d4a01101101b1b2016566018687011a1b01b6b701b4b5014e5f018182014142018a8b013637011314016a6b014b4d012a2b014546010d0e016b6c01848501424301b0b1012526016667018688017879016e6f01494a013d3e01595a010a0b015460018b8c01181901474801a6a7019193
01b2b3012728017c7d01343501b7b801808101030401b3b4015859018283011e1f012f30019a9b01061f201d1d101f10869a8d869a967a9a8d7a9a969080909080707080707080908da08d8da07373a07373a08d72958e7295728e958e8e95720909040c0e0d0b050411151203060412161006070410130f08090405140a0c0d
0413090e0a0d041411040f100415050b070e0416070d090980a080a083808083606083808083a06083808083a0a08380808360160201010403010506000508000f0900060700100a000708000e0b000d0c00090a00090c000a0b000b0c00080d00070e00050f000610000d0e000f0d00100f000e1000071f1d10190e130308e0
00e0e000202000202000e0e0c0e0e0c02020c02020c0e003010402060401010401050903030406080a070380a080608080a080800a010200010400010500020300020600030400030700040800050600070800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
