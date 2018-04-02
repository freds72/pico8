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
local game_mode=0
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
	deathstar={
		c=3
	},
	xwing={
		c=7,
		wp={
			dly=8,
   pos={{2,1,1.6},{2,-1,1.6},{-2,-1,1.6},{-2,1,1.6}},
   n={}
  }
	},
	sphere={
		c=5,
		wp={
			dly=12,
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

function v_plus_v(v,dv,scale)
	scale=scale or 1
	v[1]+=scale*dv[1]
	v[2]+=scale*dv[2]
	v[3]+=scale*dv[3]
end
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

		te[1]=1-( yy+zz )
		te[5]=xy-wz
		te[9]=xz+wy

		te[2]=xy+wz
		te[6]=1-( xx+zz )
		te[10]=yz-wx

		te[3]=xz-wy
		te[7]=yz+wx
		te[11]=1-( xx+yy )

		// last column
		te[4],te[8],te[12]=0,0,0

		// bottom row
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

local ground_colors={5,1,5,1}
local ground_scale=4
function draw_ground(self)
	local v={}
	local x,z=plyr.pos[1],plyr.pos[3]
	local dx,dy=x%ground_scale,z%ground_scale
	
	local c=1
	for j=-16,16,ground_scale do
		for i=-16,16,ground_scale do
			local ii,jj=i-dx+x,j-dy+z
			local x,y,z=cam:project(ii,0,jj)
			if z>0 then
				pset(x,y,ground_colors[flr(ii+jj)%2+1])
			end
			c+=1
		end
	end
end
local turrets={}
function make_turret(i,j)
	local x,y,z=i*ground_scale,0,j*ground_scale
	local t={
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
		update=function() return true end,
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
	for i=i0-5,i0+5 do
		local cx=(i%128+128)%128
		for j=j0-5,j0+5 do
			local cy=(j%128+128)%128
			local t=turrets[cx+cy*128]
			if t then
				t:update()
				add(drawables,t)
				add(ground_actors,t)
			end
		end
	end
end

function update_turret(self)
	local dx,dy=self.pos[1]-plyr.pos[1],self.pos[3]-plyr.pos[3]
	-- in range?
	if dx*dx+dy*dy<64 then
 	local angle=atan2(dx,dy)-0.25
 	local q=make_q(v_up,angle)
 	local m=m_from_q(q)
 	m[13],m[14],m[15]=self.pos[1],0,self.pos[3]
 	self.m=m
 	
 	if abs(angle)<0.2 and self.fire_t<time_t then
 		self:fire()
 		self.fire_t=time_t+self.model.wp.dly
 	end
	end
	
	return true
end

local debug_vectors=false
function draw_actor(self)
	draw_model(self.model,self.m)
	-- debug
	--[[
	if debug_vectors then
 	if self.target then 
 		local c=12
 		if band(self.side,self.target.side)==0 then
	 		c=8
 		end
 		local pos=v_clone(self.target.pos)
 		v_plus_v(pos,self.pos,-1)
	 	draw_vector(self.m,self.pos,pos,c)
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
 	printh("-------------------------")
 	printh("unpack:"..name)
 	local model={}
 	-- vertices
 	n=unpack_int()
 	printh("verts:"..n)
 	model.v={}
 	for i=1,n do
 		local v={unpack_float(),unpack_float(),unpack_float()}
 		add(model.v,v)
 	end	
 	-- faces
 	n=unpack_int()
 	printh("faces:"..n)
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
 	printh("normz:"..n)	
 	model.n={}
 	for i=1,n do
 		local v={unpack_float(),unpack_float(),unpack_float()}
 		printh("n["..i.."]:"..v[1].." "..v[2].." "..v[3])
 		add(model.n,v)
 	end
 	
 	-- edges
 	n=unpack_int()
 	printh("edges:"..n)	
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

function m_inv_x_v(m,v)
	local x,y,z=v[1]-m[13],v[2]-m[14],v[3]-m[15]
	v[1]=m[1]*x+m[2]*y+m[3]*z
	v[2]=m[5]*x+m[6]*y+m[7]*z
	v[3]=m[9]*x+m[10]*y+m[11]*z
end

function draw_model(model,m)
	draw_session_id+=1

	color(model.c or 1)
	-- bounding radius
	if model.show then
		--circ(xe,ye,model.r*w)
	end
	
	-- cam pos in object space
	local cam_pos=v_clone(cam.pos)
	m_inv_x_v(m,cam_pos)

 -- projected points
	local p={}	
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
	local x,y,z,w
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
	for _,a in pairs(actors) do
		if a!=self then
			local p=v_clone(a.pos)
			v_plus_v(p,pos,-1)
			local d=v_dot(p,p)
			d=smoothstep(d/(dist*dist))
			v_plus_v(v,p,d-1)
		end
	end
	return v
end
function seek(self)
	local fwd={self.m[9],self.m[10],self.m[11]}
	for _,a in pairs(actors) do
		if band(a.side,self.side)==0 then
			local p=v_clone(a.pos)
			v_plus_v(p,self.pos,-1)
			-- within range?
			if v_dot(p,p)<16*16 then
				v_normz(p)
				-- within cone?
				if v_dot(fwd,p)>0.5 then
				 -- avoid loops
				 if a.target!=self then
						return a
					end
				end
			end
		end
	end
end

-- return a pos in self space
function wander(self)
	local p=make_rnd_v(2)
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
	local pos={0,0,4}
	local m=self.m
	m_x_v(m,pos)
	-- forces
	local force={0,0,0}
	local can_fire=false

	if self.target then
		-- friendly: formation flight
		local target_pos={0,-4,-10}
		-- enemy: get in sight
		if band(self.target.side,self.side)==0 then
			target_pos={0,0,-10}
			can_fire=true
		end
		v_plus_v(force,follow(pos,self.target,target_pos))
	else
		-- seek target
		self.target=seek(self)
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
	v_plus_v(force,avf)
	
	local d=v_dot(force,force)
	-- debug
	self.dist=sqrt(d)
	self.avoid=avf
	-- too close/no force?
	if d>0.25 then
		-- ease in
		acc=min(d/0.25,1.2)*self.acc
		v_clamp(force,0.12)
		v_plus_v(pos,force)
		v_plus_v(pos,self.pos,-1)
		v_normz(pos)
		
		-- update orientation
		local q=make_q_from_v({m[9],m[10],m[11]},pos)
		q_x_q(self.q,q)
		m=m_from_q(self.q)
	end
	-- move actor
	local fwd={m[9],m[10],m[11]}
	v_plus_v(self.pos,fwd,acc)

	m[13],m[14],m[15]=self.pos[1],self.pos[2],self.pos[3]
	self.m=m

	-- fire solution?
	if can_fire and self.fire_t<time_t then
		local p=v_clone(self.target.pos)
		v_plus_v(p,self.pos,-1)
		-- in range?
		if v_dot(p)<96 then
 		v_normz(p)
 		if v_dot(fwd,p)>0.95 then
 		-- must be in sight for some time
 			if self.lock_t>45 then
 				self.lock_t=45
 				self.fire_t=time_t+self.model.wp.dly
 				self:fire()
 			end
 			self.lock_t+=2
 		end
		end
	end
	-- target memory
 self.lock_t=max(self.lock_t-4)

	return true
end

function make_plyr(x,y,z)
	local p={
		score=0,
		hp=6,
		boost=0,
		acc=0.1,
		model=all_models.xwing,
		pos={x,y,z},
		q=make_q({0,0,1},0),
		laser_i=0,
		fire_t=0,
		side=good_side,
		hit=function(self,dmg)
			screen_shake(rnd(),rnd(),2)
		end,
		fire=make_laser,
		die=die_plyr,
		draw=function(self)
			if cam_mode==1 then
				return
			end
			draw_actor(self)
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
	acc=0.1,
	model=all_models.xwing,
	side=good_side,
	update=update_flying_npc
}
local npc_tie={
	hp=4,
	acc=0.1,
	model=all_models.sphere,
	side=bad_side,
	update=update_flying_npc
}
local npc_turret={
	hp=2,
	model=all_models.turret,
	side=bad_side,
	update=update_ground_npc
}
local npc_junk={
	hp=1,
	rnd={model={
		all_models.junk1,
		all_models.junk2,
		all_models.junk3}},
	side=any_side
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
		hit=function(self,dmg)
			--self.hp-=dmg
			if self.hp<=0 then
				self:die()
			end
		end,
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
		end
	}
end

function make_laser(self)
	local wp=self.model.wp
	local i=self.laser_i%#wp.pos+1
	local p=v_clone(wp.pos[i])
	m_x_v(self.m,p)
	local v=v_clone(wp.n[i])
	o_x_v(self.m,v)
	self.laser_i+=1
	-- laser colors
	local c=self.side==good_side and 11 or 8
	add(parts,{
		t=time_t+90,
		acc=0.5,
		pos=p,
		u=v,
		c=c,
		side=self.side,
		dmg=1,
		update=update_blt,
		draw=draw_line_part})
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
			a:hit(self.dmg)
			make_flash(self.pos)
			return false
		end
	end
	v_plus_v(self.pos,self.u,self.acc)
	return true
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

local turn_t=0
local mousex,mousey=0,0
local dist=0
local sel_actor,sel_t=1,0
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
		local q=make_q({1,0,0},-pitch/128)
		q_x_q(plyr.q,q)
	end
	-- update pos
	local m=m_from_q(plyr.q)
	v_plus_v(plyr.pos,{m[9],m[10],m[11]},plyr.acc+plyr.boost)
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
	if btnp(6) then
		cam_mode+=1
		cam_mode%=3
	end
	
	if cam_mode==0 then
		local m=m_from_q(plyr.q)
		cam.pos=m_x_xyz(m,0,2,-8)
		v_plus_v(cam.pos,plyr.pos)
		cam.q=q_clone(plyr.q)
	elseif cam_mode==1 then
		cam.pos=v_clone(plyr.pos)
		cam.q=q_clone(plyr.q)
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

function draw_radar(x,y,r,rng)
	circ(x,y,r,3)
	pset(x,y,3)
	local objs=game_mode==1 and ground_actors or actors
	for _,a in pairs(objs) do
		if a!=plyr then
			local p=v_clone(a.pos)
			m_inv_x_v(plyr.m,p)
			v_clamp(p,rng)
			pset(x+r*p[1]/rng,y-r*p[3]/rng,p[2]>0 and 8 or 2)
		end
	end
end

function draw_text(s,x,y)
	print(s,x,y,7)
end

-- wait loop
function start_screen:update()
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
	--draw_model(all_models.title,m)
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
function bench_screen:init()
	time_t=0
	parts={}
	actors={}
	for i=0,1 do
		for j=0,1 do
			add(actors,make_junk(2*i,2*j,all_models.xwing))
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

-- play loop
function game_screen:init()
	game_mode=0
	time_t=0
	parts={}
	actors={}
	npc_count=0
	plyr=make_plyr(0,0,0)
	
	init_ground()
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
			if rnd()>0.8 then
				target=make_npc(p,v,npc_xwing)
				v_plus_v(p,v,-10)
			end
			-- spawn new enemy
			for i=1,flr(1+rnd(2)) do
				local a=make_npc(p,v,npc_tie)
				a.target=target
				target=a
				v_plus_v(p,v,-10)
			end
		end
	elseif game_mode==1 then
		update_ground()
	end

	zbuf_filter(actors)
	zbuf_filter(parts)

end
function game_screen:draw()
	if game_mode==0 then
		draw_deathstar()
		draw_stars()
	elseif game_mode==1 then
		draw_ground()
	end

	zbuf_draw()
		
	-- cockpit
	if cam_mode==1 then
		palt(0,false)
		palt(14,true)
		spr(64,0,32,8,4)
		spr(64,64,32,8,4,true)
		spr(8,0,64,8,4)
		spr(8,64,64,8,4,true)
		spr(72,0,96,8,4)
		spr(72,64,96,8,4,true)
		-- radar
		draw_radar(64,112,12,10)
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
	cls()

	cur_screen:draw()
	
	futures_update(after_draw)

	time_dt=0

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
	for i=1,32 do
		add(stars,make_rnd_v(32))
	end
		
	cam=make_cam(64)

	cur_screen=start_screen
end

__gfx__
000000000000000000000000000000000000000000000000000000000000000000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee
0000000000000000000000000000000000000000000000000000000000000000000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee
00000000000000000000000000000000000000000000000000000000000000000000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11eeeeeee
000000000000000000000000000000000000000000000000000000000000000010000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000e11000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eee1000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeee11eeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeee1100000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeee110000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeee10000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeee1e
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeee11000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeee1eeeee1e
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeee1100000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeee111111e
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeee110000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeee10000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeee11000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeee1100000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeee110000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeee10000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeee11000000000001eeeeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeee1100000000001eeeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeee110000000001eeeeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeee10000000001eeeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000001eeeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000001eeeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000001eeeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000001eeeeeeeeeeeeeeeeeeee
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000011111111111111111111
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000111111111111111111111111
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000001000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000010011111111111111111111111
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000100100000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000001001000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000010010000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000100100000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000001001000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000010010000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000000100100000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000001001000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000000010010000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000000000100100000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000000001001000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000000000010010000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000000000100100000000000000000000000000000000000
1eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000000000001001000000000000000000000000000000000000
01eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000000000010010000000000000000000000000000000000000
001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000000000000100100000000000000000000000000000000000000
0001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000000000000001001000000000000000000000000000000000000000
00001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000000000000010010000000000000000000000000000000000000000
000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000000000000000100100000000000000000000000000000000000000000
0000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000000000000001001000000000000000000000000000000000000000000
00000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000010010000000000000000000000000000000000000000000
000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000100100000000000000000000000000000000000000000000
0000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000001001000111111111111111111111111100000000000000000
00000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000010010001000000000000000000000000010000000000000000
000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000100100001000000000000000000000000010000000000000000
0000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000001001000001000000000000000000000000010000000000000000
00000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000010010000001000000000000000000000000010000000000000000
000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000100100000001000000000000000000000000010000000000000000
0000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000001001000000001000000000000000000000000010000000000000000
00000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee111111e0000000010010000000000111111111111111111111111100000000000000000
000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeee1e0000000100100000000000000000000000000000000000000000000000000000
0000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee0000001001000000000000000000000000000000000000000000000000000000
__map__
0405232214191244737557738f5773758d738f8d8d75578d8f578d758d8d8f8d7a7ae57a87e5867ae58687e57d7dfd7d82fd837dfd8382fd638e5c638e80399d69399d80399db369825c638e5c698286638e8674885c6f935c7488866f93869b8d5c9b8d80c69969c69980c699b390935c9b8d5c9093869b8d868a885c95825c
8a888695828663745c6374803965693965803965b36f6f5c63745c6f6f86637486747a5c69805c747a866980869b755c9b7580c66969c66980c669b395805c9b755c9580869b75868a7a5c906e5c8a7a86906e8626010402030401080407111012070407090a0805040a0c020b030405080b01080406030c090c041019181a07
0405130e11040406120f14030404140d130d04151718160b040e1b16190a040f1a171c09040d1c151b1604232425221804252728261c04282a2b291a042b2d232c180426292c221d0427242d2a2304343536332504363839372904393b3c3a27043c3e343d2504373a3d332a0438353e3b300445464744320447494a4836044a
4c4d4b34044d4f454e3204484b4e44370449464f4c3d04565758553f04585a5b5943045b5d5e5c41045e60565f3f04595c5f5544045a57605d26608080a08082a0808080806080608080a080a0808480608280a0836080828080a0806084809f866080846372808080a09d8e808080608e6380729d80909c808080a070648080
80606490809c70807263808080a08e9d808080609d7280638e809c90808080a0647080808060709c80906480600301000102000204000403000703000408000807000507000806000605000105000602000a09000b09000a0c000c0b00070b000c08000309000a04000e0d000f0d000e1000100f000b0f00100c00090d000e0a
001112011314011214011311011415011816001617001719001918001c1800191d001d1c001a1c001d1b001b1a00161a001b17001e1f012021011f2101201e01212201252300232400242600262500292500262a002a29002729002a28002827002327002824002b2c012d2e012c2e012d2b012e2f0132300030310031330033
32003632003337003736003436003735003534003034003531003839013a3b01393b013a38013b3c013f3d003d3e003e4000403f00433f004044004443004143004442004241003d4100423e00051520191604188080a0698097608080698069808060978069a0808097809780a0a069a09760a08069a06980a06097a069a0a0
8097a09780b09372b08e6db08072b07280b06d8eb07293b0808eb08e110804120f11070604140d13050404160b15030204180917010104171012080704110e14060504130c16040304150a18020c04221b210b0a04241923090904232025100f04271e260e0d04281c220c0b04211a240a1004251f270f0e04261d280d12081a
1b1c1d1e1f2019119e808c8c806262807474809e8c809e9e807474806262808c6893767693988a939898937676936868938a98938a8a936880a080280201000302000403000504000605000706000807000108000a09000b0a000c0b000d0c000e0d000f0e00100f00091000070f00100800050d000e0600030b000c04000109
000a02001211001312001413001514001615001716001817001118000b1300140c00091100120a001018000e1600170f00150d00061e1b13101d10219780978097978080a06980978069976672809a728092808e6e808e9a8e80668e806e80729280724d76804d808a4d8a804d8076b37680b3808ab38a80b38076b380c0b3c8
a0b3c860b38040b33860b338a04d80c04dc8a04dc8604d80404d38604d38a00c05030506070103060801020308020304030205040b040e1513160c040f16141706040b1712180904091811150a04101d1c1e0d040c1f1a1d07040a20191f08040d1e1b200c8b759c8b8b9c758b9c75759c7d96977d96697d6a697d6a97839669
836a69836a978396972c010200030400020400040500030500030100050100030200090600080700060c000d07000a08000b09000b0c000d0a000f0e000e1100100f00101100090f00100b00110c000e06001312001512001413001514000d1500140a000712000813001716011817011918011a19011b1a01161b011d1c011e
1d011f1e01201f012120011c2101090f100c1f131e1f0c1d1b80a080749e80699780628c806080806274806969807462808060808c62809769809e7480a080809e8c809797808c9e807398806d96806b90806d8a80738880798a807b90807996807683808a8380a0808000001b02010103020104030105040106050107060108
07010908010a09010b0a010c0b010d0c010e0d010f0e01100f01011001121101131201141301151401161501171601181701111801051901191a011a1b01000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
