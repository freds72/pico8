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

-- false: chase
-- true: cockpit
local cockpit_view,cam,radar_cam=false
-- player
local plyr_playing,plyr=true
local spawn_t=0
local actors,ground_actors,parts,all_parts={},{},{}
-- ground constants
local ground_scale,ground_colors=4,{1,5,6}

local cur_screen
local start_screen={
	starting=false
}
local game_screen={
	starting=false
}
local gameover_screen={}

function nop() return true end

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
	fn=fn or nop
	local i=1
	while i<=t do
		if(not fn(i)) return
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
	v_scale(v,scale)
	return v
end

function make_rnd_pos_v(a,rng)
	local p=make_rnd_v(8)
	p[3]+=rng
	local d,v=0
	while d==0 do
		v=make_rnd_v(4)
		v_add(v,p,-1)
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
-- world axis
local v_fwd,v_right,v_up={0,0,1},{1,0,0},{0,1,0}

function v_clone(v)
	return {v[1],v[2],v[3]}
end
function v_lerp(a,b,t)
	return {
		lerp(a[1],b[1],t),
		lerp(a[2],b[2],t),
		lerp(a[3],b[3],t)}
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
	local d=v_dot(v,v)
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
function v_add(v,dv,scale)
	scale=scale or 1
	v[1]+=scale*dv[1]
	v[2]+=scale*dv[2]
	v[3]+=scale*dv[3]
end
function in_cone(p,t,fwd,angle,rng)
	local v=v_clone(t)
	v_add(v,p,-1)
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
	v[1],v[2],v[3]=m[1]*x+m[5]*y+m[9]*z,m[2]*x+m[6]*y+m[10]*z,m[3]*x+m[7]*y+m[11]*z
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
-- inline matrix invert
-- inc. position
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

-- models
local all_models=json_parse'{"title":{"c":10},"deathstar":{"c":3},"turret":{"c":8,"r":1.1,"wp":{"sfx":1,"dmg":1,"dly":12,"pos":[[-0.2,0.8,0.65],[0.2,0.8,0.65]],"n":[[0,0,1],[0,0,1]]}},"xwing":{"c":7,"r":0.8,"proton_wp":{"dmg":4,"dly":60,"pos":[0,-0.4,1.5],"n":[0,0,1]},"wp":{"sfx":2,"dmg":1,"dly":8,"pos":[[2,1,1.6],[2,-1,1.6],[-2,-1,1.6],[-2,1,1.6]],"n":[]}},"tie":{"c":5,"r":1.2,"wp":{"sfx":1,"dmg":2,"dly":24,"pos":[[0.7,-0.7,0.7],[-0.7,-0.7,0.7]],"n":[[0,0,1],[0,0,1]]}},"junk1":{"c":3,"r":1.2},"junk2":{"c":3,"r":1.2},"generator":{"c":6,"r":2},"mfalcon":{"c":5},"vent":{"c":5,"r":1},"ywing":{"c":7,"r":1,"wp":{"sfx":1,"dmg":1,"dly":18,"pos":[[0.13,0,3.1],[-0.13,0,3.1]],"n":[[0,0,1],[0,0,1]]}}}'
local _id=0
local dither_pat=json_parse'[0b1111111111111111,0b0111111111111111,0b0111111111011111,0b0101111111011111,0b0101111101011111,0b0101101101011111,0b0101101101011110,0b0101101001011110,0b0101101001011010,0b0001101001011010,0b0001101001001010,0b0000101001001010,0b0000101000001010,0b0000001000001010,0b0000001000001000,0b0000000000000000]'

local debug_vectors=false
function draw_actor(self,x,y,z,w)
	--[[
	local s=""
	if self.target then
		s=s.."☉"
	elseif self.recover then
		s=s.."⧗"
	elseif not self.target then
		s=s.."∧"
	end	
	s=s.." "..(flr(10*self.g)/10).."/"..flr(10*self.overg_t)/10
	print(s,x-8,y-w-8,self.recover and 8 or 11)
	]]
	-- distance culling
	if w>1 then
		draw_model(self.model,self.m,x,y,z,w)
		--[[
		if self.model.r then
			circ(x,y,self.model.r*w,7)
		end
		]]
	else
		pset(x,y,self.model.c)
	end
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
 		v_add(pos,self.pos,-1)
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
--[[
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
]]

-- unpack models
local mem=0x2000
function unpack_int()
	local i=peek(mem)
	mem+=1
	if(mem>=0x3000) mem=0x1000
	return i
end
function unpack_float(scale)
	local f=(unpack_int()-128)/32	
	return f*(scale or 1)
end
-- valid chars for model names
local itoa='_0123456789abcdefghijklmnopqrstuvwxyz'
function unpack_string()
	local n,s=unpack_int(),""
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
 	local model,name,scale={},unpack_string(),unpack_int()
		printh(name.."@"..scale)
 	-- vertices
 	model.v,n={},unpack_int()
 	for i=1,n do
 		add(model.v,{unpack_float(scale),unpack_float(scale),unpack_float(scale)})
 	end
 	-- faces
 	model.f,n={},unpack_int()
 	for i=1,n do
 		local f={unpack_int(),unpack_int()}
 		for k=1,f[2] do
 			add(f,unpack_int())
 		end
 		add(model.f,f)
 	end
 	-- normals
 	model.n,n={},unpack_int()
 	for i=1,n do
 		add(model.n,{unpack_float(),unpack_float(),unpack_float()})
 	end
 	
 	-- n.p cache	
 	model.cp={}
		for i=1,#model.f do
			local f=model.f[i]
			add(model.cp,v_dot(model.n[i],model.v[f[1]]))
		end
		 	
 	-- edges
 	model.e,n={},unpack_int()
 	for i=1,n do
 		add(model.e,{
 			unpack_int(), -- start
 			unpack_int(), -- end
 			unpack_int()==1 and true or -1
 		})
 	end

		-- merge with existing model
		all_models[name]=clone(model,all_models[name])
	end
end

-- little hack to perform in-place data updates
local draw_session_id=0
function draw_model(model,m,x,y,z,w)
	draw_session_id+=1

	color(model.c or 1)
	-- camera distance dithering
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
	local p,v={}
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

_g.die_plyr=function(self)
	-- todo: fix
	plyr.disabled=true
	plyr_playing=false
	cam.flip=false
	set_view(false)

	futures_add(function()
		wait_async(30)
		-- stop tracking player
		-- cam:track(nil)
		
		local death_q=make_q(v_fwd,rndlerp(-0.04,0.04))
		local cam_acc=plyr.acc
		wait_async(90,function(i)
			q_x_q(plyr.q,death_q)
			local fwd=update_plyr_pos()
			--v_add(cam.pos,fwd,cam_acc)
			cam_acc*=0.98
			local p=make_rnd_v(0.5)
			v_add(p,plyr.pos)
			make_part("flash",p,rndlerp(8,10))
			return true
		end)
		make_part("blast",plyr.pos)
		wait_gameover(600)
	end)
end

_g.die_actor=function(self)
	make_part("blast",self.pos)
	self.disabled=true
	del(actors,self)
	-- notifies listeners (if any)
	if(self.on_die) self:on_die()
end

_g.update_exit=function(self)
	if(not plyr) return false
	if sqr_dist(self.pos,plyr.pos)<1 then
		make_msg("victory2")
		local wing=make_npc({0,56,0},v_up,"mfalcon")
		wing.target=plyr
		plyr_playing,cam.flip=false,true
		set_view(false)
		futures_add(function()
			wait_async(600)
			make_part("novae",{0,0,0})
			-- game over
			wait_gameover(600)
		end)
		return false
	end
	return true
end

_g.die_vent=function(self)
	_g.die_actor(self)
	make_msg("victory1")
	add(actors,clone(all_actors["exit"]))
end

-- offset: position relative to other pos
function follow(pos,other,offset)
	local v=v_clone(offset)
	-- offset into world position
	m_x_v(other.m,v)
	-- line to target
	v_add(v,pos,-1)
	return v
end
function avoid(self,pos,dist)
	local v={0,0,0}
	local n,d2=0,dist*dist
	for _,a in pairs(actors) do
		if a!=self then
			local p=v_clone(a.pos)
			v_add(p,pos,-1)
			local d=v_dot(p,p)
			if d>0 and d<d2 then
				v_add(v,p,-1)
			end
		end
	end
	return v
end
function seek(self,r)
	local fwd=m_fwd(self.m)
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

_g.update_flying_npc=function(self)
	-- npc still in range?
	if plyr and sqr_dist(self.pos,plyr.pos)>96*96 then
		-- notifies listeners (if any)
		if (self.on_die) self:on_die()
		return false
	end
	
	-- force application point 
	local pos,m={0,0,1},self.m
	m_x_v(m,pos)
	-- forces
	local can_fire,fwd=false,m_fwd(m)
	local force=v_clone(fwd)
	v_scale(force,25)

	if self.target and not self.target.disabled then
		-- friendly: formation flight
		local target_pos={0,-4,-10}
		-- enemy: get in sight
		if band(self.target.side,self.side)==0 then
			can_fire,target_pos=true,{0,0,-15}
		end
		local ff=follow(pos,self.target,target_pos)
		-- todo: diffent class of enemies
		v_add(force,ff,1-smoothstep(self.overg_t/25))
	elseif not self.recover then
		-- search for target
		self.target=seek(self,24)
	end
	-- nothing to track?
	if not self.target then
		if not self.wander or self.wander_t<time_t then
			-- pick a random location
			self.wander=wander(self)
			self.wander_t=time_t+120+rnd(60)
		end
		v_add(force,follow(pos,self,self.wander))
	end
	local avf=avoid(self,pos,8)
	-- weight avoid more than follow
	v_add(force,avf,4)

	-- clamp acceleration
	v_clamp(force,1.2*self.acc)

	-- update orientation
	v_add(pos,force)
	v_add(pos,self.pos,-1)
	v_normz(pos)

	local f=v_clone(force)
 v_normz(f)
	self.g+=1-abs(v_dot(f,fwd))
	
	if self.g>0.5 then
		self.overg_t+=1
	end
	if self.overg_t>25 and not self.recover then
		local target,forget_t=self.target,15+self.overg_t
		-- forget target
		can_fire,self.target,self.recover=false,nil,true
		futures_add(function()
			wait_async(forget_t)
			self.target,self.recover=target,false
		end)
	end
	self.g*=0.98
	self.overg_t=mid(self.overg_t*0.98,0,35)
	
 -- try to align w/ target
	local up={m[5],m[6],m[7]}
	if self.target then
		local up_target={self.target.m[5],self.target.m[6],self.target.m[7]}
		v_add(up,up_target,0.2)
	end
	m=make_m_toward(pos,up)
	-- move actor using force
	v_add(self.pos,force)
	m_set_pos(m,self.pos)
	self.m=m

	-- fire solution?
	local fwd=m_fwd(m)
	if self.model.wp and can_fire and self.fire_t<time_t and in_cone(self.pos,self.target.pos,fwd,0.92,24) then
 	self:fire(self.target.pos)
	end

	return true
end

_g.hit_npc=function(self,dmg)
	-- avoid reentrancy
	if(self.disabled) return
	self.hp-=dmg
	if self.hp<=0 then
		self:die()
	end
end
_g.hit_flying_npc=function(self,dmg,actor)
	_g.hit_npc(self,dmg)
	-- todo: wait a bit
	if actor==plyr then
		self.target=plyr
	end
end

_g.update_turret=function(self,i,j)
	if(not plyr) return true
	
	self.pos[1],self.pos[3]=i*ground_scale,j*ground_scale
	local dx,dy=self.pos[1]-plyr.pos[1],self.pos[3]-plyr.pos[3]
	-- in range?
	local angle,m=1,self.m
	--if dx*dx+dy*dy<64 then
		angle=atan2(dx,dy)-0.25
		local q=make_q(v_up,angle)
		m=m_from_q(q)
		self.m=m
	--end
	m_set_pos(m,self.pos)
	
	if abs(angle)<0.2 then
		self:fire(plyr.pos)
		self.fire_t=time_t+self.model.wp.dly
	end	
	return true
end
_g.update_junk=function(self,i,j)
	self.pos[1],self.pos[3]=i*ground_scale,j*ground_scale
	m_set_pos(self.m,self.pos)
end
_g.make_laser=function(self,target)
	if(self.fire_t>time_t) return false
	
	local wp=self.model.wp
	local i=self.laser_i%#wp.pos+1
	-- rebase laser in world space
	local p=v_clone(wp.pos[i])
	m_x_v(self.m,p)
	-- direction override?
	local v
	if target then
		v=v_clone(target)
		v_add(v,p,-1)
		v_normz(v) 
	else
		v=v_clone(wp.n[i])
		o_x_v(self.m,v)
	end
	self.laser_i+=1
	-- laser colors
	local c=self.side==good_side and 11 or 8
	local pt=add(parts,clone(all_parts["laser"],{
			actor=self, -- laser owner
			pos=p,
			u=v,
			c=c,
			side=self.side,
			dmg=wp.dmg}))
	pt.t=time_t+pt.dly
	self.fire_t=time_t+wp.dly
	if (wp.sfx) sfx(wp.sfx)
	make_part("flash",p,c)
	return true
end

_g.make_proton=function(self,target)
	local wp=self.model.proton_wp
	-- rebase wp in world space
	local p=v_clone(wp.pos)
	m_x_v(self.m,p)
	-- fire direction in world space
	v=v_clone(wp.n)
	o_x_v(self.m,v)

	local pt=add(parts,clone(all_parts["proton"],{
		-- proton owner
		actor=self,
		target=target,
		pos=p,
		u=v,
		side=self.side,
		dmg=wp.dmg}))
	pt.t=time_t+pt.dly
	if (wp.sfx) sfx(wp.sfx)
	make_part("flash",p,c)
end

local all_actors=json_parse'{"plyr":{"hp":5,"safe_t":0,"energy":1,"energy_t":0,"boost":0,"acc":0.2,"model":"xwing","roll":0,"pitch":0,"laser_i":0,"fire_t":0,"fire":"make_laser","lock_t":0,"proton_t":0,"proton_ammo":4,"fire_proton":"make_proton","side":"good_side","die":"die_plyr"},"patrol":{"hp":800,"acc":0.2,"g":0,"overg_t":0,"rnd":{"model":["xwing","xwing","ywing"]},"side":"good_side","wander_t":0,"lock_t":0,"laser_i":0,"fire_t":0,"fire":"make_laser","update":"update_flying_npc","hit":"hit_npc","die":"die_actor"},"tie":{"hp":4,"acc":0.2,"g":0,"overg_t":0,"model":"tie","side":"bad_side","wander_t":0,"lock_t":0,"laser_i":0,"fire_t":0,"fire":"make_laser","update":"update_flying_npc","hit":"hit_flying_npc","die":"die_actor"},"generator":{"waypt":true,"hp":2,"model":"generator","side":"bad_side","update":"nop","hit":"hit_npc","die":"die_actor"},"vent":{"waypt":true,"hp":2,"model":"vent","side":"bad_side","update":"nop","hit":"hit_npc","die":"die_vent"},"mfalcon":{"hp":8,"acc":0.4,"g":0,"overg_t":0,"model":"mfalcon","side":"good_side","wander_t":0,"lock_t":0,"update":"update_flying_npc","hit":"hit_npc","die":"die_actor"},"turret":{"hp":2,"model":"turret","side":"bad_side","fire_t":0,"laser_i":0,"fire":"make_laser","update":"update_turret","hit":"hit_npc","die":"die_actor"},"ground_junk":{"hp":2,"rnd":{"model":["junk1","junk1","junk2"]},"side":"bad_side","update":"update_junk","hit":"hit_npc","die":"die_actor"},"exit":{"pos":[0,64,0],"draw":"nop","update":"update_exit","waypt":true}}'
		
function make_plyr(x,y,z)
	local p=clone(all_actors["plyr"],{
		pos={x,y,z},
		q=make_q(v_right,0.75),
		hit=function(self,dmg)
			if(self.disabled or self.safe_t>time_t) return
			self.energy,self.safe_t=0,time_t+8
			self.hp-=dmg
			if self.hp<=0 then
				self:die()
			end
			screen_shake(rnd(),rnd(),2)
		end,
		draw=function(self,x,y,z,w)
			if cockpit_view then
				return
			end
			draw_model(self.model,self.m,x,y,z,w)
		end,
		update=function(self)
			-- energy
			plyr.energy=min(plyr.energy+0.01,1)
			-- refill shield + proton
			if plyr.energy==1 and plyr.energy_t<time_t then
				plyr.proton_ammo=min(plyr.proton_ammo+1,4)
				plyr.hp=min(plyr.hp+1,5)
				plyr.energy,plyr.energy_t=0,time_t+120
			end
	
			-- damping
			plyr.roll*=0.9
			plyr.pitch*=0.9
			plyr.boost*=0.9
		
			-- update radar cam
			radar_cam:track(m_x_xyz(plyr.m,0,12,-24),plyr.q)
						
			return true
		end
	})
	p.model=all_models[p.model]
	add(actors,p)
	return p
end

function make_npc(p,v,src)
	_id+=1
	-- instance
	local a=clone(all_actors[src],{
		id=_id,
		pos=v_clone(p),
		q=make_q(v,0),
		draw=draw_actor
	})
	a.model=all_models[a.model]

	-- init orientation
	local m=m_from_q(a.q)
	m_set_pos(m,p)
	a.m=m
	return add(actors,a)
end

local rear_q=make_q(v_up,0.5)
function make_cam(f,x0,y0)
	x0,y0=x0 or 64,y0 or 64
	local c={
		pos={0,0,3},
		q=make_q(v_up,0),
		focal=f,
		flip=false,
		update=function(self)
			self.m=m_from_q(self.q)
			m_inv(self.m)
		end,
		track=function(self,pos,q)
 		self.pos=v_clone(pos)
 		if self.flip then
 		 q=q_clone(q)
 			q_x_q(q,rear_q)
 		end
 		self.q=q
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
 			return x0+v[1]*w,y0-v[2]*w,v[3],w
		end
	}
	return c
end

_g.update_part=function(self)
	if(self.t<time_t or self.r<0) return false
	self.r+=self.dr
	return true
end
_g.update_blast=function(self)
	if self.frame==8 then
		self.kind=5
		self.dr=-0.2
		for i=1,self.sparks do
			local v=make_rnd_v(rnd(self.r))
			v_add(v,self.pos)
			make_part("spark",v)
		end
	end
	self.frame+=1
	return _g.update_part(self)
end

_g.die_blt=function(self)
	make_part("flash",self.pos)
	-- to be removed from set
	return false
end

function blt_obj_col(self,objs)
	for _,a in pairs(objs) do
		local r=a.model and a.model.r or nil
		if r and band(a.side,self.side)==0 then
			r*=r
			local hit=false
			-- edge case: base or tip inside sphere
			if sqr_dist(self.pos,a.pos)<r or sqr_dist(self.prev_pos,a.pos)<r then
				hit=true
			else
				local p=v_clone(self.prev_pos)
				v_add(p,self.pos,-1)
				local max_t=v_normz(p)
				-- vector to sphere
				local ps=v_clone(a.pos)
				v_add(ps,self.pos,-1)
				-- projection on ray
				local t=v_dot(p,ps)
				if t>=0 and t<=max_t then
					-- distance to sphere?
					v_scale(p,t)
					hit=sqr_dist(p,a.pos)<r
				end	
			end
			if hit then
				a:hit(self.dmg,self.actor)
				return true
			end	
		end
	end
	return false
end

_g.update_blt=function(self)
	if(self.t<time_t) return false
	
	-- ground?
	if self.pos[2]<0 then
		if abs(self.pos[1])>6 or self.pos[2]<-6 then
			return self:die()
		end
	end
	self.prev_pos=v_clone(self.pos)
	v_add(self.pos,self.u,self.acc)

	-- collision?
	if blt_obj_col(self,actors) or blt_obj_col(self,ground_actors) then
		return self:die()
	end
	
	return true
end

_g.update_proton=function(self)
 if time_t%2==0 then
 	make_part("trail",self.pos,10)
 end
 -- update orientation to match target
 if self.target and not self.target.disabled then
		-- old enough?
		local v=v_clone(self.target.pos)
		v_add(v,self.pos,-1)
		-- not too close?
		if v_dot(v,v)>0.25 then
			v_normz(v)
 		-- within cone?
 		if v_dot(self.u,v)>0.6 then
 			v_add(self.u,v,smoothstep(self.frame/60))
 			v_normz(self.u)
 		end
 	end
 end
 self.frame+=1
 return _g.update_blt(self)
end

_g.draw_part=function(self,x,y,z,w)
	if self.kind==0 then
		local x1,y1,z1,w1=cam:project(self.prev_pos[1],self.prev_pos[2],self.prev_pos[3])
		if z>0 and z1>0 then
			line(x,y,x1,y1,time_t%2==0 and 7 or self.c)
		end
	elseif self.kind==1 then
		circfill(x,y,self.r*w,self.c)
	elseif self.kind==2 then
		circfill(x,y,self.r*w,7)
	elseif self.kind==3 then
		-- light effect
		fillp(dither_pat[mid(#dither_pat-flr(w/2),1,#dither_pat)])
		circfill(x,y,(0.5+rnd(1))*w,8)
		fillp()
		circfill(x,y,(0.1+0.2*rnd())*w,10)
	elseif self.kind==5 then
		circ(x,y,w*self.r,7)
	elseif self.kind==6 then
		pset(x,y,15*rnd())
	end
end

all_parts=json_parse'{"laser":{"rnd":{"dly":[80,110]},"acc":3,"kind":0,"update":"update_blt","draw":"draw_part","die":"die_blt"},"flash":{"kind":1,"rnd":{"r":[0.3,0.5],"dly":[6,10]},"dr":-0.05},"trail":{"kind":1,"rnd":{"r":[0.2,0.3],"dly":[12,24]},"dr":-0.02},"blast":{"frame":0,"sfx":3,"kind":1,"c":7,"rnd":{"r":[2.5,3],"dly":[8,12],"sparks":[6,12]},"dr":-0.04,"update":"update_blast"},"novae":{"frame":0,"sfx":3,"kind":1,"c":7,"r":30,"rnd":{"dly":[8,12],"sparks":[30,40]},"dr":-0.04,"update":"update_blast"},"proton":{"rnd":{"dly":[90,120]},"frame":0,"acc":0.6,"kind":3,"update":"update_proton","draw":"draw_part","die":"die_blt"},"spark":{"kind":6,"dr":0,"r":1,"rnd":{"dly":[24,38]}}}'

function make_part(part,p,c)
	local pt=add(parts,clone(all_parts[part],{c=c or 7,pos=v_clone(p)}))
	pt.t,pt.update,pt.draw=time_t+pt.dly,pt.update or _g.update_part,_g.draw_part
	if(pt.sfx) sfx(pt.sfx)
	return pt
end

function draw_ground(self)
	if(cam.pos[2]<0) return
	
	local scale=4*mid(flr(cam.pos[2]/32+0.5),1,4)
	if scale==16 then
		draw_deathstar(12/(cam.pos[2]/24))
		draw_stars()
		return
	end
	--scale*=scale
	local v={}
	local x0,z0=cam.pos[1],cam.pos[3]
	local dx,dy=x0%scale,z0%scale
	
	for i=-8,8 do
		local ii=scale*i-dx+x0
		-- don't draw on trench
		if abs(flr(ii-x0+cam.pos[1]))>=8 then
			for j=-8,8 do
				local jj=scale*j-dy+z0
				local x,y,z,w=cam:project(ii,0,jj)
				if z>0 then
					pset(x,y,ground_colors[mid(flr(4*w),1,3)])
				end
			end
		end
	end
end
local turrets={}
function make_ground_actor(i,j,src,y)
	local x,y,z=i*ground_scale,y or 0,j*ground_scale
	local a=clone(all_actors[src],{
		pos={x,y,z},
		m=make_m(x,y,z),
		draw=draw_actor
	})
	a.model=all_models[a.model]
	turrets[i+j*128]=a
	return a
end

function init_ground()
	for i=0,127 do
		for j=0,127 do
			local r=rnd()
			if r>0.995 then
				make_ground_actor(i,j,"turret")
			elseif r>0.98 then
				make_ground_actor(i,j,"ground_junk")
			end
		end
	end
end

function update_ground()
	ground_actors={}
	
	local i0,j0=flr(plyr.pos[1]/ground_scale),flr(plyr.pos[3]/ground_scale)
	for i=i0-9,i0+9 do
		local cx=(i%128+128)%128
		for j=j0-9,j0+9 do
			local cy=(j%128+128)%128
			local t=turrets[cx+cy*128]
			if t and not t.disabled then
				t:update(i,j)
				add(drawables,t)
				add(ground_actors,t)
			end
		end
	end
end

local trench_scale=6
function make_trench(i)
	local x,y,z=0,0,i*trench_scale
	local t={
		pos={x,y,z},
		m=make_m(x,y,z),
		side=no_side,
		model=all_models.trench1,
		update=function(self)
			local dz=cam.pos[3]-cam.pos[3]%(2*trench_scale)
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
end

-- radio message
local all_msgs=json_parse'{"attack1":{"spr":12,"title":"ackbar","txt":"clear tie squadrons","dly":300,"can_skip":true},"ground1":{"spr":12,"title":"ackbar","txt":"destroy shield\ngenerators","dly":300,"can_skip":true},"victory1":{"spr":104,"title":"han solo","txt":"get out of here son.\nquick!","dly":300},"victory2":{"spr":12,"title":"ackbar","txt":"victory!","dly":300},"victory3":{"spr":10,"lipsc":8,"title":"leia","txt":"the rebellion thanks you\nget to the base","dly":300,"can_skip":true},"help":{"spr":10,"rnd":{"title":["red leader","alpha","delta wing"]},"txt":"help!","dly":300},"low_hp":{"spr":76,"title":"r2d2","txt":"..--..-..","dly":120,"sfx":4,"rnd":{"repeat_dly":[600,900]}}}'
local cur_msg
function make_msg(msg)
	local m=clone(all_msgs[msg])
	m.t=time_t+m.dly
	if (m.sfx) sfx(m.sfx)
	cur_msg=m
end

function update_msg()
	if cur_msg then
		if(cur_msg.t<time_t) cur_msg=nil return
		if(cur_msg.can_skip and btnp(4) or btnp(5)) cur_msg=nil return
	end
end
function draw_msg()
	local y=2
	rectfill(32,y,49,y+18,0)
	rect(32,y,49,y+18,1)
	spr(cur_msg.spr,33,y+1,2,2)
	print(cur_msg.title,51,y,9)
	print(cur_msg.txt,51,y+7,7)
	-- cheap comms effect
	if time_t%4>2 then
		fillp(0b1011000011110100.1)
		rectfill(33,y,49,y+23,0)
		fillp()
 end
	
	-- lips animation
	local c=cur_msg.lipsc or 0
	if time_t%64>50 then
		rectfill(40,y+13,41,y+14,c)
	elseif time_t%64>30 then
		line(40,y+13,41,y+13,c)
 	elseif time_t%64>15 then
		line(39,y+13,42,y+13,c)
 	end
end

local turn_t=0

function plyr_ground_col(pos)
	-- ground collision?
	if pos[2]<0 then
		local r,col=rnd()*0.4,false
		if abs(pos[1])<=6 then
			if pos[1]>=5.9 then
				pos[1],col=5.5-r,true
			elseif pos[1]<=-5.9 then
				pos[1],col=-5.5+r,true
			end
			if pos[2]<-6 then
				pos[2],col=-5.5+r,true
			end
			-- between trench walls?
			if(not col) return false
		else
			pos[2]=r
		end
		-- take damage
		plyr:hit(1)
		return true
	end
	return false
end

local view_offsets,view_offset=json_parse'[[0,2,-8],[0,0,0]]'
local view_changing=false
function set_view(target_view)
 -- nothing to do?
	if(view_changing or cockpit_view==target_view) return
	view_changing=true
	futures_add(function()
		local c=cockpit_view
		cockpit_view=false
		wait_async(30,function(i)
			view_offset=v_lerp(
				view_offsets[c and 2 or 1],
				view_offsets[target_view and 2 or 1],
				smoothstep(i/30))
			return true
		end)
		cockpit_view,view_changing=target_view,false
	end)
end

function find_closest_tgt(fwd,objs,min_dist,target)
	min_dist=min_dist or 32000
	for _,a in pairs(objs) do
		if a.hp and band(a.side,plyr.side)==0 then
			local d=sqr_dist(a.pos,plyr.pos)
			if d>2 and d<min_dist and in_cone(plyr.pos,a.pos,fwd,0.993,64) then
				min_dist,target=d,a
			end
			-- collision?
			local r=plyr.model.r+a.model.r
			if(d<r*r) plyr:hit(1)
		end
	end
	return min_dist,target
end

function update_plyr_pos()
	local m=m_from_q(plyr.q)
	local fwd=m_fwd(m)
	v_add(plyr.pos,fwd,plyr.acc+plyr.boost)
	plyr_ground_col(plyr.pos)	
	m_set_pos(m,plyr.pos)
	plyr.m=m
	return fwd
end

function control_plyr(self)
	if(not plyr_playing) return
	
	local pitch,roll=0,0
	
	if(btn(0)) roll=-1 turn_t+=1
	if(btn(1)) roll=1 turn_t+=1
	if(btn(2)) pitch=-1
	if(btn(3)) pitch=1

	-- flat turn
	turn_t=min(turn_t,8)
	if roll!=0 then
		self.roll=-roll/256
	else
		turn_t=0
	end
 self.roll=mid(self.roll,-1/96,1/96)
	local r=turn_t/8
	local q=make_q(v_up,(1-r)*roll/128)
	q_x_q(plyr.q,q)
	q=make_q(v_fwd,-r*roll/128)
	q_x_q(plyr.q,q)
	
	if pitch!=0 then
	 self.pitch-=pitch/396
	end
 if plyr.boost>0 then
	 self.pitch=mid(self.pitch,-0.005,0.005)
	else
	 self.pitch=mid(self.pitch,-1/256,1/256)
	end
	local q=make_q(v_right,self.pitch)
	q_x_q(plyr.q,q)
	
	-- update pos
	local fwd=update_plyr_pos()
	
	-- boost 
	if btn(4) then
		plyr.boost=min(plyr.boost+0.01,0.1)
	end
	
	-- cam modes
	if btnp(0,1) then
		set_view(not cockpit_view)
	end
	-- behind look?
	cam.flip=false
	if btn(2,1) then
		cam.flip=true
	end

	-- update cam
	cam:track(m_x_xyz(plyr.m,view_offset[1],view_offset[2],cam.flip and -view_offset[3] or view_offset[3]),plyr.q)
	
	-- find nearest enemy (in sight)
	local min_dist,target=find_closest_tgt(fwd,actors)
	min_dist,target=find_closest_tgt(fwd,ground_actors,min_dist,target)
	
	plyr.target=target
	if target then
		plyr.lock_t+=1
	else
		plyr.lock_t=0
	end
	if plyr.proton_ammo>0 and plyr.proton_t<time_t and plyr.lock_t>30 and btnp(4) then
		plyr:fire_proton(target)
		plyr.proton_t=time_t+plyr.model.proton_wp.dly
		plyr.proton_ammo-=1
		plyr.energy=0
	end
		
	if btnp(5) then
		plyr.energy=max(plyr.energy-0.1)
		if(plyr.energy>0) plyr:fire(target and target.pos or nil)
	end	
end

-- deathstar
local ds_m=make_m()
function draw_deathstar(offset)
	offset=offset or -6
	m_set_pos(ds_m,{cam.pos[1],offset+cam.pos[2],cam.pos[3]})
	draw_model(all_models.deathstar,ds_m)
end

local stars,hyper_space={},false
local stars_ramp={1,5,6,7}
function draw_stars()
	hyper_space=plyr and plyr.boost>0
 for i=1,#stars do
		local v=stars[i]
		local x,y,z,w=cam:project(v[1],v[2],v[3])
		if z>0 and z<32 then
			w=flr(4*w/12)
			color(stars_ramp[min(w+1,#stars_ramp)])
			if hyper_space and v.x then
				line(v.x,v.y,x,y)
			else
				pset(x,y)
			end
			v.x,v.y=x,y
		else
			-- reset pos
			local star=make_rnd_v(32)
			v[1],v[2],v[3]=star[1],star[2],star[3]
			v.x,v.y=nil,nil
			v_add(v,cam.pos)
		end
	end
end

local radar_cols,shield_spr=json_parse'[5,11,3]',json_parse'[72,14,45,43,74]'
function draw_radar_dots(objs)
	for _,a in pairs(objs) do
		if a!=plyr then
			local v=v_clone(a.pos)
			m_inv_x_v(plyr.m,v)
			local x,y,c=64+0.2*v[1],116-0.2*v[3],mid(flr(v[2]/4),-1,1)+2
			pset(x,y,radar_cols[c])
		end
	end
end

function draw_radar()
 clip(54,105,22,22)
	draw_radar_dots(ground_actors)
	draw_radar_dots(actors)
	pset(64,116,7)		
	clip()
	
	-- draw waypoints
	for _,a in pairs(actors) do
		if a.waypt then
 			local x,y,z,w=cam:project(a.pos[1],a.pos[2],a.pos[3])
 			if z>0 and w<4 then
 				x,y=mid(x,4,124),mid(y-2*w,4,124)
 				spr(41,x-4,y-4)
 		 		--local d=flr(10*sqrt(sqr_dist(a.pos,plyr.pos)))/10
 		 		--print(d.."nm",x-4,y-10,8)		
			end
		end
	end
	
	-- draw lock
	if plyr.target then
		local p=plyr.target.pos
		local x,y,z,w=cam:project(p[1],p[2],p[3])
		if z>0 then
			if plyr.lock_t>30 then
				pal(8,time_t%2==0 and 10 or 11)
			end
			w=max(w,4)
			spr(40,x-w,y-w)
			spr(40,x+w-8,y-w,1,1,true)
			spr(40,x-w,y+w-8,1,1,false,true)
			spr(40,x+w-8,y+w-8,1,1,true,true)
			pal()
			--print(plyr.lock_t,x+w+2,y,8)
		end
	end
	
	-- todo: convert to sprite?
	-- proton ammo
	for i=0,plyr.proton_ammo-1 do
		local x,y=77+(i%2)*6,102+6*flr(i/2)
		spr(42,x,y)
	end

 	-- shield
 	spr(shield_spr[max(plyr.hp,1)],39,104,2,2)
 	if plyr.hp!=5 and time_t%4<2 then
		spr(shield_spr[max(plyr.hp+1,1)],39,104,2,2)
	end

	-- altitude	 
	if plyr.pos[2]<10 then
		local h=tostr(flr(10*plyr.pos[2])/10)
		local dy=12*(plyr.pos[2]/10)
		fillp(0b1010010110100101.1)
		line(53,74,53,60,1)
		fillp()
		line(40,74-dy,53,74-dy,1)
		print(h,56-5*#h,68-dy,9)
	end
end

-- todo: memset
local colors={0,1,0,14}
function set_layer(top)
	pal()
 for i=0,15 do
  local id=(top and i%4 or flr(i/4))
  if id==0 then
   palt(i,true)
  else
   pal(i,colors[id+1])
  end
 end
end

-- wait loop
function start_screen:update()
	if not self.starting and (btnp(4) or btnp(5)) then
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
	m_set_pos(m,{-0.85,0.4,2.1+cam.pos[3]})
	draw_model(all_models.title,m)
	print("attack on the death star",20,78,12)
	
	if (starting and time_t%2==0) or time_t%24<12 then	
		print("press start",44,118,11)
	end
end

function wait_gameover(t)
	del(actors,plyr)
	plyr=nil
	wait_async(t or 0)
	cur_screen=gameover_screen
	wait_async(600,function()
		if btnp(4) or btnp(5) then
			-- "eat" btnp
			yield()
			return false
		end
		return true
	end)
	cur_screen=start_screen
end

function gameover_screen:update()
end

function gameover_screen:draw()
	print("game over",38,60,6)
end

-- mission start routines
_g.create_generator_group=function()
	return {
		make_npc({256,7,256},v_up,"generator"),
		make_npc({-256,7,256},v_up,"generator"),
		make_npc({-256,7,-256},v_up,"generator"),
		make_npc({256,7,-256},v_up,"generator")}
end
_g.create_vent=function()
	return {make_npc({0,-6,128},v_up,"vent")}
end

_g.create_flying_group=function()
	local p,v=make_rnd_pos_v(plyr,64)
	-- default target: player
	local target=plyr
	-- friendly npc?
	if rnd()>0.8 then
		target=make_npc(p,v,"patrol")
		make_msg("help")
		v_add(p,v,10)
	end
	-- spawn new enemy
	local npcs={}
	for i=1,flr(2+rnd(2)) do
		local a=make_npc(p,v,"tie")
		a.target=target
		v_add(p,v,10)
		add(npcs,a)
	end
	return npcs
end

local all_missions=json_parse'[{"msg":"attack1","init":"create_flying_group","dly":"180","stage":5},{"msg":"ground1","init":"create_generator_group","dly":"180","stage":0}]'
local cur_mission_i,cur_mission=0
function next_mission()
 local new_mission=true
 -- repeat mission?
	if cur_mission then
		cur_mission.stage-=1
		new_mission=cur_mission.stage<=0
	end
	-- move to next mission?
	if new_mission then
		cur_mission_i+=1
		cur_mission=clone(all_missions[cur_mission_i])
		if not cur_mission then
			-- game over 
		end
		if cur_mission.msg then
			make_msg(cur_mission.msg)
		end
	end
	-- create mission
	local npcs=cur_mission.init()
	-- die hook
	for _,a in pairs(npcs) do
		a.on_die=function(self)
			cur_mission.npcs-=1
		end
	end
	cur_mission.npcs=#npcs
end

-- play loop
function game_screen:init()
	time_t,cockpit_view,view_offset,actors,parts=0,false,v_clone(view_offsets[2]),{},{}
	
	plyr=make_plyr(0,128,0)
	-- move to cockpit
	set_view(true)

	init_ground()
	init_trench(8)
end

local low_hp_t=0
function game_screen:update()
	zbuf_clear()
	
	if cur_msg then
		update_msg()
	end
	
	if plyr then
		control_plyr(plyr)
		if plyr.hp<2 and low_hp_t<time_t and rnd()>0.95 then
			make_msg("low_hp")
			low_hp_t=time_t+cur_msg.repeat_dly
		end
	end
	
	if not cur_mission or cur_mission.npcs==0 then
		next_mission()
	end
	update_ground()

	zbuf_filter(actors)
	zbuf_filter(parts)
	
	-- must be done after update loop
	cam:update()
	radar_cam:update()
end

function game_screen:draw()
	draw_ground()
	
	zbuf_draw()
	
	-- msg
	if cur_msg then
		draw_msg()
	end
		
	-- cam modes
	if plyr then
 	if cockpit_view then
 	 if not cam.flip then
 	  
 			-- cockpit
 			set_layer(false)
  		spr(0,0,64,8,8)
  		spr(0,64,64,8,8,true)
 			set_layer(true)
  		spr(64,0,32,8,4)
  		spr(64,64,32,8,4,true)
  		pal()
  		palt()
  		
  		-- radar
  		draw_radar()
  		-- hp
  		local x=23
  		local imax=flr(8*plyr.energy)
  		for i=1,imax do
  			rectfill(x,120,x+1,123,11)
  			x+=3
  		end
  		for i=imax+1,8 do  
  			rectfill(x,120,x+1,123,1)
  			x+=3
  		end
  		-- engines
  		local p=(plyr.acc+plyr.boost)/(0.3)
  		rectfill(82,120,82+22*p,123,9)
  	else
  		set_layer(true)
  		spr(0,0,32,8,4)
  		spr(0,64,32,8,4,true)
  		rectfill(0,64,127,127,0)
  		rect(19,64,108,125,1)
  		pal()
  	end
  else
  	draw_radar()
  end
 end
 
 if cur_mission then
 	print(cur_mission.npcs,2,2,7)
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
	
	if(draw_stats) draw_stats()
end


function _init()
	-- read models from map data
	unpack_models()
	
	-- compute xwing laser aim
	local wp=all_models.xwing.wp
	for i=1,#wp.pos do
		local v=v_clone(wp.pos[i])
		v={-v[1],-v[2],48-v[3]}
		v_normz(v)
		add(wp.n,v)
	end
	
	-- stars
	for i=1,48 do
		add(stars,make_rnd_v(48))
	end
		
	cam=make_cam(64)
	radar_cam=make_cam(16,64,108)
	
	cur_screen=start_screen
end

-->8
-- stats
--[[
local cpu_stats={}

function draw_stats()
	-- 
	fillp(0b1000100010001111)
	rectfill(0,0,127,9,0x10)
	fillp()
	local cpu,mem=flr(100*stat(1)),flr(100*(stat(0)/2048))
	cpu_stats[time_t%128+1]={cpu,mem}
	for i=1,128 do
		local s=cpu_stats[(time_t+i)%128+1]
		if s then
			-- cpu
			local c,sy=11,s[1]
			if(sy>100) c=8 sy=100
			pset(i-1,9-9*sy/100,c)
		 -- mem
			c,sy=12,s[2]
			if(sy>90) c=8 sy=100
			pset(i-1,9-9*sy/100,c)
		end
	end
	if time_t%120>60 then
		print("cpu:"..cpu.."%",2,2,7)
	else
		print("mem:"..mem.."%",2,2,7)
	end
end
]]
__gfx__
99988888888888888884000000000000000000000000000000000004000000000000000000000000000000000000000000000000000000000000000000000000
aaa99999888888888888400000000000000000000000000000000004000000000000000000000000000000000000000000000012990000000000000000000000
aaaaaaaa999998888888840000000000000000000000000000000004400000000000000000000000000000066000000000000124499000000000000000000000
5aaaaaaaaaaaa9999988884000000000000000000000000000000000000000000000000000000000000011566666000000000124449000000000000000000000
05599999aaaaaaaaaa99999511111111111111111111111111111111111111110000045499400000000111577666600000000242449000000000070000000000
000488889999999aaaaaaaaa62222222222222222222222222222226622222220090549454940900001115577777660000002424944900000000070000000000
00004488888888899aaaa999951111111111111111111111111111151111111104454449454954400011155777777600000444424944400000b07770b0000000
000000448888888889aa98888840000000000000000000000000000400000000044544dffd445440001d999dd999d7000049a444944a940000b00000b0000000
000000004888888889aa98888884000000000000000000000000000400000040054544ffff4455400019999999979700004a04400940a400000bbbbb00000000
00000000044888889aaa9888888840000000000000000000000000004000004000504f0ff0f4050000199999999996000029a404404a92000000000000000000
00000000000448889aa98888888884000000000000000000000000000444444000005ffffff50000000d999ff999d00000024444444420000000000000000000
0000000000000449aaa98888888888400000000000000000000000000000000000000ffffff00000000dffffffffd00000000240042000000000000000000000
0000000000000005aa988888888888840000000000000000000000000000000000000df88fd0000000005ffffff5000000000714427000000000000000000000
0000000000000012669888888888888840000000000000000000000000000000000000dffd000000000005ffff50000000006711117600000000000000000000
00000000000000122144888888888888840000000000000000000000000000000000076666700000000000577500000000776561176677000000000000000000
00000000000000122100448888888888884000000000000000000000000000000006776677776000000000000000000000666655666666000000000000000000
000000000000012221000048888888888884000000000000000000000000000088000000aaaaaaaa000000000000000000000000000000000000000000000000
000000000000012210000004488888888888400000000000000000000000000080000000a000000a000000000000000000000000000000000000000000000000
000000000000122210000000044888888888840000000000000000000000000000000000a000000a000aa000000bbbbb00000000000bbbbb0000000000000000
0000000000001221000000000004488888888840000000000000000000000000000000000a0000a000aaaa0000b00000b000000000b00000b000000000000000
0000000000112221000000000000048888888884000000000000000000000000000000000a0000a000aaaa0000b00700b000000000b00700b000000000000000
00000001112222100000000000000044888888884000000000000000000000000000000000a00a00000aa0000000070000000000000007000000000000000000
00001112222222110000000000000000448888888400000000000000000000000000000000a00a0000000000b0b07770b0b0000000b07770b000000000000000
011122222222221210000000000000000048888888400000000000000000000000000000000aa00000000000b0b00000b0b0000000b00000b000000000000000
1222222222221122210000000000000000044888888400000000000000000000888000000000000000000000b00bbbbb00b00000000bbbbb0000000000000000
22222222211122222210000000000000000004488888444444444444444444448080000000000000000000000b0000000b000000000000000000000000000000
222222211222222222210000000000000000488888888888888888888888888888800000000000000000000000bbbbbbb0000000000000000000000000000000
22221112222222222222100000000000000488888888888888888888888888880000000000000000000000000000000000000000000000000000000000000000
21112222222222222222211111111111115999999999999999999999999999990000000000000000000000000000000000000000000000000000000000000000
1222222222222222222221222222222226aaaaaa6666666666666666666666660000000000000000000000000000000000000000000000000000000000000000
222222222222222222221222222222226aaaaaa6aaaaaaaaaaaaaaaaaaaaaaaa0000000000000000000000000000000000000000000000000000000000000000
22222222222222222221221111111115999999599555555555555555555555550000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000004888888488488888888888888888888888000000000000000000bbbbbbb000000000000000000000000000000000000000
000000000000000000000000000004888888488488888888888888888888888800000000000000000b0000000b00000000000000000000000000000000000000
00000000000000000000000000004888888488488888888888888888888888880000000000000000b00bbbbb00b0000000000000000000000000000000000000
00000000000000000000000000048888884884888888888888888888444444440000000000000000b0b00000b0b0000000000000000000000000000000000000
00000000000000000000000004488888848848888888888888888884888888880000070000000000b0b00700b0b0000000000000000000000000000000000000
00000000000000000000000448888888488488888888888888888884888888880000070000000000000007000000000000000055660000000000000000000000
00000000000000000000004888888884884888444444444444488848888888880000777000000000b0b07770b0b0000000005561167600000000000000000000
00000000000000000000448888888848848884888888888888848848888888880000000000000000b0b00000b0b0000000056617016760000000000000000000
00000000000000000044888888888488488884888888888888848844444444440000000000000000b00bbbbb00b0000000566610016776000000000000000000
000000000000000004888888888848848888848888888888888484888888888800000000000000000b0000000b00000000566611116676000000000000000000
0000000000000004488888888884884888888488888888888884848888488888000000000000000000bbbbbbb000000005611666666676600000000000000000
00000000000004488888888888488488888884888888888888848488884888880000000000000000000000000000000005666666666666600000000000000000
00000000000448888888888884884888888884888888888888848488888488880000000000000000000000000000000005611616111616600000000000000000
10000000004888888888888848848888888884888888888888848488888488880000000000000000000000000000000005666616181616600000000000000000
21000000448888888888888488488888888884888888888888848488888848880000000000000000000000000000000001550000005575100000000000000000
22100044888888888888884884888888888884888888888888848488888848880000000000000000000000000000000001566166111666100000000000000000
22214488888888888888848848888888888884888888888888848488888884880000000000000000000000000000000000000000000000000000000000000000
22269888888888888888488488888888888884888888888888848488888884880000000000000000000000000000000000000000000000000000000000000000
266aa988888888888884884888888888888884888888888888848488888888880000000000000000000000000000000000000000000000000000000000000000
6aaaaa98888888888848848888888888888884888888888888848488888888880000000000000000000000000000000000000000000000000000000000000000
aaaaaaa9888888888488488888888888888888444444444444488444888888880000000000000000000000000000000000000000000000000000000000000000
aaaaaaaa988888884884888888888888888888888888888888888488888888880000000000000000000000000000000000000000000000000000000000000000
aaaaaaaaa98888848848884444444444444444444444444888888488888888880000044499400000000000000000000000000000000000000000000000000000
aaaaaaaaaa9888488488848888888888888888888888888488888488888888880000444994440000000000000000000000000000000000000000000000000000
aaaaaaaaaaa98488488884888888888888888888888888848888848888888888000044ffff440000000000000000000000000000000000000000000000000000
aaaaaaaaaaaa588488888488888888888888888888888884888884888888888800004f0ff0f40000000000000000000000000000000000000000000000000000
aaaaaaaaaaa6a94888888488888888888888888888888884888884888888888800005ffffff50000000000000000000000000000000000000000000000000000
aaaaaaaaaa6aa69888888488888888888888888888888884888884888888888800000ffffff00000000000000000000000000000000000000000000000000000
aaaaaaaaa6aa6aa9888884888888888888888888888888848888848888888888000009f00f900000000000000000000000000000000000000000000000000000
aaaaaaaa6aa6aaaa9888884444444444444444444444444888888488899999980000009ff9000000000000000000000000000000000000000000000000000000
aaaaaaa6aa6aaaaaa988888888888888888888888888888888888488988888980000111551110000000000000000000000000000000000000000000000000000
aaaaaa6aa6aaaaaaaa98888888888888888888888888888888888845444444440001111771111000000000000000000000000000000000000000000000000000
3290904032025201f04072e162e0d04082c122c0b04012a142a0014052f172f0e04062d182d001b9d8b8b8d85656d85757d8b9b8d8b9b9d85757d85656d8b8b9
e8b8b8e85657e85656e8b8b8e8b9b9e85756e85757e8b982201000302000403000504000605000706000807000108000a09000b0a000c0b000d0c000e0d000f0
e00001f00090010070f00001800050d000e0600030b000c04000109000a02000211100312100413100514100615100716100817100118100b0310041c0009011
0021a000018100e0610071f00051d000708111c071e0a191308796783a26783ad5c83ad5293a26793a96793ad6293ad6c83a56f8f92978b53988e5298816f898
36b89816a888e5b888b5f8789578b8c668b89678a866a8a846e8a866f8a896d8b8c6a8b8e63bc77b3b87fa3b871a8bf77bcbd7facbd7d98b487bcb68facb68d9
3b787b3bb8fa3bb81afa487bba68faba688afaf77bbad7fabad78a98687c77481e844829844886d548a40848f33a48a48b48868b482998481e08c80977c8d817
c85817c8b777c83708c80798c837f8c8b7f8c85898c8d87727d81727581727b7772737082707982737f827b7f827589827d808270977d71e84d72984d786d5d7
a408d7f33ad7a48bd7868bd72998d71e98977c77687c77977cba488a4bd7994b4899e77875c78895c788d5e798f52898f54888d5488895287875e7b8b6c7b896
c7a856e7a83628a83648a85648b89628b8b6e678b5c688e5e688161798364798166788e54788b517789567b8e637b8c627a89637a86667a846a7a866b7b896a7
b8c643b14012723262c14022624252e14032c282b2f14042b292a212408213d20322409203e2f25240e25333434240d2632353d16072e3b36313c2824033a383
93724023b373a3b24083d322c3a24073e312d373a08494a4b4c4d4e4f4057484a085756555453525a51595d250f34684567763500566a75674437064a8688766
f47633405476e48623404486d49613403496c4a6034024a6b4b6f24014b6a4c6e24004c694462570c8d898d685e6b7a4402677e7c7b45036c707a517c440b517
2527d440c5273537e440d5374547f440e54755570540f55765671540066775d604500797e695155540e7a7d797c24087f6b7d7e240f32636f7f24004f7b50803
401408c51813402418d52823403428e53833404438f548434054480658534068e8c8f6c13088d8c36540a8b8d8e88540645898b8f130788852754078a216b822
40f2f8091652304319f8823029391943f866c8094608e908c80a0808f8a9c809c90807c90817a9c808080a0608082608c80746081766c8080a08080608b7f938
080a3858f93868f90858f9d7280ab7e70ab7b7f9d7a7f9086816480a0808a71648871608a716c7e716972816976816c788160808064808080a06080856081906
08086608d6670826a80826a908d60a0808b90819a6d637b90819b90819885617f80846d879e687d9b87608493b20101030201040301050401060501070601080
701010801010901050901090601030901090401090201080901090701011a010b0c010c0d010d0e010e0f010f00110011110a0b0103121104131105141106151
10716110817110918110219110a1b100b1c100d1e100e1f100c1f100e1b100a1d100021200122200f1220012e100d10200324200425200225200421200023200
62720072820082520042720062320092a200a2b200b2820072a200926200c1b200a2b100a19200e2d200f2e20003f20013030023130033230043330085430073
6300837300938300a39300b3a300c3b300d3c300e3d300f3e30063f300940400142400243400344400445400546400647400748400849400041400b4c400c4d4
00d4e400e4f400f40500051500228500d2a400a4b400e28300457300c2f30043e30033d30023c30013b30003a300f29300157400358400532500550400b41400
c42400d43400e44400f45400056400d2450053c20035550045c200253500a45500c23500455500e2b400f2c40003d40013e40023f400330500431500536500f1
7500c17500157500658500857500b2250075b20065b200e35200e3850082e300658200e36500a59510b5a510c5b510d5c510e5d510f5e51006f5109506102616
10362610463610564610665610766610867610168610a69610b6a610c6b610d6c610e6d610f6e61007f610960710271710372710473710574710675710776710
8777101787109021019101d1c0f1a1d150c1a7a7a7a768a7a7a768a7686868a7a76868a768a768686868d738d8d7d7d838d7d83838d837d73837d7d73738d737
3838d7d737d7383738383738d737d83838d8d738d8d7d7d838d70805a88705b78805b7080908912040309171a18040701101218040909272a26040c012022230
405080b01080406030c090a040d0f001e070405031e01140406021f04130404041d031e04061718151304010b151c1404040c18191104020a161b14140e102d1
f15040a022e13220402042d1121040b032f142614052728262704070a252b26040a0c28292504080b262c291300313f2b1301323e2a1302303d29127d908d908
e8e8d90808d927080608080a0808080a0836e808d9e83608e80608082736082708d9270836080806d908273608270836270a0808e808d9e80836e83608c92809
0828064628092330100010200020400040300070300040800080700050700080600060500010500060200090a000b0a00090c000c0b00070b000c0800030a000
904000d0e000e0f000f0010001d000400100f0200010e000d0300011210031410011410031210020210031600041500010110051610071610051810081710060
8100518000617000507100a19100b1a10091b100c19100b1c100a1c1000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0b090f100c1f131e1f0c1d011f769c8f769e89729d846c9a846996876a948d6e94927398938080a074809e69809762808c6280746980697480628080608c80629780699e80749e808c9780978c809ea08080608080628c80699780749e8080a0808c9e809797809e8c8000002002010103020104030105040106050107060108
07010108010a09010b0a010c0b010e0d010f0e01100f011110011211011312011514011615010916011918011a19011b1a011c1b011d1c011e1d011f1e01180c010d1801171301141701171f0105152019160301188080a0698097608080698069808060978069a0808097809780a0a069a09760a08069a06980a06097a069a0
a08097a09780b09372b08e6db08072b07280b06d8eb07293b0808eb08e110804120f11070604140d13050404160b15030204180917010104171012080704110e14060504130c16040304150a18020c04221b210b0a04241923090904232025100f04271e260e0d04281c220c0b04211a240a1004251f270f0e04261d280d1208
1a1b1c1d1e1f2019119e808c8c806262807474809e8c809e9e807474806262808c6893767693988a939898937676936868938a98938a8a936880a080280201000302000403000504000605000706000807000108000a09000b0a000c0b000d0c000e0d000f0e00100f00091000070f00100800050d000e0600030b000c040001
09000a02001211001312001413001514001615001716001817001118000b1300140c00091100120a001018000e1600170f00150d000515201916040108a060a0a060606060606060a096809696806a6a806a6a80960601040104060205040a0c0b090104030905010204050b07040304070c080605040302080a0680608080a0
809f898080896161898080899f0c010200010400010500020300020600030400030700040800050600050800060700070800031f141001416672809a72809c80906480909a8e80668e806480709c80704d76804d808a4d8a804d8076b37680b3808ab38a80b38076b380c0b3c8a0b3c860b38040b33860b338a04d80c04dc8a0
4dc8604d80404d38604d38a08080608b8b649494709797809494908b8b9c908064a0808090809c8b7564946c70976980946c908b759c80706480647080608080649080709c7575646c6c706969806c6c9075759c70806460808070809c758b646c94706997806c9490758b9c809064809c7080a080809c9080909c310604060d
0b0e0704070e0c0f0104030f0a1004040110090d0504081514160804041712150204021811170304051613183f045d6727663d0365256840045e66286a3e045c68266741045f6a29692104282e5a4b1f04262f352b2204294b512c2004272b2d2e1e03256b2f03045a36324a08043537302a2504514a333424042d2a31362303
6b6c372904323d3a3c2704303f383e2a04333c3b402804313e393d26036c6d3f2c04384641452f043b4844472d04394542492b036d6e462e043a49434834044473724e3204424d4f5030036e6f5233044350717331044152584d37047274565736044f4c545935036f705b25083440474e5760692c0404715955740704585b53
4c3c0456615f603a0454635d6238037065643b0455625e61390453645c63317b97967b976a7b696a7b699685976a85696a8569968597968c9d788388618c9d8889966a8996969d8c8896896a9689969d8c788883619d748896776a9677969d7478887d618c6388896a6a896a968c6378837861776a6a776a967463787d786174
63886a7796637478787d616374886a776a6a8996638c787883618080a0638c886a896a779696749d787d8861749d8877966a740401000302000107000802000503000604000607000805000a0900090c000b0a000b0c00040a000b06000c07000901000e0d00100d000f0e00100f000810000f0500020d00030e001211011312
011413011514011615011116011817011918011a19011b1a011c1b01171c011d1e001e1f001f2000202100212200270800081f002225000824002024001e2300262700272800282900292a00252a002308002428002326002b2c002c2d002d2e002e2f002e2900282d002c2700262b002f2a0030310031320032330033340031
2c002b3000342f002e3300322d00290300032100073900310700373400073600363200032500353000383900393a003a3b003b3c003c37003507003a36002403003835003d3e003e3f003f4000404100413c003b40003f3a00393e003d38001d3d00203f003e1f001e3d004122002140001d23001d26001d2b001d30001d3500
1d3800360400043700330400043b000523221419120144737557738f5773758d738f8d8d75578d8f578d758d8d8f8d7a7ae57a87e5867ae58687e57d7dfd7d82fd837dfd8382fd638e5c638e80399d69399d80399db369825c638e5c698286638e8674885c6f935c7488866f93869b8d5c9b8d80c69969c69980c699b390935c
9b8d5c9093869b8d868a885c95825c8a888695828663745c6374803965693965803965b36f6f5c63745c6f6f86637486747a5c69805c747a866980869b755c9b7580c66969c66980c669b395805c9b755c9580869b75868a7a5c906e5c8a7a86906e8626010402030401080407111012070407090a0805040a0c020b03040508
0b01080406030c090c041019181a070405130e11040406120f14030404140d130d04151718160b040e1b16190a040f1a171c09040d1c151b1604232425221804252728261c04282a2b291a042b2d232c180426292c221d0427242d2a2304343536332504363839372904393b3c3a27043c3e343d2504373a3d332a0438353e3b
300445464744320447494a4836044a4c4d4b34044d4f454e3204484b4e44370449464f4c3d04565758553f04585a5b5943045b5d5e5c41045e60565f3f04595c5f5544045a57605d26608080a08082a0808080806080608080a080a0808480608280a0836080828080a0806084809f866080846372808080a09d8e808080608e
6380729d80909c808080a07064808080606490809c70807263808080a08e9d808080609d7280638e809c90808080a0647080808060709c80906480600301000102000204000403000703000408000807000507000806000605000105000602000a09000b09000a0c000c0b00070b000c08000309000a04000e0d000f0d000e10
00100f000b0f00100c00090d000e0a001112011314011214011311011415011816001617001719001918001c1800191d001d1c001a1c001d1b001b1a00161a001b17001e1f012021011f2101201e01212201252300232400242600262500292500262a002a29002729002a28002827002327002824002b2c012d2e012c2e012d
2b012e2f013230003031003133003332003632003337003736003436003735003534003034003531003839013a3b01393b013a38013b3c013f3d003d3e003e4000403f00433f004044004443004143004442004241003d4100423e00051f141f1710017da7807ba78078ab8078ac8079ab807aab807bab8078af8088af808baf
808aaf8089af8088ab8088ab808b95808ea0808ea58080a080809f80839b80839a808095808098808a93808a9380808e80808e808a88808a87808a8780898980878a80858a80848b80838a80818980818880807b80807b808484808485808584808582808881808a81808c81808d82808e84808e8d808ea68080a6808eaf808e
b2808db3808bb38088b28087b08086b18084b88084b88080af8080ab8085ab8080b3808bb38088bd807ab8807ab7807ab78079b98077bb8075bb8074bb8073bb8071ba8071b88070ac8070b48074b58075b58075b28078b1807ab1807cb2807db3807eb4807ebd807ea28070a2807eab807eae807db0807bb08079ae8077ac80
76ad8074a78075a7807099807b9780759b80759d807ea180709c80709b80739780739680708a807688807084807090807095807e84807e86807888807e8c807e8e80788f807e8c80709d808b9b80859f808591807096807e7f807e00007d0307010506010405010304010102010207010601010a09010d08010c0b010e0d0109
0e010b0a010c0801161701252601191a011a1b010f10011b1c012627013031011819012728012829011314011415012e2f011516012021011213012c2d011f2001292a011e1f012f30011011011112012d2e011718012a2b012b2c010f31011d1e011c1d01242501232401222301212201393a013e3f01323f013a3b013b3c01
3c3d013233013d3e013334013640013741013637013536013738013839013435014d6101586201616201464701424301454601505101425701595a015152014445015f60014c4d01565701474801434401585901604e015c5d015d5e015b5c015a5b015e5f014f50014e4f015556015455015354015253014b4c014a4b01494a
01484901646501686901797a01747501676801757601737401666701696a017172016a6b017273016e7d016d6e016c6d016f7001636401787901636501787a017d71016c77017c7b01667c017076016f77016b7b01061f201d1d101f0110869a8d869a967a9a8d7a9a969080909080707080707080908da08d8da07373a07373
a08d72958e7295728e958e8e95720909040c0e0d0b050411151203060412161006070410130f08090405140a0c0d0413090e0a0d041411040f100415050b070e0416070d090980a080a083808083606083808083a06083808083a0a08380808360160201010403010506000508000f0900060700100a000708000e0b000d0c00
090a00090c000a0b000b0c00080d00070e00050f000610000d0e000f0d00100f000e1000071f1d10190e130306089060a09060807060807060a09080a09080807080807080a003010402060401010401050903030406080a070380a080608080a080800a01020001040001050002030002060003040003070004080005060007
0800042110191f01188080a0698097608080698069808060978069a080809780978093967093906a938070937080936a90937096938090939080869075868b7086807586758086708b86759086808b868b100804120f11070604140d13050404160b15030204180917010104171012080704110e14060504130c16040304150a
18020c04221b210b0a04241923090904232025100f04271e260e0d04281c220c0b04211a240a1004251f270f0e04261d280d109b8d8b8b8d65658d75758d9b8b8d9b9b8d75758d65658d8b9b8e8b8b8e65758e65658e8b8b8e9b9b8e75658e75758e9b280201000302000403000504000605000706000807000108000a09000b
0a000c0b000d0c000e0d000f0e00100f00091000070f00100800050d000e0600030b000c04000109000a02001211001312001413001514001615001716001817001118000b1300140c00091100120a001018000e1600170f00150d000718110c170e1a1903786987a36287a35d8ca35d92a36297a36997a36d92a36d8ca3658f
9f92875b93885e9288618f89638b89618a885e8b885b8f8759878b6c868b69878a668a8a648e8a668f8a698d8b6c8a8b6eb37cb7b378afb378a1b87fb7bc7dafbc7d9db884b7bc86afbc869db387b7b38bafb38ba1af84b7ab86afab86a8af7fb7ab7dafab7da88986c77784e14884924884685d844a80843fa3844ab88468b8
84928984e1808c90778c8d718c85718c7b778c73808c70898c738f8c7b8f8c85898c8d77728d71728571727b7772738072708972738f727b8f728589728d807290777de1487d92487d685d7d4a807d3fa37d4ab87d68b87d92897de18979c77786c77779c7ab84a8b47d99b484997e87577c88597c885d7e895f82895f84885d
8488598287577e8b6b7c8b697c8a657e8a63828a63848a65848b69828b6b6e875b6c885e6e886171896374896176885e74885b718759768b6e738b6c728a69738a66768a647a8a667b8b697a8b6c341b04212723261c04222624251e04232c282b1f04242b292a210428312d30220429302e2f25042e35333424042d3632351d
06273e3b36312c2804333a38392704323b373a2b04383d223c2a04373e213d370a48494a4b4c4d4e4f5047480a585756555453525a51592d053f64486577360550667a65473407468a8678664f67330445674e68320444684d69310443694c6a3004426a4b6b2f04416b4a6c2e04406c496452078c8d896d586e7b4a0462777e
7c4b05637c705a714c045b7152724d045c7253734e045d7354744f045e74557550045f75567651046076576d400570796e595155047e7a7d792c04786f7b7d2e043f62637f2f04407f5b80300441805c81310442815d82320443825e83330444835f843404458460853504868e8c6f1c03888d3c56048a8b8d8e58044685898b
1f038788255704872a618b22042f8f9061250334918f2803929391348f668c9064809e808ca080808f9a8c909c80709c80719a8c8080a060808062808c70648071668c80a0808060807b9f8380a083859f83869f80859f7d82a07b7ea07b7b9f7d7a9f80866184a080807a61847861807a617c7e617982617986617c88618080
60848080a060808065809160808066806d7680628a80629a806da080809b80916a6d739b80919b80918865718f80648d976e789d8b678094b3020101030201040301050401060501070601080701010801010901050901090601030901090401090201080901090701110a010b0c010c0d010d0e010e0f010f10011011010a0b
011312011413011514011615011716011817011918011219011a1b001b1c001d1e001e1f001c1f001e1b001a1d002021002122001f2200211e001d2000232400242500222500242100202300262700272800282500242700262300292a002a2b002b2800272a002926001c2b002a1b001a29002e2d002f2e00302f0031300032
31003332003433005834003736003837003938003a39003b3a003c3b003d3c003e3d003f3e00363f004940004142004243004344004445004546004647004748004849004041004b4c004c4d004d4e004e4f004f50005051002258002d4a004a4b002e38005437002c3f00343e00333d00323c00313b00303a002f3900514700
5348003552005540004b41004c42004d43004e44004f45005046002d5400352c00535500542c005253004a55002c53005455002e4b002f4c00304d00314e00324f003350003451003556001f57001c57005157005658005857002b5200572b00562b003e25003e5800283e005628003e56005a59015b5a015c5b015d5c015e5d
015f5e01605f015960016261016362016463016564016665016766016867016168016a69016b6a016c6b016d6c016e6d016f6e01706f0169700172710173720174730175740176750177760178770171780109121019101d0c1f1a1d051c7a7a7a7a867a7a7a867a8686867a7a86867a867a868686867d838d7d7d8d837d8d83
838d737d83737d7d73837d7383837d7d737d8373838373837d738d83838d7d838d7d7d8d837d80508a78507b88507b8090801902040319171a08040711101208040929272a06040c212022030405080b01080406030c090a040d0f100e070405130e11040406120f14030404140d130e04161718150304011b151c0404041c18
190104021a161b14041e201d1f05040a221e23020402241d2101040b231f241604252728260704072a252b06040a2c28290504082b262c190330312f1b0331322e1a0332302d19729d809d808e8e9d80809d7280608080a0808080a080638e809d8e63808e60808072638072809d7280638080609d8072638072806372a08080
8e809d8e80638e63809c829080826064829032030100010200020400040300070300040800080700050700080600060500010500060200090a000b0a00090c000c0b00070b000c0800030a000904000d0e000e0f000f1000100d000410000f0200010e000d030011120013140011140013120002120013060014050001110015
16001716001518001817000618001508001607000517001a19001b1a00191b001c19001b1c001a1c00
__sfx__
00020000085500d650086500955009550086500955008550086500955008650085500865008550076500855008550076500855007650085500865008650075500765007650076500755008650096500855009550
000100002c060350503b0403e0403365029060216501e0501665015040116400d0400b6300a030076300803007630060300463004030036300263002640016400164001640016300163001630016300163001620
000100003c5503755034550315502f5502b5502565024550206501f5301b620185201161010550096500655004650036500265004550036500b50008500065000350001500015000000000000000000000000000
00020000026500a650146501765019650196501765015650126500e6500d6500b6500a65007650036400565004650026500165001650016500165001650026500265002650036500365003650036500265002650
0004000032450334602c4603235033460333602e3502e3502b450343402d4302c320333203a3203d330363302033025330224302f4302e33021350243402644026330284202632035430203301c320364201b320
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000
00 00000000

