pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- xwing vs. tie figther
-- by freds72

-- game globals
local time_t,time_dt=0,0
local good_side,bad_side,any_side,no_side=0x1,0x2,0x0,0x3
local before_update,after_draw={},{}

-- 0: chase
-- 1: cockpit
-- 2: orbit
local cam_mode=0
local actors,npc_count={},0
local parts={}
local scores,last_score={},0
local cur_screen
local start_screen={
	starting=false
}
local game_screen={
	starting=false
}
local gameover_screen={}

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

function filter(array,fn)
	for _,a in pairs(array) do
		if not a[fn](a) then
			del(array,a)
		end
	end
end
function forall(array,fn)
	for _,a in pairs(array) do
		a[fn](a)
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
	cube={
		v={
		{-1,2,-1},
		{1,2,-1},
		{1,-2,-1},
		{-1,-2,-1},
		{-1,2,1},
		{1,2,1},
		{1,-2,1},
		{-1,-2,1}},
		f={4,4,3,2,1,4,5,6,7,8,4,1,5,8,4,4,6,7,3,2},
		e={}
	},
 	plane={
		v={
		{0,0,0},
		{0,5,0},
		{5,5,0},
		{5,0,0}},
		f={0,1,2,3,4},
		e={}
	},
	xwing={
		c=7,
		r=1.6,
		v={{-0.4,-0.35,-1.28},{-0.4,0.47,-1.28},{-0.4,-0.35,0.4},{-0.4,0.47,0.4},{0.4,-0.35,-1.28},{0.4,0.47,-1.28},{0.4,-0.35,0.4},{0.4,0.47,0.4},{-0.2,-0.19,3.15},{-0.2,0.21,3.15},{0.2,-0.19,3.15},{0.2,0.21,3.15},{-0.11,-0.09,3.89},{-0.11,0.08,3.89},{0.11,-0.09,3.89},{0.11,0.08,3.89},{-0.89,0.43,-1.14},{-0.89,0.43,0.01},{-2.21,0.91,-0.73},{-2.21,0.91,0.01},{-2.21,0.91,1.61},{-0.73,0.07,-1.14},{-0.89,0.43,-1.14},{-0.73,0.07,0.2},{-0.89,0.43,0.2},{-0.36,0.24,-1.14},{-0.53,0.6,-1.14},{-0.36,0.24,0.2},{-0.53,0.6,0.2},{0.84,0.41,-1.14},{0.84,0.41,0.01},{2.2,0.77,-0.73},{2.2,0.77,0.01},{2.2,0.77,1.61},{0.5,0.61,-1.14},{0.84,0.41,-1.14},{0.5,0.61,0.2},{0.84,0.41,0.2},{0.3,0.26,-1.14},{0.64,0.06,-1.14},{0.3,0.26,0.2},{0.64,0.06,0.2},{-0.89,-0.37,-1.14},{-0.89,-0.37,0.01},{-2.21,-0.85,-0.73},{-2.21,-0.85,0.01},{-2.21,-0.85,1.61},{-0.53,-0.54,-1.14},{-0.89,-0.37,-1.14},{-0.53,-0.54,0.2},{-0.89,-0.37,0.2},{-0.36,-0.18,-1.14},{-0.73,-0.01,-1.14},{-0.36,-0.18,0.2},{-0.73,-0.01,0.2},{0.84,-0.35,-1.14},{0.84,-0.35,0.01},{2.2,-0.71,-0.73},{2.2,-0.71,0.01},{2.2,-0.71,1.61},{0.64,-0.0,-1.14},{0.84,-0.35,-1.14},{0.64,-0.0,0.2},{0.84,-0.35,0.2},{0.3,-0.2,-1.14},{0.5,-0.55,-1.14},{0.3,-0.2,0.2},{0.5,-0.55,0.2}},
		f={4,1,2,4,3,4,8,7,11,12,4,7,8,6,5,4,5,6,2,1,4,3,7,5,1,4,8,4,2,6,4,12,11,15,16,4,7,3,9,11,4,4,8,12,10,4,3,4,10,9,4,13,14,16,15,4,11,9,13,15,4,10,12,16,14,4,9,10,14,13,4,22,23,25,24,4,24,25,29,28,4,28,29,27,26,4,26,27,23,22,4,24,28,26,22,4,29,25,23,27,4,35,36,38,37,4,37,38,42,41,4,41,42,40,39,4,39,40,36,35,4,37,41,39,35,4,42,38,36,40,4,48,49,51,50,4,50,51,55,54,4,54,55,53,52,4,52,53,49,48,4,50,54,52,48,4,55,51,49,53,4,61,62,64,63,4,63,64,68,67,4,67,68,66,65,4,65,66,62,61,4,63,67,65,61,4,68,64,62,66},
		e={16,17,18,19,17,19,18,16,19,20,29,30,31,32,30,32,31,29,32,33,42,43,44,45,43,45,44,42,45,46,55,56,57,58,56,58,57,55,58,59},
		wp={
			dly=8,
			pos={{2,1,1.6},{2,-1,1.6},{-2,-1,1.6},{-2,1,1.6}},
			n={}
		}
	},
	tie={
		c=5,
		r=0.9,
		show=true,
		v={{0.71,0.0,0.71},{-0.0,0.71,0.71},{-0.0,0.0,1.0},{-0.71,0.0,0.71},{0.0,-0.71,0.71},{-0.81,-0.45,-0.0},{0.81,-0.45,-0.0},{0.55,-0.0,0.45},{-0.55,-0.0,0.45},{0.81,0.45,0.0},{-0.81,0.45,0.0},{-0.55,0.0,-0.45},{0.55,0.0,-0.45},{-1.58,-0.32,-0.0},{-1.58,0.0,0.32},{-1.58,0.32,0.0},{-1.58,0.0,-0.32},{1.58,-0.32,-0.0},{1.58,0.0,0.32},{1.58,0.32,0.0},{1.58,0.0,-0.32},{1.58,0.0,2.0},{1.58,2.25,1.0},{1.58,2.25,-1.0},{1.58,-0.0,-2.0},{1.58,-2.25,-1.0},{1.58,-2.25,1.0},{-1.58,0.0,2.0},{-1.58,2.25,1.0},{-1.58,2.25,-1.0},{-1.58,-0.0,-2.0},{-1.58,-2.25,-1.0},{-1.58,-2.25,1.0}},
		f={3,5,3,1,3,1,3,2,3,2,3,4,3,4,3,5,4,11,9,15,16,4,12,11,16,17,4,6,12,17,14,4,9,6,14,15,4,10,13,21,20,4,13,7,18,21,4,7,8,19,18,4,8,10,20,19},
		e={22,21,23,22,24,23,25,24,26,25,21,26,28,27,29,28,30,29,31,30,32,31,27,32},
		wp={
			dly=12,
			pos={{0.7,-0.7,0.7},{-0.7,-0.7,0.7}},
			n={{0,0,1},{0,0,1}}
		}
	},
	deathstar={
		c=3,
		v={{0.0,1.0,0.0},{-0.38,0.92,0.0},{-0.71,0.71,0.0},{-0.92,0.38,0.0},{-1.0,-0.0,0.0},{-0.92,-0.38,0.0},{-0.71,-0.71,0.0},{-0.38,-0.92,0.0},{-0.0,-1.0,-0.0},{0.38,-0.92,-0.0},{0.71,-0.71,-0.0},{0.92,-0.38,-0.0},{1.0,0.0,-0.0},{0.92,0.38,-0.0},{0.71,0.71,-0.0},{0.38,0.92,-0.0},{-0.4,0.76,0.0},{-0.58,0.68,0.0},{-0.66,0.5,0.0},{-0.58,0.32,0.0},{-0.4,0.24,0.0},{-0.22,0.32,0.0},{-0.14,0.5,0.0},{-0.22,0.68,0.0},{-0.3,0.1,0.0},{0.3,0.1,-0.0},{1.0,-0.0,-0.0}},
		f={},
		e={1,0,2,1,3,2,4,3,5,4,6,5,7,6,8,7,9,8,10,9,11,10,12,11,13,12,14,13,15,14,0,15,17,16,18,17,19,18,20,19,21,20,22,21,23,22,16,23,4,24,24,25,25,26}		
	}
}

function sqr_dist(a,b)
	local dx,dy,dz=b[1]-a[1],b[2]-a[2],b[3]-a[3]
	if abs(dx)>128 or abs(dy)>128 or abs(dz)>128 then
		return 32000
	end
	return dx*dx+dy*dy+dz*dz
end

function make_v(x,y,z,w)
	return {x,y,z,w or 1}
end
function make_rnd_v(scale)
	local v={rnd()-0.5,rnd()-0.5,rnd()-0.5}
	v_normz(v)
	return {scale*v[1],scale*v[2],scale*v[3]}
end
function make_v_cross(a,b)
	local ax,ay,az=a[1],a[2],a[3]
	local bx,by,bz=b[1],b[2],b[3]
	return {ay*bz-az*by,az*bx-ax*bz,ax*by-ay*bx}
end
local  v_fwd,v_right,v_up={0,0,1},{1,0,0},{0,1,0}

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
	v[1]=m[1]*x+m[5]*y+m[9]*z+m[13]
	v[2]=m[2]*x+m[6]*y+m[10]*z+m[14]
	v[3]=m[3]*x+m[7]*y+m[11]*z+m[15]
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
function make_m()
	local m={}
	for i=1,16 do
		m[i]=0
	end
	m[1],m[6],m[11],m[16]=1,1,1,1
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
function q_slerp(qa, qb, t) 
		if (t==0) return qa
		if (t==1) return qb

		local x,y,z,w=qa[1],qa[2],qa[3],qa[4]

		-- http://www.euclideanspace.com/maths/algebra/realnormedalgebra/quaternions/slerp/

		local coshalftheta = w * qb._w + x * qb._x + y * qb._y + z * qb._z

		if coshalftheta<0  then
			qa[1],qa[2],qa[3],qa[4]=-qa[4],-qa[1],-qa[2],-qa[3]
			coshalftheta = -coshalftheta
		else
			qa=q_clone(qb)
		end

		if coshalftheta>=1.0 then
			qa[1],qa[2],qa[3],qa[4]=w,x,y,z
			return
		end

		local sinhalftheta = sqrt( 1.0 - coshalftheta * coshalftheta )

		if abs( sinhalftheta ) < 0.001 then
			qa[4] = 0.5 * ( w + qa[4] )
			qa[1] = 0.5 * ( x + qa[1] )
			qa[2] = 0.5 * ( y + qa[2] )
			qa[3] = 0.5 * ( z + qa[3] )
			return
		end

		local halftheta = atan2( sinhalftheta, coshalftheta )
		local ratioa,ratiob = sin( ( 1 - t ) * halftheta ) / sinhalftheta,sin( t * halftheta ) / sinhalftheta

		qa[4] = ( w * ratioa + qa[4] * ratiob )
		qa[1] = ( x * ratioa + qa[1] * ratiob )
		qa[2] = ( y * ratioa + qa[2] * ratiob )
		qa[3] = ( z * ratioa + qa[3] * ratiob )
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

function printm(m)
	local s=""
	for i=1,16 do
		if (i-1)%4==0 then
			printh(s)
			s=""
		end
		s=s.." "..tostr(m[i])
	end
	printh(s)
end

-- drawing helpers
function getwinding(v1,v2,v3)
	local a={v2[1]-v1[1],v2[2]-v1[2]}
	local b={v3[1]-v1[1],v3[2]-v1[2]}
	--cross product
	return a[1]*b[2]-a[2]*b[1]
end

function qline(v,i,j,k,l)
	if getwinding(v[i],v[j],v[k])<0 then
		if(v[i][3]>0 and v[j][3]>0) line(v[i][1],v[i][2],v[j][1],v[j][2])
 	if(v[j][3]>0 and v[k][3]>0) line(v[j][1],v[j][2],v[k][1],v[k][2])
 	if(v[k][3]>0 and v[l][3]>0) line(v[k][1],v[k][2],v[l][1],v[l][2])
 	if(v[l][3]>0 and v[i][3]>0) line(v[i][1],v[i][2],v[l][1],v[l][2])
	end
end
function triline(v,i,j,k)
	if getwinding(v[i],v[j],v[k])<0 then
 	if(v[i][3]>0 and v[j][3]>0) line(v[i][1],v[i][2],v[j][1],v[j][2])
 	if(v[j][3]>0 and v[k][3]>0) line(v[j][1],v[j][2],v[k][1],v[k][2])
 	if(v[k][3]>0 and v[i][3]>0) line(v[k][1],v[k][2],v[i][1],v[i][2])
	end
end

local ground_colors={5,1}
function draw_ground(self)
	local v={}
	local scale=4
	local dx,dy=cam.pos[1]%scale,cam.pos[3]%scale
	
	local c=1
	for j=0,32,scale do
		for i=-16,16,scale do
			local ii,jj=i-dx+cam.pos[1],j-dy+cam.pos[3]
			local x,y,z=cam:project(ii,0,jj)
			if z>0 then
				pset(x,y,ground_colors[c%2+1])
			end
			c+=1
		end
	end
end

local hist,hist_i={},0
for i=1,64 do
	add(hist,0)
end
local debug_vectors=true
function draw_actor(self)
	draw_model(self.model,self.m)
	-- debug
	if debug_vectors then
 	if self.target then 
 		local c=12
 		if band(self.side,self.target.side)==0 then
	 		c=8
 		end
	 	draw_vector(self.m,self.pos,self.target.pos,c) 
 	end
 	if self.avoid then
 		local m=self.m
 		local pos=v_clone(self.avoid)
 		o_x_v(m,pos)
 		draw_vector(m,self.pos,pos,1,"a")
 	end
 end
end
function draw_vector(m,pos,v,c,s)	
	local x0,y0,z0,w=cam:project(pos[1],pos[2],pos[3])
	local x1,y1,z1,w=cam:project(pos[1]+v[1],pos[2]+v[2],pos[3]+v[3])
	line(x0,y0,x1,y1,c)
	if s then
		local dx,dy=x1-x0,y1-y0
		local d=sqrt(dx*dx+dy*dy)
		dx/=d
		dy/=d
		print(s,x1+4*dx,y1-4*dy,c)
	end
end

function draw_model(model,m)
	local xe,ye,ze,w=cam:project(m[13],m[14],m[15])
	if(ze<1) return 

	color(model.c)
	-- bounding radius
	if model.show then
		circ(xe,ye,model.r*w)
	end
	
	local p={}
	for i=1,#model.v do
		local v=model.v[i]
		v=m_x_xyz(m,v[1],v[2],v[3])
		xe,ye,ze,w=cam:project(v[1],v[2],v[3])
		add(p,{
			xe,
			ye,
			ze,
			w})
	end
	
	-- faces
	local i=1
	while i<#model.f do
		local ftype=model.f[i]
		-- quad
		if ftype==4 then
			qline(p,
				model.f[i+1],
				model.f[i+2],
				model.f[i+3],
				model.f[i+4])
		-- triangle
		elseif ftype==3 then
			triline(p,
				model.f[i+1],
				model.f[i+2],
				model.f[i+3])
		end
		i+=ftype+1
	end
	-- edges
	for i=1,#model.e,2 do
		local e0,e1=p[model.e[i]+1],p[model.e[i+1]+1]
		if(e0[3]>0 and e1[3]>0) line(e0[1],e0[2],e1[1],e1[2])
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
	for _,a in pairs(actors) do
		if band(a.side,self.side)==0 then
			local p=v_clone(a.pos)
			v_plus_v(p,self.pos,-1)
			-- within range?
			if v_dot(p,p)<16*16 then
				v_normz(p)
				-- within cone?
				if v_dot(fwd,p)>0.5 then
					return a
				end
			end
		end
	end
end

-- return a pos in self space
function wander(self)
	local p=make_rnd_v(5)
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
		-- enemy: get in sight
		local target_pos={0,-4,-10}
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
		if self.wander_t<time_t then
			-- pick a random location
			v_plus_v(force,follow(pos,self,wander(self)))
			self.wander_t=time_t+60
		end
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
		--m=m_from_q(self.q)
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
		v_normz(p)
		if v_dot(fwd,p)>0.95 then
		-- must be in sight for some time
			if self.lock_t>45 then
				self.lock_t=45
				self.fire_t=time_t+self.model.wp.dly
				self:fire()
			end
			self.lock_t+=1
		else
			-- target memory
			self.lock_t-=4
			self.lock_t=max(self.lock_t)
		end
	end
	return true
end

function make_plyr(x,y,z)
	local p={
		score=0,
		hp=8,
		acc=0,--0.1,
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
	model=all_models.tie,
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
		pos=p,
		q=make_q_from_v(v,v_up),
		pitch=0,
		roll=0,
		wander_t=0,
		lock_t=0,
		fire_t=0,
		laser_i=0,
		fire=make_laser,
		die=die_actor,
		hit=function(self,dmg)
			self.hp-=dmg
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
		q=make_q(fwd,0),
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
	--[[
	if self.pos[2]<0 then
		self.pos[2]=0
		make_flash(self.pos)
		return false
	end
	]]
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

function draw_line_part(self)
	local x0,y0,z0,w0=cam:project(self.pos[1],self.pos[2],self.pos[3])
	local x1,y1,z1,w1=cam:project(self.pos[1]+self.u[1],self.pos[2]+self.u[2],self.pos[3]+self.u[3])
	if z0>0 and z1>0 then
		line(x0,y0,x1,y1,time_t%2==0 and 7 or self.c)
	end
end

function draw_circ_part(self)
	local x0,y0,z0,w0=cam:project(self.pos[1],self.pos[2],self.pos[3])
	if z0>0 then
		circfill(x0,y0,self.r*w0,self.c)
	end
end

function draw_blast_part(self)
	local x0,y0,z0,w0=cam:project(self.pos[1],self.pos[2],self.pos[3])
	
	circfill(x0,y0,self.r*w0,7)
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
	m[13]=plyr.pos[1]
	m[14]=plyr.pos[2]
	m[15]=plyr.pos[3]
	v_plus_v(plyr.pos,{m[9],m[10],m[11]},plyr.acc)
	plyr.m=m
			
	-- cam modes
	if btnp(4) then
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
	local m=m_clone(plyr.m)
	m_inv(m)
	m[13],m[14],m[15]=0,0,0
	for _,a in pairs(actors) do
		if a!=plyr then
			local p=v_clone(a.pos)
			v_plus_v(p,plyr.pos,-1)
			m_x_v(m,p)
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
		draw_text("press start",32,110,5)
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

-- play loop
function game_screen:init()
	time_t=0
	parts={}
	actors={}
	npc_count=0
	plyr=make_plyr(0,0,0)
end

function make_rnd_pos_v(a)
	local p={0,0,0} --make_rnd_v(12)
	local v={1,0,0} --make_rnd_v(4)
	--v_plus_v(v,p,-1)
	--v_normz(v)
	m_x_v(a.m,p)
	return p,v
end

function game_screen:update()
	if plyr then
		control_plyr(plyr)
	end
	cam:update()

	if npc_count<=0 then
		local p,v,target=make_rnd_pos_v(plyr)
		-- friendly npc?
		if rnd()>0 then
			target=make_npc(p,v,npc_xwing)
			v_plus_v(p,v,8)
		end
		-- spawn new enemy
		for i=1,flr(1+rnd(2)) do
			local a=make_npc(p,v,npc_tie)
			a.target=target
			target=a
			v_plus_v(p,v,8)
		end
	end

	filter(actors,"update")
	filter(parts,"update")	
end
function game_screen:draw()
	draw_deathstar()
	draw_stars()

	forall(actors,"draw")
	forall(parts,"draw")
		
	if cam_mode==1 then
		palt(0,false)
		palt(14,true)
		spr(0,0,0,8,16)
		spr(0,64,0,8,16,true)
		-- radar
		draw_radar(64,112,12,10)
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
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
1eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
01eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
00001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
00000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
00000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
00000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
00000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee111111e0000000000000000000000000000000000000000000000000000000000000000
000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeee1e0000000000000000000000000000000000000000000000000000000000000000
0000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee0000000000000000000000000000000000000000000000000000000000000000
00000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee0000000000000000000000000000000000000000000000000000000000000000
000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee0000000000000000000000000000000000000000000000000000000000000000
0000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee11eeeeeee0000000000000000000000000000000000000000000000000000000000000000
10000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
e11000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eee1000000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeee11eeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeee1100000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeee110000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeee10000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeee1e0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeee11000000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeee1eeeee1e0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeee1100000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeee111111e0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeee110000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeee10000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeee11000000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeee1100000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeee110000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeee10000000000001eeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeee11000000000001eeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeee1100000000001eeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeee110000000001eeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeee10000000001eeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeee11000000001eeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000001eeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000001eeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee110000001eeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1100000111111111111111111110000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000001111111111111111111111110000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee100000010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1000000100111111111111111111111110000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeee10000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeee100000010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeee1000000100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeee10000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeee1100000010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeee110000000100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeee1000000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeeeee110000000010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeeee11000000000100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeeeee100000000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeeeee11000000000010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeeeee1100000000000100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeeee110000000000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeeeee1000000000000010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeeeee110000000000000100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeeeee11000000000000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eeee1100000000000000010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
eee10000000000000000100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
e1100000000000000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
10000000000000000010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000100100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000001001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000010010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
