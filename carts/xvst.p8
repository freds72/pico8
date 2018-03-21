pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
local time_t=0
local good_side,bad_side,any_side=0x1,0x2,0x0
local chase_cam=true
local actors,npc_count={},0
local parts={}
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
		r=1.6,
		v={{0.0,-0.71,0.71},{0.71,0.71,-0.0},{0.71,-0.71,-0.0},{-0.0,0.71,-0.71},{-0.0,-0.0,-1.0},{-0.0,-0.71,-0.71},{0.0,-1.0,-0.0},{-0.0,1.0,-0.0},{-0.71,0.71,0.0},{-0.71,-0.71,0.0},{0.0,0.71,0.71},{0.0,-0.0,1.0},{-0.81,-0.45,-0.0},{0.81,-0.45,-0.0},{0.55,-0.0,0.45},{-0.55,-0.0,0.45},{0.81,0.45,0.0},{-0.81,0.45,0.0},{-0.55,0.0,-0.45},{0.55,0.0,-0.45},{-1.58,-0.32,-0.0},{-1.58,0.0,0.32},{-1.58,0.32,0.0},{-1.58,0.0,-0.32},{1.58,-0.32,-0.0},{1.58,0.0,0.32},{1.58,0.32,0.0},{1.58,0.0,-0.32},{1.58,0.0,2.0},{1.58,2.25,1.0},{1.58,2.25,-1.0},{1.58,-0.0,-2.0},{1.58,-2.25,-1.0},{1.58,-2.25,1.0},{-1.58,0.0,2.0},{-1.58,2.25,1.0},{-1.58,2.25,-1.0},{-1.58,-0.0,-2.0},{-1.58,-2.25,-1.0},{-1.58,-2.25,1.0}},
		f={3,7,1,3,4,12,11,2,15,4,1,12,15,3,3,11,8,2,3,7,3,6,4,3,20,5,6,3,2,8,4,3,7,6,10,4,5,4,9,19,4,6,5,19,10,3,4,8,9,3,7,10,1,4,10,16,12,1,3,9,8,11,4,20,2,4,5,4,16,9,11,12,4,18,16,22,23,4,19,18,23,24,4,13,19,24,21,4,16,13,21,22,4,17,20,28,27,4,20,14,25,28,4,14,15,26,25,4,15,17,27,26,3,2,20,17,3,15,2,17,3,20,3,14,3,3,15,14,3,19,9,18,3,9,16,18,3,16,10,13,3,10,19,13},
		e={29,28,30,29,31,30,32,31,33,32,28,33,35,34,36,35,37,36,38,37,39,38,34,39}
	}
}

function sqr_dist(a,b)
	local dx,dy,dz=b[1]-a[1],b[2]-a[2],b[3]-a[3]
	if abs(dx)>128 or abs(dy)>128 or abs(dz)>128 then
		return 32000
	end
	return dx*dx+dy*dy+dz*dz
end

function make_vec(x,y,z,w)
	return {x,y,z,w or 1}
end
local fwd,right=make_vec(0,0,1),make_vec(1,0,0)

function v_clone(v)
	return {v[1],v[2],v[3]}
end
function v_normz(v)
	local d=v[1]*v[1]+v[2]*v[2]+v[3]*v[3]
	d=sqrt(d)
	v[1]/=d
	v[2]/=d
	v[3]/=d
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
-- quaternion
function make_quat(v,angle)
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
 	color(7)
 	line(v[i][1],v[i][2],v[j][1],v[j][2])
 	line(v[j][1],v[j][2],v[k][1],v[k][2])
 	line(v[k][1],v[k][2],v[l][1],v[l][2])
 	line(v[i][1],v[i][2],v[l][1],v[l][2])
	end
end
function triline(v,i,j,k)
	if getwinding(v[i],v[j],v[k])<0 then
 	color(7)
 	line(v[i][1],v[i][2],v[j][1],v[j][2])
 	line(v[j][1],v[j][2],v[k][1],v[k][2])
 	line(v[k][1],v[k][2],v[i][1],v[i][2])
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
function draw_actor(self)
	local p={}
	for i=1,#self.model.v do
		local v=self.model.v[i]
		v=m_x_xyz(self.m,v[1],v[2],v[3])
		local xe,ye,ze,we=cam:project(v[1],v[2],v[3])
		add(p,{
			xe,
			ye,
			ze,
			we})
	end
	-- faces
	local i=1
	while i<#self.model.f do
		local ftype=self.model.f[i]
		-- quad
		if ftype==4 then
			qline(p,
				self.model.f[i+1],
				self.model.f[i+2],
				self.model.f[i+3],
				self.model.f[i+4])
		-- triangle
		elseif ftype==3 then
			triline(p,
				self.model.f[i+1],
				self.model.f[i+2],
				self.model.f[i+3])
		end
		i+=ftype+1
	end
	-- edges
	color(7)
	for i=1,#self.model.e,2 do
		line(
			p[self.model.e[i]+1][1],
			p[self.model.e[i]+1][2],
			p[self.model.e[i+1]+1][1],
			p[self.model.e[i+1]+1][2])
	end
	
	if self.target then
		rect(2,9,2+16,9+16,8)
	 line(2+8,9+8,2+8+8*self.target[1],9+8-8*self.target[2],8)
	
		--print("roll:"..self.roll,20,9,7)
		--print("pitch:"..self.pitch,20,18,7)
		line(19,20,127,20,1)
		for i=0,63 do
			pset(19+i,20-8*hist[(i+time_t)%64+1],8)
		end
	end
end

function die_actor(self)
	make_blast(self.pos)
	
	npc_count-=1
	del(actors,self)
end

-- offset: position relative to other pos
function follow(self,other,offset)
	-- offset into world position
	local v=v_clone(offset)
	m_x_v(other.m,v)
	-- line to target
	v_plus_v(v,self.pos,-1)
	return v
end

function make_plyr(x,y,z)
	local p={
		hp=8,
		acc=0.1,
		model=all_models.xwing,
		pos=make_vec(x,y,z),
		q=make_quat({0,0,1},0),
		laser_i=0,
		fire_t=0,
		side=good_side,
		fire=make_laser,
		draw=function(self)
			if chase_cam==false then
				return
			end
			draw_actor(self)
		end,
		update=function(self)
		end
	}
	add(actors,p)
	return p
end

local kp=1
local ki=0
local kd=0

function make_pid()
	return {
		error=0,
		errorlast=0,
		errointe=0,
		update=function(self,input)
   self.error=self.error*0.7+input*0.3
   local errordiff=self.error-self.errorlast
   self.errointe=mid(self.errointe+self.error,-1,1)
   local output=kp*self.error+ki*self.errointe+kd*errordiff
   self.errorlast=self.error
   return output
  end
	}
end

function make_npc(x,y,z)
	npc_count+=1
	local p={
		hp=4,
		acc=0.1,
		side=bad_side,
		model=all_models.tie,
		pos=make_vec(x,y,z),
		q=make_quat({0,0,1},0),
		pid_roll=make_pid(),
		pid_pitch=make_pid(),
		pitch=0,
		roll=0,
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
		draw=draw_actor,
		update=function(self)
			local target=follow(self,plyr,{3,3,10})
			-- convert target to self space
			local m=m_from_q(self.q)
			m_inv(m)
			m_x_v(m,target)
			self.target=target
			local angle=0
			if abs(target[1])>0.1 then
 			angle=mid(target[1],-1,1) -- (self.pid_roll):update(self.target[1])
				if abs(angle)>0.5 then
	 			self.roll=angle
	 			local q=make_quat({0,0,1},angle/128)
 				q_x_q(self.q,q)
 			end
			elseif abs(target[2])>0.1 then
				angle=mid(target[2],-1,1)-- (self.pid_pitch):update(self.target[2])
				if abs(angle)>0.5 then
					self.pitch=angle
					local q=make_quat({1,0,0},angle/128)
					q_x_q(self.q,q)
				end
			end

			hist[time_t%64+1]=angle

			local m=m_from_q(self.q)
			m[13]=self.pos[1]
			m[14]=self.pos[2]
			m[15]=self.pos[3]
			v_plus_v(self.pos,{m[9],m[10],m[11]},self.acc)
			self.m=m
		end
	}
	add(actors,p)
	return p
end

function make_cam(f)
	return {
		pos=make_vec(0,0,3,f),
		q=make_quat(fwd,0),
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
 			local w=self.pos[4]/v[3]
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
	make_flash(p)
	self.laser_i+=1
	add(parts,{
		t=time_t+90,
		acc=0.5,
		pos=p,
		u=v,
		side=self.side,
		dmg=1,
		update=update_blt,
		draw=draw_line_part})
end
function make_flash(p)
	return add(parts,{
		t=time_t+8,
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
	if self.pos[2]<0 then
		self.pos[2]=0
		make_flash(self.pos)
		return false
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

function draw_line_part(self)
	local x0,y0,z0,w0=cam:project(self.pos[1],self.pos[2],self.pos[3])
	local x1,y1,z1,w1=cam:project(self.pos[1]+self.u[1],self.pos[2]+self.u[2],self.pos[3]+self.u[3])
	line(x0,y0,x1,y1,time_t%2==0 and 7 or 11)
end

function draw_circ_part(self)
	local x0,y0,z0,w0=cam:project(self.pos[1],self.pos[2],self.pos[3])
	circfill(x0,y0,self.r*w0,11)
end

function draw_blast_part(self)
	local x0,y0,z0,w0=cam:project(self.pos[1],self.pos[2],self.pos[3])
	
	circfill(x0,y0,self.r*w0,7)
end

local turn_t=0
function control_plyr(self)
	local pitch,roll=0,0
	if(btn(0)) roll=-1 turn_t+=1
	if(btn(1)) roll=1 turn_t+=1
	if(btn(2)) pitch=-1
	if(btn(3)) pitch=1

	turn_t=min(turn_t,8)
	if roll!=0 then
		local r=turn_t/8
		local q=make_quat({0,1,0},(1-r)*roll/128)
		q_x_q(plyr.q,q)
		local q=make_quat({0,0,1},-r*roll/128)
		q_x_q(plyr.q,q)
	else
		turn_t=0
	end
	
	if pitch!=0 then
		local q=make_quat({1,0,0},-pitch/128)
		q_x_q(plyr.q,q)
	end

	-- update pos
	local m=m_from_q(plyr.q)
	m[13]=plyr.pos[1]
	m[14]=plyr.pos[2]
	m[15]=plyr.pos[3]
	v_plus_v(plyr.pos,{m[9],m[10],m[11]},plyr.acc)
	plyr.m=m
			
	-- chase cam
	if btnp(4) then
		chase_cam=not chase_cam
	end
	
	if chase_cam then
		local m=m_from_q(plyr.q)
		cam.pos=m_x_xyz(m,0,2,-8)
		cam.pos[4]=64
		v_plus_v(cam.pos,plyr.pos)
		cam.q=q_clone(plyr.q)
	else
		cam.pos=v_clone(plyr.pos)
		cam.pos[4]=64
		cam.q=q_clone(plyr.q)
	end
	
	if btnp(5) then
		plyr:fire()
	end
end

local stars={}
function draw_stars()
 for i=1,#stars do
		local v=stars[i]
		for k=1,3 do
			v[k]+=cam.pos[k]
		end
		local x,y,z,w=cam:project(v[1],v[2],v[3])
		if z>0 then
			pset(x,y,7)
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

function _update60()
	time_t+=1
	control_plyr()
	
	for _,a in pairs(actors) do
		a:update()
	end
	
	for _,a in pairs(parts) do
		if a:update()==false then
			del(parts,a)
		end
	end

	cam:update()
end

function _draw()
	cls()

	--draw_ground()
	draw_stars()
	
	for _,a in pairs(actors) do
		a:draw()
	end
	for _,a in pairs(parts) do
		a:draw()
	end
	
	if chase_cam==false then
		palt(0,false)
		palt(14,true)
		spr(0,0,0,8,16)
		spr(0,64,0,8,16,true)
		-- radar
		draw_radar(64,112,12,10)
	end
	
	rectfill(0,0,127,8,1)
	print(stat(1),2,2,7)
end

function _init()
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
		local v={rnd(),rnd(),rnd()}
		v_normz(v)
		v[1]*=32
		v[2]*=32
		v[3]*=32
		add(stars,v)
	end
	
	plyr=make_plyr(0,0,0)
	make_npc(-4,0,8)
	make_npc(4,0,8)
	
	cam=make_cam(64)
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
