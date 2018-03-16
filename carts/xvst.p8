pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
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
		f={0,1,2,3,4,0,8,7,6,5,0,4,8,5,1,0,7,3,2,6}
	},
 plane={
		v={
		{0,0,0},
		{0,5,0},
		{5,5,0},
		{5,0,0}},
		f={0,1,2,3,4}
	}
}

local cam
function cam_project(x,y,z)
 local d=z-cam[3]
 local w=cam[4]/d
 local px=64+(x-cam[1])*w
 local py=64-(y-cam[2])*w
 return px,py,d,w
end

function make_matrix4(x,y,z)
	return {
		1,0,0,0,
		0,1,0,0,
		0,0,1,0,
  x or 0,y or 0,z or 0,1}
end
function move_matrix4(m,dx,dy,dz)
	m[13]+=dx
	m[14]+=dy
	m[15]+=dz
end
function make_vec(x,y,z,w)
	return {x,y,z,w or 1}
end
local fwd,right=make_vec(0,0,1),make_vec(1,0,0)

function vec_clone(v)
	return {v[1],v[2],v[3]}
end
function v_move(v,dx,dy,dz)
	v[1]+=dx
	v[2]+=dy
	v[3]+=dz
end
function v_plus_v(v,dv)
	v[1]+=dv[1]
	v[2]+=dv[2]
	v[3]+=dv[3]
end
function m_x_v(m,v)
	local x,y,z=v[1],v[2],v[3];
	v[1]=e[1]*x+e[5]*y+e[9]*z+m[13]
	v[2]=e[2]*x+e[6]*y+e[10]*z+m[14]
	v[3]=e[3]*x+e[7]*y+e[11]*z+m[15]
end
function m_x_xyz(m,x,y,z)
	return {
		e[1]*x+e[5]*y+e[9]*z+m[13],
		e[2]*x+e[6]*y+e[10]*z+m[14],
		e[3]*x+e[7]*y+e[11]*z+m[15]}
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

		local x,y,z,w = q[1],q[2], q[3], q[4]
		local x2,y2,z2 = x + x, y + y, z + z
		local xx,xy,xz = x * x2, x * y2, x * z2
		local yy,yz,zz = y * y2, y * z2, z * z2
		local wx,wy,wz = w * x2, w * y2, w * z2

		te[ 1 ] = 1 - ( yy + zz )
		te[ 5 ] = xy - wz
		te[ 9 ] = xz + wy

		te[ 2 ] = xy + wz
		te[ 6 ] = 1 - ( xx + zz )
		te[ 10 ] = yz - wx

		te[ 3 ] = xz - wy
		te[ 7 ] = yz + wx
		te[ 11 ] = 1 - ( xx + yy )

		// last column
		te[ 4 ] = 0
		te[ 8 ] = 0
		te[ 12 ] = 0

		// bottom row
		te[ 13 ] = 0
		te[ 14 ] = 0
		te[ 15 ] = 0
		te[ 16 ] = 1

		return te
end
function m_clone(m)
	local c={}
	for i=1,16 do
		c[i]=m[i]
	end
	return c
end

function m_inv(m)
	for i=1,4 do
		for j=i,4 do
			m[i+j*4],m[j+i*4]=m[j+i*4],m[i+j*4]
		end
	end
end
function m_print(m)
	local s=""
	for i=1,16 do
		
		if (i-1)%4==0 then
			printh(s)
		end
	end
end

-- drawing helpers
function qline(v,i,j,k,l)
	
 	color(7)
 	line(v[i][1],v[i][2],v[j][1],v[j][2])
 	line(v[j][1],v[j][2],v[k][1],v[k][2])
 	line(v[k][1],v[k][2],v[l][1],v[l][2])
 	line(v[i][1],v[i][2],v[l][1],v[l][2])
	
end

function draw_actor(self)
	local p={}
	for i=1,#self.model.v do
		local v=self.model.v[i]
		v=v_x_q(v,self.q)
		local xe,ye,ze,we=cam_project(v[1]+self.pos[1],v[2]+self.pos[2],v[3]+self.pos[3])
		--local xe,ye,ze,we=cam_project(x,y,z)
		add(p,{
			xe,
			ye,
			ze,
			we})
	end
	local i=1
	while i<#self.model.f do
		local ftype=self.model.f[i]
		-- quad
		if ftype==0 then
			qline(p,
				self.model.f[i+1],
				self.model.f[i+2],
				self.model.f[i+3],
				self.model.f[i+4])
			i+=5
		end
	end
end
function make_plyr()
	local p={
		model=all_models.cube,
		pos=make_vec(0,0,0),
		q=make_quat(fwd,0),
		draw=draw_actor
	}
	add(actors,p)
	return p
end

function make_cam(f)
	return {
		pos=make_vec(0,0,-8,f),
		q=make_quat(fwd,0),
		update=function()
			self.m=m_inv(m_from_q(self.q))
		end,
		project=function(x,y,z)
			x-=self.pos[1]
			y-=self.pos[2]
			z-=self.pos[3]
			local v=m_x_xyz(self.m,x,y,z)
		end
	}
end

function _init()
	plyr=make_plyr()
	cam=make_cam(64)
end

function control_plyr()
	q_x_q(plyr.q,make_quat(fwd,0.01))
end

function _update60()
	control_plyr()

	cam[3]=-8.5
end

function _draw()
	cls()
	plyr:draw()
end

