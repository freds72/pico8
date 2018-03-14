pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
local cam_x,cam_y,cam_z
function cam_track(x,y,z)
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
	v[1]=ix*qw+iw*-qx+iy*-qz-iz*-qy
	v[2]=iy*qw+iw*-qy+iz*-qx-ix*-qz
	v[3]=iz*qw+iw*-qz+ix*-qy-iy*-qx
end
function make_m_from_q(q)

		local te={}

		local x = q[1], y = q[2], z = q[3], w = q[4]
		local x2 = x + x, y2 = y + y, z2 = z + z
		local xx = x * x2, xy = x * y2, xz = x * z2
		local yy = y * y2, yz = y * z2, zz = z * z2
		local wx = w * x2, wy = w * y2, wz = w * z2

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
function make_m_inv(m)
	-- todo
end

function make_plyr()
	local p={
		model=make_plane(),
		pos=make_vec(0,0,0),
		q=make_quat(fwd,0)
	}
	add(actors,p)
	return p
end

function _init()
	plyr=make_plyr()
end

function control_plyr()
	v_move(plyr.pos,0.05,0,-0.01)
	q_x_q(plyr.q,make_quat(fwd,0.01))
end

