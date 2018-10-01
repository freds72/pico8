pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- world axis
local v_fwd,v_right,v_up={0,0,1},{1,0,0},{0,1,0}
local cam

local points={}
function _init()
	cam=make_cam(64)
	
	--
	add(points,{-2,0,-2,uv={0,0}})
	add(points,{2,0,2,uv={1,1}})
	add(points,{2,0,-2,uv={1,0}})

	add(points,{2,0,2,uv={1,1}})
	add(points,{-2,0,-2,uv={0,0}})
	add(points,{-2,0,2,uv={0,1}})

end

local cam_angle,cam_dist=0.2,5
function _update()
	cam_angle+=0.01
	local q=make_q(v_up,cam_angle)
	local m=m_from_q(q)	
	cam:track(m_x_v(m,{0,2,-8}),q)
	cam:track(m_x_v(m,{0,2,-8}),q)
	cam:update()
end

function _draw()
	cls(0)

	local v={}	
	for i=1,#points do
		local p=points[i]
		v[i]=cam:project(p[1],p[2],p[3])
	end
	
	--[[
	trifill(
		v[1][1],v[1][2],v[1][3],v2_clone(points[1].uv),
		v[2][1],v[2][2],v[1][3],v2_clone(points[2].uv),
		v[3][1],v[3][2],v[1][3],v2_clone(points[3].uv),
		11)
	
	trifill(
		v[4][1],v[4][2],v[4][3],v2_clone(points[4].uv),
		v[5][1],v[5][2],v[5][3],v2_clone(points[5].uv),
		v[6][1],v[6][2],v[6][3],v2_clone(points[6].uv),
		11)
 ]]
  
 trifill2(
		{x=v[1][1],y=v[1][2],w=v[1][4],uv=points[1].uv},
		{x=v[2][1],y=v[2][2],w=v[2][4],uv=points[2].uv},
		{x=v[3][1],y=v[3][2],w=v[3][4],uv=points[3].uv})
 trifill2(
		{x=v[4][1],y=v[4][2],w=v[4][4],uv=points[4].uv},
		{x=v[5][1],y=v[5][2],w=v[5][4],uv=points[5].uv},
		{x=v[6][1],y=v[6][2],w=v[6][4],uv=points[6].uv})
	

 --[[	
	quadfill(
		{x=v[1][1],y=v[1][2],w=v[1][4],uv=points[1].uv},
		{x=v[6][1],y=v[6][2],w=v[6][4],uv=points[6].uv},
		{x=v[2][1],y=v[2][2],w=v[2][4],uv=points[2].uv},
 	{x=v[3][1],y=v[3][2],w=v[3][4],uv=points[3].uv})
 ]]
	print(stat(1),2,2,7)
end

-->8
-- camera
function make_cam(f)
	local c={
		pos={0,0,3},
		q=make_q(v_up,0),
		focal=f,
		update=function(self)
			self.m=m_transpose(m_from_q(self.q))
		end,
		track=function(self,pos,q)
			self.pos,q=v_clone(pos),q_clone(q)
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
			-- view to screen
 		local w=1/v[3]
 		return {64+self.focal*v[1]*w,64-self.focal*v[2]*w,v[3],w}
		end
	}
	return c
end
-->8
-- trifill
function lerp(a,b,t)
	return a*(1-t)+b*t
end
function v2_print(a)
	print(a[1].."/"..a[2])
end
function make_v2(a,b)
	return {
		b[1]-a[1],
		b[2]-a[2]}
end
function v2_clone(a)
	return {a[1],a[2]}
end
function v2_add(a,b,scale)
	scale=scale or 1
	a[1]+=scale*b[1]
	a[2]+=scale*b[2]
end
function v2_lerp(a,b,t)
	return {
		lerp(a[1],b[1],t),
		lerp(a[2],b[2],t)}
end
function v2_scale(v,scale)
	return {
		v[1]*scale,
		v[2]*scale}
end

function p01_trapeze_h(l,r,lt,rt,wl,wr,wlt,wrt,y0,y1,uvl,uvr,uvlt,uvrt)
 local dy=1/(y1-y0)
 lt,rt=(lt-l)*dy,(rt-r)*dy
 wlt,wrt=(wlt-wl)*dy,(wrt-wr)*dy
 uvlt,uvrt=v2_scale(make_v2(uvl,uvlt),dy),v2_scale(make_v2(uvr,uvrt),dy)

 -- cliping
 if y0<0 then
		v2_add(uvl,uvlt,-y0)
		v2_add(uvr,uvrt,-y0)
 	l,r,wl,wr,y0=l-y0*lt,r-y0*rt,wl-y0*wlt,wr-y0*wrt,0
 end
	y1=min(y1,127)

	-- rasterization
	for y0=y0,y1 do
  --rectfill(l,y0,r,y0)
  local len=ceil(r-l)
		local t,dx=0,1/len
  for i=l,r do
	  local w=lerp(wl,wr,t)
	  local uv=v2_lerp(v2_scale(uvl,wl),v2_scale(uvr,wr),t)
	  uv[1]/=w
	  uv[2]/=w
  	pset(i,y0,sget(8+8*uv[1],8*uv[2]))
  	t+=dx
  end 

  l+=lt
  r+=rt
  wl+=wlt
  wr+=wrt
  v2_add(uvl,uvlt)
  v2_add(uvr,uvrt)
 end
end

function trifill(x0,y0,w0,uv0,x1,y1,w1,uv1,x2,y2,w2,uv2,col)
 color(col)
 if(y1<y0)x0,x1,y0,y1,w0,w1,uv0,uv1=x1,x0,y1,y0,w1,w0,uv1,uv0
 if(y2<y0)x0,x2,y0,y2,w0,w2,uv0,uv2=x2,x0,y2,y0,w2,w0,uv2,uv0
 if(y2<y1)x1,x2,y1,y2,w1,w2,uv1,uv2=x2,x1,y2,y1,w2,w1,uv2,uv1
 -- mid point
 local mt=1/(y2-y0)*(y1-y0)
 col=x0+(x2-x0)*mt
 local w02=w0+(w2-w0)*mt
 local uv02=v2_clone(uv0)
 v2_add(uv02,make_v2(uv0,uv2),mt)
 if(x1>col)x1,col,w1,w02,uv1,uv02=col,x1,w02,w1,uv02,uv1
 p01_trapeze_h(x0,x0,x1,col,w0,w0,w1,w02,y0,y1,v2_clone(uv0),v2_clone(uv0),v2_clone(uv1),v2_clone(uv02))
 p01_trapeze_h(x1,col,x2,x2,w1,w02,w1,w1,y1,y2,uv1,uv02,uv2,uv2)
end
-->8
-- vector math
function v_print(v,x,y,c)
	print(v[1].."|"..v[2].."|"..v[3],x,y,c)
end
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
function v_lerp(a,b,t)
	return {
		lerp(a[1],b[1],t),
		lerp(a[2],b[2],t),
		lerp(a[3],b[3],t)}
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
-->8
-- matrix math
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
-- 
function m_x_xyz(m,x,y,z)
	return {
		m[1]*x+m[4]*y+m[7]*z,
		m[2]*x+m[5]*y+m[8]*z,
		m[3]*x+m[6]*y+m[9]*z}
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
-->8
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

		-- angular velocity "converted" to quaternion
		-- not: q[v,0]!!
		--[[
	local qdot=v_clone(v)
	qdot[4]=0
	q_x_q(qdot,q)
	q_scale(qdot,0.5)
	q[1]+=dt*qdot[1]
	q[2]+=dt*qdot[2]
	q[3]+=dt*qdot[3]
	q[4]+=dt*qdot[4]
	q_normz(q)
	]]
end

function q_scale(q,scale)
	return {scale*q[1],scale*q[2],scale*q[3],scale*q[4]}
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
-->8
-- barycentric trifill
local function orient2d(a,b,c)
 return (b.x-a.x)*(c.y-a.y)-(b.y-a.y)*(c.x-a.x)
end
local function istopleft(a,b)
	return a.x<b.x and a.y<b.y
end
function trifill2(a,b,c)
 -- compute triangle bounding box
 local minx,miny=min(a.x,min(b.x,c.x)),min(a.y,min(b.y,c.y))
 local maxx,maxy=max(a.x,max(b.x,c.x)),max(a.y,max(b.y,c.y))

 -- clip against screen bounds
 minx,miny=max(minx),max(miny)
 maxx,maxy=min(maxx,127),min(maxy,127)

 local a01,b01=a.y-b.y,b.x-a.x
 local a12,b12=b.y-c.y,c.x-b.x
 local a20,b20=c.y-a.y,a.x-c.x

 local bias0=istopleft(b,c) and 0 or -1
 local bias1=istopleft(c,a) and 0 or -1
 local bias2=istopleft(a,b) and 0 or -1
     
 local p={x=minx,y=miny}
 local w0_row=orient2d(b,c,p)+bias0
 local w1_row=orient2d(c,a,p)+bias1
 local w2_row=orient2d(a,b,p)+bias2

 -- rasterize
 for y=miny,maxy do
  local w0,w1,w2=w0_row,w1_row,w2_row
  local u0,v0=a.uv[1]*a.w,a.uv[2]*a.w
  local u1,v1=b.uv[1]*b.w,b.uv[2]*b.w
  local u2,v2=c.uv[1]*c.w,c.uv[2]*c.w
  local inout=false
 	for x=minx,maxx do 
   -- if p is on or inside all edges, render pixel.
   if bor(w0,bor(w1,w2))>=0 then
			 local z=1/(w0*a.w+w1*b.w+w2*c.w)
    local s,t=(w0*u0+w1*u1+w2*u2)*z,(w0*v0+w1*v1+w2*v2)*z
    
    -- persp correction
    pset(x,y,sget(8+8*s,8*t))
    --
    inout=true
 		elseif inout==true then
 			-- end of segment?
 			break
 		end
 		-- one step to the right
   w0+=a12
   w1+=a20
   w2+=a01
 	end
 	-- one row step
  w0_row+=b12
  w1_row+=b20
  w2_row+=b01
 end
end

function quadfill(a,b,c,d)
 -- compute triangle bounding box
 local minx,miny=min(a.x,min(b.x,min(c.x,d.x))),min(a.y,min(b.y,min(c.y,d.y)))
 local maxx,maxy=max(a.x,max(b.x,max(c.x,d.x))),max(a.y,max(b.y,max(c.y,d.y)))

 -- clip against screen bounds
 minx,miny=max(minx),max(miny)
 maxx,maxy=min(maxx,127),min(maxy,127)

 local a01,b01=a.y-b.y,b.x-a.x
 local a12,b12=b.y-c.y,c.x-b.x
 local a23,b23=c.y-d.y,d.x-c.x
 local a30,b30=d.y-a.y,a.x-d.x
   
 local p={x=minx,y=miny}
 local w0_row=(c.x-b.x)*(p.y-b.y)-(c.y-b.y)*(p.x-b.x)
	local w1_row=(d.x-c.x)*(p.y-c.y)-(d.y-c.y)*(p.x-c.x)
	local w2_row=(a.x-d.x)*(p.y-d.y)-(a.y-d.y)*(p.x-d.x)
	local w3_row=(b.x-a.x)*(p.y-a.y)-(b.y-a.y)*(p.x-a.x)
	
 -- rasterize
 for y=miny,maxy do
  local w0,w1,w2,w3=w0_row,w1_row,w2_row,w3_row
  local u0,v0=a.uv[1]*a.w,a.uv[2]*a.w
  local u1,v1=b.uv[1]*b.w,b.uv[2]*b.w
  local u2,v2=c.uv[1]*c.w,c.uv[2]*c.w
  local u3,v3=d.uv[1]*d.w,d.uv[2]*d.w
  local inout=false
 	for x=minx,maxx do 
   -- if p is on or inside all edges, render pixel.
   if bor(w0,bor(w1,bor(w2,w3)))>0 then
			 local z=1/(w0*a.w+w1*b.w+w2*c.w+w3*d.w)
    local s,t=(w0*u0+w1*u1+w2*u2+w3*u3)*z,(w0*v0+w1*v1+w2*v2+w3*v3)*z
    
    -- persp correction
    pset(x,y,11) --sget(8+8*(s+t),8*(t-s)))
    --
    inout=true
 	elseif inout==true then
 		-- end of segment?
 		break
 	end
 		-- one step to the right
   w0+=a12
   w1+=a23
   w2+=a30
   w3+=a01
 	end
 	-- one row step
  w0_row+=b12
  w1_row+=b23
  w2_row+=b30
  w3_row+=b01
 end
end
__gfx__
00000000777777770011223300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000700000070011223300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700700880074455667700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000700080074455667700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000700080078899aabb00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700700888078899aabb00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000070000007ccddeeff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000077777777ccddeeff00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
