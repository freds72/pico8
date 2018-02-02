pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
local cam={0,0,0,64}
local time_t=0
local actors={}
local plyr

function make_plane()
    local plane={}
    --verts
    plane.v={{-1.333,-1.498,1.54},{-1.333,0.512,1.54},{-1.333,-1.309,-1.8},{-1.333,0.512,-2.785},{1.354,-1.498,1.54},{1.354,0.512,1.54},{1.354,-1.309,-1.8},{1.354,0.512,-2.785},{0.011,1.158,-3.028},{0.011,1.415,1.125},{0.011,-0.604,8.137},{-0.9470000000000001,0.55,6.356},{-7.555,3.076,-3.354},{0.968,0.55,6.356},{-7.97,3.122,0.494},{7.5760000000000005,3.076,-3.354},{7.9910000000000005,3.122,0.494},{-1.125,0.468,-4.926},{-1.125,-0.989,-4.926},{0.011,-1.551,-4.926},{0.011,1.029,-4.926},{0.011,0.603,9.744},{1.1460000000000001,0.468,-4.926},{1.1460000000000001,-0.989,-4.926},{0.011,1.408,6.215},{-1.333,0.512,0.259},{0.011,3.975,10.229000000000001},{1.354,0.512,0.259},{-6.049,-1.248,-2.232},{-5.8580000000000005,3.015,-3.21},{-6.049,-1.248,0.743},{-5.8580000000000005,3.015,-0.23500000000000001},{6.043,-1.228,-2.232},{5.548,3.029,-2.821},{6.043,-1.228,0.749},{5.548,3.029,0.161},{-1.29,0.679,-1.852},{-2.2880000000000003,2.7560000000000002,-2.44},{1.212,0.75,-1.852},{2.437,2.721,-2.44},{0.011,2.725,0.974},{0.011,2.725,-3.354},{0.0,-2.053,-5.625},{0.904,-1.811,-5.625},{1.566,-1.149,-5.625},{1.808,-0.245,-5.625},{1.566,0.659,-5.625},{0.904,1.321,-5.625},{0.0,1.563,-5.625},{-0.905,1.321,-5.625},{-1.567,0.659,-5.625},{-1.809,-0.245,-5.625},{-1.567,-1.149,-5.625},{-0.905,-1.811,-5.625},{-0.044,-0.252,-5.585},{-0.044,1.109,0.157},{-8.385,3.1670000000000003,-2.394},{8.407,3.1670000000000003,-2.394},{0.011,2.955,-1.19},{-7.555,-1.165,-2.702},{-7.97,-1.119,1.1460000000000001},{7.5760000000000005,-1.165,-2.702},{7.9910000000000005,-1.119,1.1460000000000001},{0.011,-1.516,1.6260000000000001},{0.011,-1.516,-2.702},{-8.385,-1.074,-1.742},{8.407,-1.074,-1.742},{0.012,-1.424,-1.739},{-1.031,-1.2650000000000001,-2.811},{-1.031,-1.2650000000000001,-2.496},{1.052,-1.2650000000000001,-2.811},{1.052,-1.2650000000000001,-2.496},{2.1390000000000002,-3.4530000000000003,-2.439},{-2.119,-3.4530000000000003,-2.439},{0.011,3.152,6.966},{0.011,3.757,7.615},{0.011,0.603,9.744},{-0.468,0.586,8.252},{0.489,0.586,8.252},{-0.934,0.55,6.356},{-0.308,0.603,8.257},{-2.963,0.55,6.9510000000000005},{-3.359,0.603,8.257},{0.933,0.55,6.356},{0.307,0.603,8.257},{2.962,0.55,6.9510000000000005},{3.358,0.603,8.257},{-0.934,0.55,6.356},{-0.308,0.603,8.257},{0.933,0.55,6.356},{0.307,0.603,8.257},{0.011,-0.261,-5.3740000000000006},{0.011,1.338,-2.858},{0.011,1.21,-5.414},{-2.078,-3.068,-1.994},{-2.173,-3.681,-1.946},{-2.227,-3.952,-2.411},{-2.059,-2.866,-2.495},{-2.114,-3.137,-2.96},{-2.209,-3.75,-2.9130000000000003},{2.121,-3.0660000000000003,-1.994},{2.242,-3.674,-1.946},{2.283,-3.948,-2.411},{2.07,-2.87,-2.495},{2.111,-3.144,-2.96},{2.231,-3.7520000000000002,-2.9130000000000003},{2.125,-3.3770000000000002,-2.702},{-2.141,-3.3770000000000002,-2.702},{2.125,-3.3770000000000002,-1.289},{-2.141,-3.3770000000000002,-1.289},{2.125,-3.3770000000000002,-2.702},{-2.141,-3.3770000000000002,-2.702},{2.125,-3.3770000000000002,-1.289},{-2.141,-3.3770000000000002,-1.289}}
    --triangles
    plane.t={{26,4,3,2},{79,22,11,2},{7,1,3,2},{10,12,25,8},{10,14,6,8},{2,11,12,2},{6,11,5,2},{59,41,17,8},{41,13,42,2},{13,57,59,8},{4,19,3,8},{4,21,18,8},{7,3,20,2},{22,25,75,8},{3,19,20,2},{24,8,7,8},{8,21,9,8},{16,59,58,8},{5,11,1,2},{10,6,28,2},{28,26,10,1},{9,26,28,1},{79,25,22,8},{78,25,12,8},{17,42,16,2},{16,42,59,8},{3,1,26,2},{1,2,26,2},{6,5,28,2},{7,8,28,2},{28,5,7,2},{7,5,1,2},{10,2,12,8},{10,25,14,8},{2,1,11,2},{6,14,11,2},{15,57,13,2},{41,15,13,2},{4,18,19,8},{4,9,21,8},{78,11,22,2},{7,20,24,2},{4,26,9,2},{10,26,2,2},{24,23,8,8},{8,23,21,8},{9,28,8,2},{17,41,42,2},{13,59,42,8},{59,15,41,8},{59,17,58,8},{15,59,57,8},{68,64,63,2},{64,60,65,2},{60,66,68,8},{62,68,67,8},{63,65,62,2},{62,65,68,8},{66,60,61,2},{64,61,60,2},{63,64,65,2},{60,68,65,8},{68,61,64,2},{68,63,67,2},{61,68,66,2},{75,76,22,8},{76,27,22,8},{77,75,25,2},{75,77,76,2},{76,77,27,2},{62,67,63,2},{58,17,16,2},{11,78,12,2},{79,11,14,2},{80,83,81,8},{84,87,86,8},{88,83,82,2},{90,87,91,2},{79,14,25,8},{78,22,25,8},{80,82,83,8},{84,85,87,8},{88,89,83,2},{90,86,87,2},{24,20,92,13},{20,19,92,13},{19,18,92,5},{18,21,92,6},{21,23,92,6},{23,24,92,5},{107,110,109,8},{107,108,110,8},{111,113,114,2},{111,114,112,2}}
    --edges 
    plane.e={{45,44,7},{47,46,7},{49,48,7},{51,50,7},{53,52,7},{43,54,7},{44,43,7},{46,45,7},{48,47,7},{50,49,7},{52,51,7},{54,53,7},{73,71,8},{74,69,8},{73,72,2},{74,70,2},{31,32,2},{35,36,2},{37,38,2},{39,40,2},{29,32,2},{33,36,2},{29,30,8},{33,34,8},{99,100,0},{95,96,0},{97,96,0},{95,98,0},{99,98,0},{97,100,0},{105,106,0},{101,102,0},{103,102,0},{101,104,0},{105,104,0},{103,106,0}}
    --circles
    plane.s={{73,6,1},{74,6,1},{55,5,3},{56,4,2}}
    return plane
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

function _update60()
	time_t+=1

 control_plyr()
 --draw background
 cls(15)
 rectfill(0,64,128,128,13)
 rectfill(0,64-1,128,64-1,14)
 cam[3]=-16.5
 
 -- todo: project bounding sphere
 -- check viz against screen
 
 --vertex list
 --transformed verts
 local nv={}
 --add all verts
 for _,a in pairs(actors) do
  for _,v in pairs(a.model.v) do
   v=vec_clone(v)
   v_x_q(v,a.q)
   v_plus_v(v,a.pos)
   add(nv,v)
  end
 end
 
 --projected verts
 local pv={}
 
 -- view project
 for p in all(nv) do
  local dp=project(p[1],p[2],p[3])
  add(pv,dp)
 end

 -- todo: rebase triangle indices
 -- render component list
 local cmp,o={}
 for _,a in pairs(actors) do
  o=a.model
  --add tris to render list
  for nt in all(o.t) do
   local ov1=nt[1]
   local ov2=nt[2]
   local ov3=nt[3]
   local od=(nv[ov1][3]+nv[ov2][3]+nv[ov3][3])/3-cam[3]
		
   if od>1 then
    --test winding before sort
    local nn=getwinding(pv[ov1],pv[ov2],pv[ov3])
    if nn>0 then
     add(cmp,{t=1,d=od,v1=ov1,v2=ov2,v3=ov3,c=nt[4]})
    end
   end
 	end
  --add edges to render list
  for ne in all(o.e) do
   local ov1=ne[1]
   local ov2=ne[2]
   local od=(nv[ov1][3]+nv[ov2][3])/2-cam[3]
   
   if(od>1)then
    add(cmp,{t=2,d=od,v1=ov1,v2=ov2,c=ne[3]})
   end
  end
 
  --add circles to render list
  for ns in all(o.s) do
   local ov1=ns[1]
   local od=(nv[ov1][3]-cam[3])
   if(od>1)then
  	 add(cmp,{t=3,d=od,v1=ov1,r=ns[3],c=ns[2]})
   end
  end
 end
 
 --sort all render components
 quicksort(cmp)
 
 --draw all render components
 for p in all(cmp) do
  if p.t==1 then
   trifill(pv[p.v1].x,pv[p.v1].y,pv[p.v2].x,pv[p.v2].y,pv[p.v3].x,pv[p.v3].y,p.c)
  elseif p.t==2 then
   line(pv[p.v1].x,pv[p.v1].y,pv[p.v2].x,pv[p.v2].y,p.c)
  elseif p.t==3 then
   circfill(pv[p.v1].x,pv[p.v1].y,p.r,p.c)
  end
 end
end

function project(x,y,z)
 local d=z-cam[3]
 local px=64+(x-cam[1])*cam[4]/d
 local py=64-(y-cam[2])*cam[4]/d
 return {x=px,y=py}
end

function getwinding(v1,v2,v3)
	local a={v2.x-v1.x,v2.y-v1.y}
	local b={v3.x-v1.x,v3.y-v1.y}
	--cross product
	return a[1]*b[2]-a[2]*b[1]
end

--fast trifill and quicksort by electricgryphon 

function quicksort(t,start,endi)
  start,endi=start or 1,endi or #t
  --partition w.r.t. first element
  if(endi-start < 1) then return t end
  local pivot=start
  for i=start+1,endi do
    if t[i].d >= t[pivot].d then
      if i == pivot+1 then
        t[pivot],t[pivot+1]=t[pivot+1],t[pivot]
      else
        t[pivot],t[pivot+1],t[i]=t[i],t[pivot],t[pivot+1]
      end
      pivot=pivot+1
    end
  end
   t=quicksort(t,start,pivot-1)
  return quicksort(t,pivot+1,endi)
end

function trifill(x1,y1,x2,y2,x3,y3,color1)

          local min_x=min(x1,min(x2,x3))
         if(min_x>127)return
          local max_x=max(x1,max(x2,x3))
         if(max_x<0)return
          local min_y=min(y1,min(y2,y3))
         if(min_y>127)return
          local max_y=max(y1,max(y2,y3))
         if(max_y<0)return

          local x1=band(x1,0xffff)
          local x2=band(x2,0xffff)
          local y1=band(y1,0xffff)
          local y2=band(y2,0xffff)
          local x3=band(x3,0xffff)
          local y3=band(y3,0xffff)

     --wide triangle  
          local nsx,nex
          --sort y1,y2,y3
          if(y1>y2)then
            y1,y2=y2,y1
            x1,x2=x2,x1
          end

          if(y1>y3)then
            y1,y3=y3,y1
            x1,x3=x3,x1
          end

          if(y2>y3)then
            y2,y3=y3,y2
            x2,x3=x3,x2          
          end

         if(y1!=y2)then  
            local delta_sx=(x3-x1)/(y3-y1)
            local delta_ex=(x2-x1)/(y2-y1)

            if(y1>0)then
                nsx=x1
                nex=x1
                min_y=y1
            else --top edge clip
                nsx=x1-delta_sx*y1
                nex=x1-delta_ex*y1
                min_y=0
            end

            max_y=min(y2,128)

            for y=min_y,max_y-1 do

            rectfill(nsx,y,nex,y,color1)
            nsx+=delta_sx
            nex+=delta_ex
            end

        else --where top edge is horizontal
            nsx=x1
            nex=x2
        end

        if(y3!=y2)then
            local delta_sx=(x3-x1)/(y3-y1)
            local delta_ex=(x3-x2)/(y3-y2)

            min_y=y2
            max_y=min(y3,128)
            if(y2<0)then
                nex=x2-delta_ex*y2
                nsx=x1-delta_sx*y1
                min_y=0
            end

             for y=min_y,max_y do
                rectfill(nsx,y,nex,y,color1)
                nex+=delta_ex
                nsx+=delta_sx
             end

        else --where bottom edge is horizontal
            rectfill(nsx,y3,nex,y3,color1)
        end
    
    
end

__label__
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff888ffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8888fffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff88888ffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff888888fffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8888888ffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff88888888fffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff88888888ffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff888888888ffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff888888888f8fffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff888888888ff8fffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff888888888ffff8ffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff888888888fffff8ffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff888888888ffffff8ffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8888888882fffffff8fffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff888888888ff2ffffff8fffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8888888888fff2ffffff8ffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8888888888f2fff22ffff8fff8ffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8888888888ff2fffff2fff8228888ffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff88888888882fff2fffff222288888fffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8888888888f2fff2fff2222228888ffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffffff8888888888ff2ffff2f22222228888ffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffffff8888888888fff2ffff222222228888fffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffffffffffffffff88888888888fff2ffff22222228888ffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffffffffffffff88888888888ffff2fff222222228888ffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffff2fffffffffff88888888888fffff2fff22222228877777ffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffff22222ffffffffff8888888888ffffff2ff22222228778ffff7fffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffff22222fffffff88888888888fffffff2f222222227888ffff7fffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffff22222fffff88888888888ffffffff2222888888786ffffff7ffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffff22222fff888888888884222222222288888887666ffffff7ffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffff22222f88888888888444111111188888888676665fffff7ffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffff2222888888888884444111112888888886766665ffffff7fffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffff2288888888888444411112228888888867666655fffff7fffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffff28888888888821441112222888888888676666555ffff7fffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffffffff888888888882222111222228888888866766655555fff7fffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffffff88888888888222112222222888888888667666655555ff7fffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffffff8888888888888222122222222888888888667666655555ff7fffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffffff88888888888822222222222222888888886657555555555ff7fffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffffff888888888888f2222222222222288888888855755555555fff7fffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffffff888888888888fff2222222222222888888888857555d555ffff7fffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffffff888888888888ffffff22222222222888888888857555ddd0ffff7fffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffffff888888888888fffffffff22222222288888888888575ddddffff7ffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffffff888888888888fffffffffff2222222288888888888875dfffffff7ffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffffffff888888888888ffffffffffff2222222288888888888875ffffffff7ffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffffff888888888888ffffffffffff222222222888888ff88887ffffffff7fffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffffff8888888888888ffffffffffff2222222222888888f88888877fffff7ffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffffff8888888888888fffffffffffff22222222288888f8f88888fff77777fffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffffff8888888888888fffffffffffff22222222288888ff888888ffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffffff8888888888888fffffffffffff22222222288888fff288888ffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffffff88888888888888ffffffffffff222222222888888fff28888fffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffffff88888888888882f8ffffffffff222222222288888fff82888ffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffffff8888888888888ff2f8ffffffff222222222288888fff880888ffffffffffffffffffffffffffffffffffffffffffffffffffff
fffffffffffffffffffffffff8888888888888ffff2f8ffffff222222222288888ffff80800fffffffffffffffffffffffffffffffffffffffffffffffffffff
ffffffffffffffffffffffff8888888888888ffffff2f8fffff222222222888888ffffff6860ffffffffffffffffffffffffffffffffffffffffffffffffffff
eeeeeeeeeeeeeeeeeeeeeee8888888888888eeeeeeee2e8eee222222222888888eeeeeee06ee0eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
dddddddddddddddddddddd8888888888888dddddddddd228d222222222288888ddddddddd0000ddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddd888888888888dddddddddddd22822222222288888ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddd88888888888dddddddddddddd2822222222888888ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddd8888888888dddddddddddddd2228222222888888dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddd888888888dddddddddddddd2222282222888888ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddd88888dddddddddddddddddd222222822288888dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddd8ddddddddddddddddddddd2222222282888888dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
ddddddddddddddddddddddddddddddddddddddddddd2222222222888888ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddd2222222222888888dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
ddddddddddddddddddddddddddddddddddddddddd2222222222888888ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddd22222222222888888ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddd2222222222888888dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddd222222222888888ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddd22222222888888dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
ddddddddddddddddddddddddddddddddddddddddd2222228888888dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
ddddddddddddddddddddddddddddddddddddddddd222222888888ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
ddddddddddddddddddddddddddddddddddddddddd22222888888dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddd222888888ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddd228888888ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddd28888ddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
ddddddddddddddddddddddddddddddddddddddddddd8dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd

