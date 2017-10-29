pico-8 cartridge // http://www.pico-8.com
version 9
__lua__
local before_update={c=0}
local after_draw={c=0}

local actors = {} --all actors in world

-- side
local no_side,good_side,bad_side,any_side=0x0,0x1,0x2,0x3
local table_delims={
	['{']="}",
	['[']="]"}
-- register json context here
local _g={
	['true']=true,
	['false']=false,
	no_side=no_side,
	good_side=good_side,
	bad_side=bad_side,
	any_side=any_side
}

-- json parser
-- from: https://gist.github.com/tylerneylon/59f4bcf316be525b30ab
local function error(str)
	printh("error"..str)
	assert()
end

local function match(s,tokens)
	for i=1,#tokens do
		if(s==sub(tokens,i,i)) return true
	end
	return false
end
local function skip_delim(str, pos, delim, err_if_missing)
 if sub(str,pos,pos)!=delim then
  if(err_if_missing) error('expected '..delim..' near position '.. pos)
  return pos,false
 end
 return pos+1,true
end
local function parse_str_val(str, pos, val)
	val=val or ''
	if pos>#str then
		error('end of input found while parsing string.')
	end
	local c=sub(str,pos,pos)
	if(c=='"') return _g[val] or val,pos+1
	return parse_str_val(str,pos+1,val..c)
end
local function parse_num_val(str,pos,val)
	val=val or ''
	if pos>#str then
		error('end of input found while parsing string.')
	end
	local c=sub(str,pos,pos)
	if(not match(c,"-x0123456789.")) return val+0,pos
	return parse_num_val(str,pos+1,val..c)
end
-- public values and functions.

function json_parse(str, pos, end_delim)
	pos=pos or 1
	if(pos>#str) error('reached unexpected end of input.')
	local first=sub(str,pos,pos)
	if match(first,"{[") then
		local obj,key,delim_found={},true,true
		pos+=1
		while true do
			key,pos=json_parse(str, pos, table_delims[first])
			if(key==nil) return obj,pos
			if not delim_found then error('comma missing between table items.') end
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
		for lit_str,lit_val in pairs(_g) do
			local lit_end=pos+#lit_str-1
			if sub(str,pos,lit_end)==lit_str then return lit_val,lit_end+1 end
		end
		local pos_info_str = 'position ' .. pos .. ': ' .. sub(str, pos, pos + 10)
		error('invalid json syntax starting at ' .. pos_info_str)
	end
end

-- player settings
local plyr
local plyr_playing,plyr_hpmax
local plyr_score
local plyr_acc=0.05
local plyr_frames=json_parse('[[17,18,19,18,17],[33,34,35],[49,50,51]]')
local pause_t=0
-- blast
local blast_frames=json_parse('[192,194,196,198,200,202]')
-- camera
local shkx,shky=0,0
local cam_x,cam_y
-- weapons catalog
local dmg_mask,dmg_types=0xff,json_parse('{"dmg_phys":0x0100,"dmg_contact":0x0200,"dmg_energy":0x0400,"dmg_poison":0x0800}')
local weapons=json_parse('{"base_gun":{"sx":48,"sy":8,"blt_frames":[42,42,42],"dmg_type":"dmg_phys","dmg":1,"spread":0.05,"v":0.1,"ttl":90,"dly":32,"ammo":50},"acid_gun":{"blt_frames":[26,27,28],"blts":3,"spread":0.1,"bounce":true,"dmg_type":"dmg_poison","dmg":3,"v":0.1,"xy":[1,0],"ttl":30,"dly":5,"ammo":50},"uzi":{"name":"uzi","icon":21,"sx":32,"sy":8,"blt_frames":[10,12,11],"spread":0.05,"dmg_type":"dmg_phys","dmg":2,"v":0.4,"ttl":30,"dly":5,"ammo":50,"shk_pow":2},"shotgun":{"name":"pump","icon":37,"sx":32,"sy":16,"blt_frames":[10,12,11],"spread":0.05,"blts":3,"dmg_type":"dmg_phys","dmg":2,"inertia":0.95,"v":0.3,"ttl":30,"dly":56,"ammo":25,"shk_pow":2},"glock":{"name":"g.lock","icon":53,"sx":32,"sy":24,"blt_frames":[10,12,11],"spread":0.01,"dmg_type":"dmg_phys","dmg":4,"v":0.5,"ttl":30,"dly":32,"ammo":17,"shk_pow":2},"rpg":{"name":"rpg","icon":23,"sx":48,"sy":8,"actor_cls":"msl_cls","spread":0.02,"v":0.4,"dly":72,"ammo":8,"shk_pow":3},"grenade":{"name":"mortar","icon":55,"sx":48,"sy":24,"actor_cls":"grenade_cls","spread":0.02,"dly":72,"ammo":12,"shk_pow":2.1}}')
for k,v in pairs(weapons) do
	if v.dmg then
		v.dmg=bor(dmg_types[v.dmg_type],v.dmg)
	end
	_g[k]=v
end

-- modifiers
--[[
	weapon bounce
	reduce fire dly
	multiple bullets
	reduced spread
	reduced damage
	world inertia
]]

-- levels
local cur_level=1
local levels=json_parse('[{"name":"desert","ground_tiles":[68,64,65,67,111],"wall_tiles":[66],"solid_tiles_base":112,"shadow_tile":110,"bkg_col":1,"depth":3,"cw":32,"ch":32,"w":[4,6],"h":[4,6],"paths":[2,4],"path":{"bends":[1,2],"w":[4,6],"len":[4,6]},"spawn":[[2,4,"sandman_cls"],[1,3,"worm_cls"]]},{"name":"sewers","ground_tiles":[86,87,87,88],"wall_tiles":[90,89,91],"solid_tiles_base":112,"shadow_tile":94,"borders":[10,11,3],"bkg_col":3,"depth":4,"cw":32,"ch":32,"w":[4,6],"h":[4,6],"paths":[1,2],"path":{"bends":[1,2],"w":[1,2],"len":[2,3]}},{"name":"snow plains","ground_tiles":[70,71,72],"wall_tiles":[74],"solid_tiles_base":112,"shadow_tile":95,"borders":[1,12,7],"bkg_col":7,"depth":5,"cw":32,"ch":48,"w":[4,6],"h":[4,6],"paths":[1,2],"path":{"bends":[1,2],"w":[1,2],"len":[2,3]}},{"name":"palace","ground_tiles":[96,100],"wall_tiles":[97,98,99,108],"solid_tiles_base":112,"shadow_tile":101,"borders":[7,0,5],"bkg_col":9,"depth":5,"cw":32,"ch":48,"w":[4,6],"h":[4,6],"paths":[1,2],"path":{"bends":[1,2],"w":[1,2],"len":[2,3]}},{"name":"lab","ground_tiles":[102,105],"wall_tiles":[103,104,106],"solid_tiles_base":112,"shadow_tile":107,"borders":[6,7,5],"bkg_col":5,"depth":4,"cw":32,"ch":48,"w":[4,6],"h":[3,5],"paths":[4,4],"path":{"bends":[0,2],"w":[1,2],"len":[8,12]}},{"name":"desert","ground_tiles":[68,64,65,67],"wall_tiles":[66],"solid_tiles_base":112,"bkg_col":1,"depth":3,"cw":32,"ch":32,"w":[4,6],"h":[4,6],"paths":[1,2],"path":{"bends":[1,2],"w":[1,2],"len":[2,3]}},{"name":"sewers","ground_tiles":[86,87,87,88],"wall_tiles":[90,89,91],"solid_tiles_base":112,"shadow_tile":94,"borders":[10,11,3],"bkg_col":3,"depth":4,"cw":32,"ch":32,"w":[4,6],"h":[4,6],"paths":[1,2],"path":{"bends":[1,2],"w":[1,2],"len":[2,3]}},{"name":"snow plains","ground_tiles":[70,71,72],"wall_tiles":[74],"solid_tiles_base":112,"shadow_tile":95,"borders":[1,12,7],"bkg_col":7,"depth":5,"cw":32,"ch":48,"w":[4,6],"h":[4,6],"paths":[1,2],"path":{"bends":[1,2],"w":[1,2],"len":[2,3]}},{"name":"palace","ground_tiles":[96,100],"wall_tiles":[97,98,99,108],"solid_tiles_base":112,"shadow_tile":101,"borders":[7,0,5],"bkg_col":9,"depth":5,"cw":32,"ch":48,"w":[4,6],"h":[4,6],"paths":[1,2],"path":{"bends":[1,2],"w":[1,2],"len":[2,3]}},{"name":"lab","ground_tiles":[102,105],"wall_tiles":[103,104,106],"solid_tiles_base":112,"shadow_tile":107,"borders":[6,7,5],"bkg_col":5,"depth":4,"cw":32,"ch":48,"w":[4,6],"h":[3,5],"paths":[4,4],"path":{"bends":[0,2],"w":[1,2],"len":[8,12]}}]')

local blts={len=0}
local parts={len=0}
local zbuf={len=0}
local time_t=0

local face2unit=json_parse('[[1,0],[0.6234,-0.7819],[-0.2225,-0.9749],[-0.901,-0.4338],[-0.901,0.4338],[-0.2225,0.975],[0.6234,0.7819],[1,0]]')

local face1strip=json_parse('[{"flipx":false,"flipy":false},{"flipx":false,"flipy":false},{"flipx":false,"flipy":false},{"flipx":true,"flipy":false},{"flipx":true,"flipy":false},{"flipx":true,"flipy":false},{"flipx":false,"flipy":false},{"flipx":false,"flipy":false}]')
local face2strip=json_parse('[{"strip":1,"flipx":false,"flipy":false},{"strip":2,"flipx":false,"flipy":false},{"strip":2,"flipx":false,"flipy":false},{"strip":1,"flipx":true,"flipy":false},{"strip":2,"flipx":false,"flipy":true},{"strip":2,"flipx":false,"flipy":true},{"strip":2,"flipx":false,"flipy":true},{"strip":2,"flipx":false,"flipy":true}]')
local face3strip=json_parse('[{"strip":1,"flipx":false,"flipy":false},{"strip":2,"flipx":false,"flipy":false},{"strip":2,"flipx":false,"flipy":false},{"strip":3,"flipx":false,"flipy":false},{"strip":1,"flipx":true,"flipy":false},{"strip":3,"flipx":false,"flipy":false},{"strip":3,"flipx":true,"flipy":false},{"strip":3,"flipx":false,"flipy":false}]')

-- fade ramp + screen manager
_shex={}
_shexstr="0123456789abcdef"
for i=1,16 do
	_shex[sub(_shexstr,i,i)]=i-1
end
_pl={"00000015d67",
     "0000015d677",
     "0000024ef77",
     "000013b7777",
     "0000049a777",
     "000015d6777",
     "0015d677777",
     "015d6777777",
     "000028ef777",
     "000249a7777",
     "00249a77777",
     "00013b77777",
     "00013c77777",
     "00015d67777",
     "00024ef7777",
     "0024ef77777"}
_pl_from=0.5
function fade(to,f)
	f=mid(f,1,32) -- sensible boundaries
	to=mid(to,0,1)
	futures_add(function()
		for i=0,f do
			local t=i/f
			local pix=flr(10*lerp(_pl_from,to,t))+1
			for x=0,15 do
				pal(x,_shex[sub(_pl[x+1],pix,pix)],1)
			end
			yield()
		end
		_pl_from=to
	end,after_draw)
end
-- screen manager
local sm_t,sm_cur,sm_next,sm_dly=0,nil,nil,0
function sm_push(s)
	sm_t=0
	if sm_cur then
		sm_dly=sm_t+8
		sm_next=s
		fade(0,8)
	else
		sm_cur=s
		sm_cur:init()
	end
end
function sm_update()
	sm_t+=1
	if sm_next then 
		if sm_dly<sm_t then
			sm_cur=sm_next
			sm_next=nil
			time_t=0
			sm_cur:init()
			fade(0.5,8)
		end
	else
		sm_cur:update()
	end
end
function sm_draw()
	sm_cur:draw()
end
-- futures
function futures_update(futures)
	futures=futures or before_update
	for f in all(futures) do
		local r,e=coresume(f)
		if not r then
			del(futures,f)
		--[[
		else
			printh("exception:"..e)
		]]
		end
	end
end
function futures_add(fn,futures)
	add(futures or before_update,cocreate(fn))
end
-- print text helper
txt_center=false
txt_shade=-1
txt_border=false
function txt_options(c,s,b)
	txt_center=c or false
	txt_shade=s or -1
	txt_border=b or false
end
function txt_print(s,x,y,col)
	if txt_center then
		x-=flr((4*#s)/2+0.5)
	end
	if txt_shade!=-1 then	
		print(s,x+1,y,txt_shade)
		if txt_border then
			print(s,x-1,y,txt_shade)
			print(s,x,y-1,txt_shade)
			print(s,x,y+1,txt_shade)
		end
	end
	print(s,x,y,col)
end
-- helper
function foreach_update(a)
	local n,c,elt=a.len,0
	a.len=0
	for i=1,n do
		elt=a[i]
		if elt:update() then
			c+=1
			a[c]=elt
		end
	end
	-- avoid mlk
	for i=c+1,n do
		a[i]=nil
	end
	a.len=c
end
function clone(src,dst)
	if(src==dst) assert()
	if(type(src)!="table") assert()
	dst=dst or {}
	for k,v in pairs(src) do
		dst[k]=v
	end
	return dst
end
function nop() end
function lerp(a,b,t)
	return a*(1-t)+b*t
end
function smoothstep(t)
	t=mid(t,0,1)
	return t*t*(3-2*t)
end
function rndrng(ab)
	return flr(lerp(ab[1],ab[2],rnd(1)))
end
function rndarray(a)
	return a[flr(rnd(#a))+1]
end
function rotate(a,p)
	local c,s=cos(a),-sin(a)
	return {
		p[1]*c-p[2]*s,
		p[1]*s+p[2]*c}
end
function bpset(x,y,c)
	local d=bor(0x6000,x)+shl(y,7)
	-- todo: fix (not a ramp!)
	c=sget(min(c,7),8)
	c=bor(c,shl(c,4))
	poke(d,c)
	poke(d+64,c)
end
function rspr(sx,sy,x,y,a)
	local ca,sa=cos(a),sin(a)
 local srcx,srcy
 local ddx0,ddy0=ca,sa
 ca*=4
 sa*=4
 local dx0,dy0=sa-ca+4,-ca-sa+4
 for ix=0,7 do
  srcx,srcy=dx0,dy0
  for iy=0,7 do
   if band(bor(srcx,srcy),0xfff8)==0 then
   	local c=sget(sx+srcx,sy+srcy)
   	if c!=14 then
   		pset(x+ix,y+iy,c)
  		end
  	end
   srcx-=ddy0
  	srcy+=ddx0
  end
  dx0+=ddx0
  dy0+=ddy0
 end
end

-- https://github.com/morgan3d/misc/tree/master/p8sort
function sort(t,n)
	if (n<2) return
 local i,j,temp
 local lower = flr(n/2)+1
 local upper = n
 while 1 do
  if lower>1 then
   lower-=1
   temp=t[lower]
  else
   temp=t[upper]
   t[upper]=t[1]
   upper-=1
   if upper==1 then
    t[1]=temp
    return
   end
  end

  i,j=lower,lower*2
  while j<=upper do
   if j<upper and t[j].key<t[j+1].key then
    j += 1
   end
   if temp.key<t[j].key then
    t[i] = t[j]
    i = j
    j += i
   else
    j = upper + 1
   end
  end
  t[i] = temp
 end
end

-- collision
function circline_coll(x,y,r,x0,y0,x1,y1)
	local dx,dy=x1-x0,y1-y0
	local ax,ay=x-x0,y-y1
	local t,d=ax*dx+ay*dy,dx*dx+dy*dy
	if(d==0) return true
	t=mid(t,0,d)
	t/=d
	local ix,iy=x0+t*dx-x,y0+t*dy-y
	return (ix*ix+iy*iy)<r*r	
end
-- zbuffer
function zbuf_clear()
	zbuf.len=0
end
function zbuf_write(obj)
	local xe,ye=cam_project(obj.x,obj.y)
	local ze=obj.z and 8*obj.z or 0
	zbuf.len+=1
	zbuf[zbuf.len]={obj,{xe,ye-ze},key=ye+ze}
end
function zbuf_draw()
	sort(zbuf,zbuf.len)
	for i=1,zbuf.len do
		local o,pos=zbuf[i][1],zbuf[i][2]
		o:draw(pos[1],pos[2])
	end
end

-- collision map
local cmap={}
local cmap_cells={0,1,129,128,127,-1,-129,-128,-127}
function cmap_clear(objs)
	local h,obj
	cmap={}
	for i=1,#objs do
		obj=objs[i]
		if obj.w>0 then
			h=flr(obj.x)+128*flr(obj.y)
			cmap[h]=cmap[h] or {}
			add(cmap[h],obj)
		end
	end
end
function cmap_write(obj)
	local h=flr(obj.x)+128*flr(obj.y)
	cmap[h]=cmap[h] or {}
	add(cmap[h],obj)
end
local cmap_i,cmap_cell,cmap_h
function cmap_near_iterator(x,y)
	cmap_i,cmap_cell=1,1
	cmap_h=flr(x)+128*flr(y)
end
function cmap_near_next()
	if(cmap_cell==nil) assert()
	while(cmap_cell<=9) do
		local h=cmap_h+cmap_cells[cmap_cell]
		local objs=cmap[h]
		if objs and cmap_i<=#objs then
			local obj=objs[cmap_i]
			if(not obj) assert()
			cmap_i+=1
			return obj
		end
		cmap_i=1
		cmap_cell+=1
	end
	return nil
end
function cmap_draw()
	local h=flr(plyr.x)+128*flr(plyr.y)

	for k,v in pairs(cmap) do
		local s=(h==k and "*" or "")
		local x,y=cam_project(k%128,flr(k/128))
		print(s..(#v),x,y,7)
	end
end

-- camera
function cam_shake(u,v,pow)
	shkx=pow*u
	shky=pow*v
end
function cam_update()
	shkx*=-0.7-rnd(0.2)
	shky*=-0.7-rnd(0.2)
	if(abs(shkx)>0.5 or abs(shky)>0.5) camera(shkx,shky)
end
function cam_track(x,y)
 cam_x,cam_y=(x*8)-4,(y*8)-4
end
function cam_project(x,y)
 local sx,sy=x*8,y*8
 return 64+sx-cam_x,64+sy-cam_y
end

-- special fxs
function update_part(self)
	if(self.t<time_t or self.r<0) return false
	self.x+=self.dx
	self.y+=self.dy
	self.z+=self.dz
	self.dx*=self.inertia
	self.dy*=self.inertia
	self.dz*=self.inertia
	self.r+=self.dr
	zbuf_write(self)
	return true
end
function make_part(x,y,z,src)
	local p={
		x=x,y=y,z=z,
		dx=0,dy=0,dz=0,
		r=1,dr=0,
		inertia=0,
		t=time_t+src.dly,
		update=update_part
	}
	for k,v in pairs(src) do
		p[k]=v
	end
	-- randomize selected values
	if src.rnds then
		for k in all(rnds) do
			p[k]*=(1+0.2*rnd())
		end
	end
	parts.len+=1
	parts[parts.len]=p
	return p
end

_g.update_static_part=function(self)
	if(self.t<time_t or self.r<0) return false
	self.r+=self.dr
	zbuf_write(self)
	return true
end
_g.draw_circ_part=function(self,x,y)
	circfill(x,y,8*self.r,self.c)
end
_g.draw_spr_part=function(self,x,y)
	local sw=self.sw
	spr(self.spr,x-4*sw,y-4*sw,sw,sw)
end
_g.draw_txt_part=function(self,x,y)
	local l=2*#self.txt
	print(self.txt,x-l+1,y-2,0)
	print(self.txt,x-l,y-2,7)
end
local all_parts=json_parse('{"flash_part_cls":{"dly":4,"r":0.5,"c":7,"dr":-0.1,"update":"update_static_part","draw":"draw_circ_part"},"smoke_part_cls":{"dly":18,"r":0.3,"dr":-0.01,"c":7,"rnds":["r"],"draw":"draw_circ_part"}}')

-- bullets
function blt_update(self)
	if self.t>time_t then
		local x0,y0=self.x,self.y
		local x1,y1=x0+self.dx,y0+self.dy
		local inertia=self.wp.inertia
		if inertia then
			self.dx*=inertia
			self.dy*=inertia
		end
		local s=solid(x1,y0) or solid(x0,y1) or solid(x1,y1)
		if s then
			-- todo: blt hit wall
			return false
		end
		
		-- actors hit?
		-- todo:get all hitable actors in range
		for a in all(actors) do
			if (self.side!=a.side or a.side==any_side) and circline_coll(a.x,a.y,a.w,x0,y0,x1,y1) then
				a:hit(self.wp.dmg)
				return false
			end
		end
		self.prevx,self.prevy=x0,y0
		self.x,self.y=x1,y1
		zbuf_write(self)
		return true
	end
	return false
end
function make_blt(a,wp)
	local n=wp.blts or 1
	for i=1,n do
		if a.ammo then
			if a.ammo<=0 then
				-- todo: click sound
				sfx(3)
				return
			end
			a.ammo-=1
		end
		if wp.sfx then
			sfx(wp.sfx)
		end
		local ang=a.angle+wp.spread*(rnd(2)-1)
		local u,v=cos(ang),sin(ang)
		local b={
			x=a.x+0.5*u,y=a.y+0.5*v,
			wp=wp,
			dx=wp.v*u,dy=wp.v*v,
			side=a.side,
			facing=flr(8*(ang%1))
		}
		if wp.actor_cls then
			make_actor(0,0,
				clone(bad_actors[wp.actor_cls],b))
		else
			-- for fast collision
			clone({
				t=time_t+wp.ttl,
				prevx=b.x,prevy=b.y,
				update=blt_update,
				draw=draw_blt},b)
			blts.len+=1
			blts[blts.len]=b
		end
		-- muzzle flash
		if(i==1) make_part(b.x,b.y,0.5,all_parts.flash_part_cls)
	end
end
function draw_blt(b,x,y)
	palt(0,false)
	palt(14,true)
	local spr_options=face3strip[b.facing+1]
	spr(b.wp.blt_frames[spr_options.strip],x-4,y-4,1,1,spr_options.flipx,spr_options.flipy)
end

-- map
local rooms
function make_rooms(x,y,rules)
	rooms={}
	for i=0,rules.cw-1 do
		for j=0,rules.ch-1 do
			mset(i,j,rules.solid_tiles_base)
		end
	end
	make_room(
			x,y,
			rndrng(rules.w),
			rndrng(rules.h),
			rules.depth,
			rules)
	rooms_done(rules)
end
local tiles_sides={
	{0,0},
	{1,0},
	{0,1},
	{-1,0},
	{0,-1}}
function tile_flags(cx,cy)
	local c=0
	for i=0,#tiles_sides-1 do
		local p=tiles_sides[i+1]
		local s=mget(cx+p[1],cy+p[2])
		if s==0 or fget(s,7) then
			c=bor(c,shl(1,i))
		end
	end
	return c
end

function rooms_done(rules)
	local tf,t
	local walls={}
	for i=0,rules.cw-1 do
		for j=0,rules.ch-1 do
			-- borders
			tf=tile_flags(i,j)
			if band(tf,1)!=0 then
				tf=shr(band(tf,0xfffe),1)
				t=rules.solid_tiles_base+tf
				mset(i,j,t)
				-- south not solid?
				if band(tf,0x2)==0 then
					if rnd()<0.8 then
					 t=rules.wall_tiles[1]
					else
						t=rndarray(rules.wall_tiles)
					end
					add(walls,{i,j+1,t})
				end
			end
		end
	end
	for w in all(walls) do
		mset(w[1],w[2],w[3])
		mset(w[1],w[2]+1,rules.shadow_tile)
	end
end

function make_room(x,y,w,h,ttl,rules)
	if(ttl<0) return
	local r={
		x=x,y=y,
		w=w,h=h}
	r=dig(r,rules)
	if r then
		add(rooms,r)
		local n=ttl*rndrng(rules.paths)
		for i=1,n do
			local a=flr(rnd(4))/4
			local v=rotate(a,{1,0})
			local bends=rndrng(rules.path.bends)
			-- starting point
			local hh,hw=r.w/2,r.h/2
			local cx,cy=r.x+hw,r.y+hh
			x,y=cx+v[1]*hw,cy+v[2]*hh
			make_path(x,y,a,
				bends,ttl-1,rules)
		end
	end
end
function make_path(x,y,a,n,ttl,rules)
	-- end of corridor?
	if n<=0 then
		make_room(
			x,y,
			rndrng(rules.w),
			rndrng(rules.h),
			ttl-1,
			rules)
		return
	end
	local w,h=
		rndrng(rules.path.w),
		rndrng(rules.path.len)
	-- rotate
	local wl=rotate(a,{h,w})
	local c={
		x=x,y=y,
		w=wl[1],h=wl[2]
	}
	-- stop invalid paths
	if dig(c,rules) then
		a+=(rnd(1)>0.5 and 0.25 or -0.25)
		make_path(
			c.x+c.w,c.y+c.h,
			a,n-1,ttl,rules)
	end
end
function dig(r,rules)
	local cw,ch=rules.cw-1,rules.ch-1
	local x0,y0=mid(r.x,1,cw),mid(r.y,1,cw)
	local x1,y1=mid(r.x+r.w,1,ch),mid(r.y+r.h,1,ch)
	x0,x1=min(x0,x1),max(x0,x1)
	y0,y1=min(y0,y1),max(y0,y1)
	cw,ch=x1-x0,y1-y0
	if cw>0 and ch>0 then
		for i=x0,x1 do
			for j=y0,y1 do
				if rnd()<0.8 then
					mset(i,j,rules.ground_tiles[1])
				else							
					mset(i,j,rndarray(rules.ground_tiles))
				end
			end
		end
		return {x=x0,y=y0,w=cw,h=ch}
	end
	return nil
end

function solid(x, y)
 return fget(mget(x,y),7)
end

function solid_area(x,y,w,h)

 return 
  solid(x-w,y-h) or
  solid(x+w,y-h) or
  solid(x-w,y+h) or
  solid(x+w,y+h)
end

function lineofsight(x1,y1,x2,y2,dist)
	x1,y1=flr(x1),flr(y1)
	x2,y2=flr(x2),flr(y2)
	local dx=x2-x1
	local ix=dx>0 and 1 or -1
	dx=shl(abs(dx),1)

	local dy=y2-y1
	local iy=dy>0 and 1 or -1
	dy=shl(abs(dy),1)

	if(dx==0 and dy==0) return true
	
	if dx>=dy then
		error=dy-dx/2
 	while x1!=x2 do
   if (error>0) or ((error==0) and (ix>0)) then
	   error-=dx
 	  y1+=iy
			end

 	 error+=dy
 	 x1+=ix
 	 dist-=1
 	 if(dist<0) return false
	if(solid(x1,y1)) return false
 	end
	else
 	error=dx-dy/2

 	while y1!=y2 do
  	if (error>0) or ((error==0) and (iy > 0)) then
  	 error-=dy
  	 x1+=ix
		 end
	
  	error+=dx
  	y1+=iy
			dist-=1
		 if(dist<0) return false
	 	if(solid(x1,y1)) return false
 	end
 end
	return true 
end
-- true if a will hit another
-- actor after moving dx,dy
function solid_actor(a,dx,dy)
	cmap_near_iterator(a.x+dx,a.y+dy)
	local a2=cmap_near_next()
	while a2 do
  if a2 != a then
   local x,y=(a.x+dx)-a2.x,(a.y+dy)-a2.y
   if abs(x)<(a.w+a2.w) and
      abs(y)<(a.w+a2.w)
   then 
    -- collision damage?
    if a2.dmg and band(a.side,a2.side)!=0 and a.hit then
    	a:hit(a2.dmg)
    end
    
    -- moving together?
    -- this allows actors to
    -- overlap initially 
    -- without sticking together    
    if (dx!=0 and abs(x) <
	abs(a.x-a2.x)) then
     local v=a.dx+a2.dy
     a.dx=v/2
     a2.dx=v/2
     return true 
    end
    
    if (dy!=0 and abs(y) <
	abs(a.y-a2.y)) then
     local v=a.dy+a2.dy
     a.dy=v/2
     a2.dy=v/2
     return true 
    end    
   end
  end
	a2=cmap_near_next()
 end
 return false
end

-- checks both walls and actors
function solid_a(a, dx, dy)
	if(solid_area(a.x+dx,a.y+dy,a.w,a.w)) return true
	return solid_actor(a, dx, dy) 
end

-- custom actors
function draw_anim_spr(a,x,y)
	palt(0,false)
	palt(14,true)	
	local i=flr(lerp(1,#a.frames,1-(a.t-time_t)/a.ttl))
	spr(a.frames[i],x-8,y-8,2,2)
end

function plyr_die(self)
	
end

function die_actor(self)
	if rnd()>0.5 then
		make_actor(self.x,self.y,health_cls)
	else
		make_actor(self.x,self.y,ammo_cls)
	end
	
	--[[
	if self.drop_value then
		local v=flr(rnd(self.drop_value))
		if v>0 then
		make_actor(self.x,self.y,loot[v+1])
		end
	end
	]]
end

function hit_actor(self,dmg)
	self.hit_t=time_t+8
	self.hp-=band(dmg_mask,dmg)
	if self.hp<=0 then
		self.hp=0
	 --if(self.die) self:die()
	if rnd()>0.5 then
		make_actor(self.x,self.y,health_cls)
	else
		make_actor(self.x,self.y,ammo_cls)
	end
	
			del(actors,self)
	end
end
function make_blast(x,y)
	pause_t=4
	return make_actor(x,y,{
		w=0.8,
		bounce=0,
		dmg=bor(dmg_phys,15),
		side=any_side,
		t=time_t+12,
		ttl=12,
		frames=blast_frames,
		draw=draw_anim_spr,
		update=function(a)
			if(a.t<time_t) del(actors,a)
		end,
		hit=nop})
end

-- custom actors
warp_cls={
	w=0,
	captured=false,
	frames={80,81},
	draw=nop,
	update=function(self)
		mset(x,y,self.frames[flr(time_t/8)%#self.frames+1])
		if (self.captured) return
		local dx,dy=plyr.x-self.x,plyr.y-self.y
		local d=dx*dx+dy*dy
		if d<4 then
			self.captured=true
			futures_add(function()
				plyr_playing=false
				d=sqrt(d)
				local a=atan2(dx,dy)
				for i=1,90 do
					local dist=lerp(d,0,i/90)
					plyr.x,plyr.y=self.x+dist*cos(a),self.y+dist*sin(a)
				end
				cur_level+=1
				sm_push(warp_screen)
			end)
		end
	end
}

health_cls={
	spr=48,
	w=0,
	update=function(self)
		local dx,dy=plyr.x-self.x,plyr.y-self.y
		if abs(dx)<0.5 and abs(dy)<0.5 then
			plyr.hp=min(plyr_hpmax,plyr.hp+2)
			make_part(self.x,self.y,0,{
				dz=0.1,
				inertia=0.91,
				dly=72,
				txt=(plyr.hp==plyr_hpmax) and "max. hp" or "hp+2",
				draw=_g.draw_txt_part})
			del(actors,self)
		end
	end
}
ammo_cls={
	spr=32,
	w=0,
	update=function(self)
		local dx,dy=plyr.x-self.x,plyr.y-self.y
		if abs(dx)<0.5 and abs(dy)<0.5 then
			plyr.ammo=min(plyr.wp.ammo,plyr.ammo+10)
			make_part(self.x,self.y,0,{
				dz=0.1,
				inertia=0.91,
				dly=72,
				txt=(plyr.wp.ammo==plyr.ammo) and "max. ammo" or "ammo+10",
				draw=_g.draw_txt_part})
			del(actors,self)
		end
	end
}

_g.npc_rnd_move=function(self)
	if self.move_t<time_t then
		self.dx,self.dy=0.05*(rnd(2)-1),0.05*(rnd(2)-1)
		self.move_t=time_t+8+rnd(8)
	end
end
_g.blast_on_hit=function(self,dmg)
	if(band(dmg_types.dmg_contact,dmg)!=0) return
	self.hit_t=time_t+8
	self.hp-=1--band(dmg_mask,dmg)
	if self.hp<=0 then
		make_blast(self.x,self.y)
		del(actors,self)
	end
end
_g.blast_on_touch=function(self)
	make_blast(self.x,self.y)
	del(actors,self)
end
_g.smoke_emitter=function(self)
	if time_t%2==0 then
		make_part(self.x,self.y,0,all_parts.smoke_part_cls)
	end
end
_g.draw_rspr_actor=function(self,x,y)
	local ang=atan2(self.dx,self.dy)
	rspr(self.sx,self.sy,x-4,y-4,1-ang)
end
_g.sandman_update=function(self)
	if self.seek_t<time_t and lineofsight(self.x,self.y,plyr.x,plyr.y,4) then
		self.seek_t=time_t+8+rnd(8)
		local dx,dy=plyr.x-self.x,plyr.y-self.y
		local d=sqrt(dx*dx+dy*dy)
		if(d<0.01) return
		dx/=d
		dy/=d
		self.dx=-0.02*dx
		self.dy=-0.02*dy
		self.angle=atan2(dx,dy)%1
		self.facing=flr(8*self.angle)
		if self.fire_dly<time_t then				
			make_blt(self,self.wp)		
			self.fire_dly=time_t+self.wp.dly
		end
	elseif self.move_t<time_t then
		self.dx,self.dy=0.05*(rnd(2)-1),0.05*(rnd(2)-1)
		self.move_t=time_t+16+rnd(16)
	end
end
bad_actors=json_parse('{"barrel_cls":{"side":"any_side","inertia":0.8,"spr":128,"hit":"blast_on_hit"},"msl_cls":{"side":"any_side","inertia":1.01,"sx":80,"sy":24,"update":"smoke_emitter","draw":"draw_rspr_actor","hit":"blast_on_hit","touch":"blast_on_touch"},"sandman_cls":{"hp":3,"wp":"base_gun","frames":[[4,5,6]],"move_t":0,"drop_value":3,"die":"npc_die","update":"sandman_update"},"scorpion_cls":{"w":1.8,"hp":10,"frames":[[135,137]],"move_t":0,"update":"npc_rnd_move"},"worm_cls":{"palt":3,"w":0.2,"inertia":0.8,"dmg_type":"dmg_contact","dmg":1,"frames":[[7,8]],"move_t":0,"update":"npc_rnd_move"},"slime_cls":{"palt":3,"w":0.2,"inertia":0.8,"dmg_type":"dmg_contact","dmg":1,"frames":[[29,30,31,30]],"move_t":0,"update":"npc_rnd_move"}}')
for k,v in pairs(bad_actors) do
	v.dmg=bor(dmg_types[v.dmg_type],v.dmg)
end

_g.wpdrop_draw=function(self,x,y)
	draw_actor(self,x,y)
	if self.near_plyr_t>time_t then
		draw_txt_part(self,x,y-8)
	end
end
_g.wpdrop_update=function(self)
	local dx,dy=plyr.x-self.x,plyr.y-self.y
	if abs(dx)<0.5 and abs(dy)<0.5 then
		self.near_plyr_t=time_t+30
		if btnp(4) then
			self.near_plyr_t=0
			make_part(self.x,self.y,0,{
				dz=0.1,
				inertia=0.91,
				dly=72,
				txt=self.txt,
				draw=_g.draw_txt_part})
			-- swap weapons
			local wp,ang=plyr.wp,rnd()
			-- todo: fix rentrancy
			make_actor(plyr.x,plyr.y,{
				w=0,
				inertia=0.9,
				btn_t=0,
				near_plyr_t=0,
				draw=wpdrop_draw,
				update=wpdrop_update,
				dx=0.1*cos(ang),
				dy=0.1*sin(ang),
				drop=wp,
				ammo=plyr.ammo,
				spr=wp.icon,
				txt=wp.name})
			-- pick drop
			plyr.wp=self.drop
			plyr.ammo=self.ammo
			del(actors,self)
		end
	end
end
local wpdrop_cls=json_parse('{"w":0,"inertia":0.9,"btn_t":0,"near_plyr_t":0,"draw":"wpdrop_draw","update":"wpdrop_update"}')

-- actor
-- x,y in map tiles (not pixels)
function make_actor(x,y,src)
	local a={
		x=x,
		y=y,
		dx=0,
		dy=0,
		frame=0,
		inertia=0.6,
		bounce=1,
		hp=1,
		seek_t=0,
		hit_t=0,
		fire_t=0,
		fire_dly=rnd(16),
		w=0.4, -- actors are round!
		angle=0,
		facing=0, -- trig order e/n/w/s
		side=bad_side,
		draw=draw_actor,
		die=die_actor,
		hit=hit_actor}
	if src then
		for k,v in pairs(src) do
			a[k]=v
		end
	end
	add(actors,a)
	return a
end

function move_actor(a)
	if a.update then
		a:update()
	end

 -- static? no collision check
	if a.dx==0 and a.dy==0 then
		zbuf_write(a)
		return
	end
	local touch=false
 if not solid_a(a,a.dx,0) then
  a.x+=a.dx
 else
  -- otherwise bounce
  touch=true
  a.dx*=-a.bounce
  sfx(2)
 end

 -- ditto for y
 if not solid_a(a,0,a.dy) then
  a.y+=a.dy
 else
 	touch=true
  a.dy*=-a.bounce
  sfx(2)
 end
 
 if touch and a.touch then
 	a:touch()
 end
 
 -- apply inertia
 a.dx*=a.inertia
 a.dy*=a.inertia
 
 a.frame+=abs(a.dx)*4
 a.frame+=abs(a.dy)*4

 zbuf_write(a)
end

function draw_actor(a,sx,sy)
	if a.safe_t and a.safe_t>time_t and band(time_t,1)==0 then
		return
	end
	
	local sw=flr(a.w)+1
	sx,sy=sx-4*sw,sy-4*sw
	-- shadow
	palt(14,true)	
	spr(16,sx,sy+7)
	palt(14,false)	
	-- hit effect
	local tcol=a.palt or 14
	if a.hit_t>time_t then
		memset(0x5f00,0xf,16)
		pal(tcol,tcol)
 end
 local s,flipx,flipy=a.spr,false,false
 if a.frames then
 	local spr_options=(#a.frames==3 and face3strip or face1strip)[a.facing+1]
		local frames=a.frames[spr_options.strip or 1]
		s=frames[flr(a.frame%#frames)+1]
		flipx,flipy=spr_options.flipx,spr_options.flipy
	end
	-- actor
	palt(tcol,true)
	spr(s,sx,sy,sw,sw,flipx,flipy)
	palt(tcol,false)
	pal()
 palt(0,false)
 palt(14,true)
	local wp=a.wp
	if wp then
		local u,v=cos(a.angle),sin(a.angle)
		-- recoil animation
		local f=-2*max(0,a.fire_t-time_t)/8
		rspr(wp.sx,wp.sy,sx+4*u+f*u,sy+4*v+f*v,1-a.angle)
	end
 palt(14,false)	
end

-- player actor
function make_plyr()
	plyr_score=0
	plyr_playing=true
	plyr_hpmax=5
	plyr=make_actor(18,18,{
		hp=5,
		side=good_side,
		-- todo: rename to strips
		frames=plyr_frames,
		wp=weapons.uzi,
		ammo=weapons.uzi.ammo,
		safe_t=time_t+30,
		die=plyr_die
	})
	return plyr
end

function control_player()
	local wp,angle=plyr.wp,plyr.angle
 -- how fast to accelerate
 local dx,dy=0,0
 if(btn(0)) plyr.dx-=plyr_acc angle=0.5
 if(btn(1)) plyr.dx+=plyr_acc angle=0
 if(btn(2)) plyr.dy-=plyr_acc angle=0.25
 if(btn(3)) plyr.dy+=plyr_acc angle=0.75	
	
	if wp and btn(4) and plyr.fire_dly<time_t then
	 	-- todo: rename recoil
		if plyr.ammo>0 then
			plyr.fire_t=time_t+8
			plyr.fire_dly=time_t+wp.dly
			make_blt(plyr,wp)
			local u=face2unit[plyr.facing+1]
			plyr.dx-=0.05*u[1]
			plyr.dy-=0.05*u[2]
			cam_shake(u[1],u[2],wp.shk_pow)
		end
	elseif plyr.fire_dly<time_t then
		plyr.facing=flr(8*angle)
		plyr.angle=angle
	end
	
 -- play a sound if moving
 -- (every 4 ticks)
 
 	if (abs(plyr.dx)+abs(plyr.dy)>0.1
     and (time_t%4)==0) then
  sfx(1)
 end 
 
 cam_track(plyr.x,plyr.y)
end

function make_level(lvl)
	local rules=levels[lvl]
	make_rooms(8,8,rules)
	-- spawn entities
	for i=2,#rooms do
		local r,sp=rooms[i],rndarray(rules.spawn)
		local n=flr(lerp(sp[1],sp[2],rnd()))
		for k=1,n do
			local x,y=r.x+lerp(0,r.w,rnd()),r.y+lerp(0,r.h,rnd())
			make_actor(x,y,bad_actors[sp[3]])
		end
	end
end

local game={}
function game:update()
	pause_t-=1
	if(pause_t>0) return
	pause_t=0

	-- todo: update vs clear
	cmap_clear(actors)
	zbuf_clear()
	control_player(plyr)
	
	foreach(actors,move_actor)
	foreach_update(blts)
	foreach_update(parts)
	cam_update()
end

function game:draw()
	local lvl=levels[cur_level]
 cls(lvl.bkg_col)
 map(0,0,64-cam_x,64-cam_y,32,32,1)
 zbuf_draw()
 
 palt()
 if lvl.borders then
	 pal(10,lvl.borders[1])
 	pal(9,lvl.borders[2])
	 pal(1,lvl.borders[3])
 end
 map(0,0,64-cam_x,64-cam_y,32,32,2)
	pal()
	
	rectfill(1,1,34,9,0)
	rect(2,2,33,8,6)
	local hp=max(0,plyr.hp)
	rectfill(3,3,flr(32*hp/plyr_hpmax),7,8)
	txt_options(false,0)
	txt_print(hp.."/"..plyr_hpmax,12,3,7)

	palt(14,true)
	palt(0,false)
	spr(plyr.wp.icon,2,10)
	txt_print(plyr.ammo,14,12,7)
	
 --rectfill(0,0,127,8,1)
 --local cpu=flr(1000*stat(1))/10
 --print(""..cpu.."% "..stat(4).."kb",2,2,7)

	--[[
	local tf,t
	local rules=levels[cur_level]
	for i=0,rules.cw-1 do
		for j=0,rules.ch-1 do
			-- borders
			tf=tile_flags(i,j)
			if band(tf,1)!=0 then
				local x,y=cam_project(i,j)
				print(shr(band(tf,0xfe),1),x,y,(i+j)%15+1)
			end
		end
	end
	]]
end
function game:init()
	poke(0x5f2c,0)
	if not plyr then
		plyr=make_plyr()	
	end
	local r=rooms[1]
	plyr.x=r.x+r.w/2
	plyr.y=r.y+r.h/2
	cam_track(plyr.x,plyr.y)
	
	--make_actor(12,12,warp_cls)

 --make_actor(12,12,health_cls)
	--make_actor(14,14,ammo_cls)

	--[[
	make_actor(12,12,
		clone(wpdrop_cls,{
			ammo=weapons.shotgun.ammo,
			drop=weapons.shotgun,
			spr=weapons.shotgun.icon,
			txt=weapons.shotgun.name}))
	]]
	
end

function spawner(n,src)
	for i=1,n do
		local x,y=0,0
		local ttl=5
		while(solid(x,y) and ttl>0) do
			x,y=flr(rnd(16)),flr(rnd(16))
			ttl-=1
		end
		if(ttl<0) return
		-- found empty space!
		make_actor(x+0.5,y+0.5,src)
	end
end

local warp_screen={}
local gia,gr,ga
function warp_screen:update()
	ga+=gia
	gia=mid(-.1,gia,.1)
	gr=mid(-.1,gr,.1)
	if btnp(4) then
		make_level(cur_level)
		sm_push(game)
	end
end
function draw_warp()
	local x,y,y2,a,r,u,v
	for y=0,15 do
		y2=y*y
		for x=0,15 do
			a=4*atan2(y,x)
			r=sqrt(x*x+y2)
			u=gr*r+ga
			v=flr(4+4*cos(u+a))
			bpset(15+x,15-y,v)
			v=flr(4+4*cos(u+2-a))
			bpset(15-x,15-y,v)
			v=flr(4+4*cos(u+a+2))
			bpset(15-x,15+y,v)
			v=flr(4+4*cos(u+4-a))
			bpset(15+x,15+y,v)
		end
	end
end
function warp_screen:draw()
	cls(0)
		
	draw_warp()

	local x,y=cos(time_t/64),sin(time_t/64)
	rspr(8,24,32+8*x,32+8*y,time_t/16)
	
	txt_options(true,0)
	txt_print(levels[cur_level].name,32,8,7)
end
function warp_screen:init()
	poke(0x5f2c,3)
end

local game_over_screen={}
function game_over_screen:update()
	if time_t>180 or btnp(4) or btnp(5) then
		sm_push(title_screen)
	end
end
function game_over_screen:draw()
	cls(1)
	txt_options(true,3)
	txt_print("you are dead",64,12,7)

	txt_print("max level:"..cur_level,64,24,7)		
end
function game_over_screen:init()
	poke(0x5f2c,3)
end

local title_screen={}
function title_screen:update()
	ga+=gia
	gia=mid(-.1,gia,.1)
	gr=mid(-.1,gr,.1)
	if btnp(4) or btnp(5) then
		sm_push(warp_screen)
	end
end
function title_screen:draw()
	cls(1)
	draw_warp()
	txt_options(true,0,true)
 txt_print("nu   lear",32,2,7)
	spr(144,24,0)
	txt_options(true,0,true)
	txt_print("   lone",36,11,7)
	spr(144,24,9)
	
	if time_t%2==0 then
		txt_options(true,3)
		txt_print("press start",32,54,11)
	end
end
function title_screen:init()
	poke(0x5f2c,3)	
	cur_level=1
	ga,gia,gr=0,.01,.01
	palt(14,true)
	palt(0,false)
end

-- game loop
local perf={}
local perf_ramp={8,11,5}
local perf_counters={"up","draw","mem"}

function _update60()
	time_t+=1
	futures_update(before_update)
	sm_update()

	perf[time_t%64+1]={
		stat(1),
		0,
		stat(0)/1024}
end

function draw_perf(x)
	rectfill(x,0,x+64,32,1)
	local t=64-time_t%64
	line(x+t,0,x+t,32,6)
	if perf[1] then
		for k=1,#perf_counters do
			local scale=32
			if(perf[1][k]<0.5) scale=64
			if(perf[1][k]<0.25) scale=128
			color(perf_ramp[k])
			for i=1,#perf do
				local p=perf[i]
				pset(x+64-i,32-scale*p[k])
			end
			print(perf_counters[k]..":"..(flr(1000*perf[1][k])/10).."%",x+2+1,28+6*k,0)
			print(perf_counters[k]..":"..(flr(1000*perf[1][k])/10).."%",x+2,28+6*k,perf_ramp[k])
		end
	end	
end

function _draw()
	sm_draw()
	futures_update(after_draw)
	
	perf[time_t%64+1][2]=stat(1)
	--draw_perf(64)
end
function _init()
	cls(0)
	sm_push(title_screen)
end


__gfx__
00000000e000000ee000000ee000000ee000000ee000000ee000000e3333333333333333eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000ee000000ee000000e
000000000555555005555550055555500f66ff600f66ff600f66ff603333333333333333e00e00eeeeeeeeeeeee99eeeeeeeeeee01111a10011111a001111110
0070070005558580055558500555558005585850055858500558585033333333333333330880870eee9999eeee9aa9eeeee99eee01c00000011c00000111c000
000770000555252005555250055555200ff66ff00ff66ff00ff66ff033300033333333330288820ee999aa9eee9aa9eeee9aa9ee0ccc0c000cccc0c00ccccc00
0007700005555550055555500555555006ff66f006ff66f006ff66f0330fef0333000033e02820eee999aa9eee9999eee99aa9ee0cccccc00cccccc00cccccc0
007007000555555005555550055555500f66f6600f66f6600f66f660330e0e0330efef03ee020eeeee9999eeee9999eee9999eee055556500555556005555550
0000000005000050e050050ee005500ee06f0ff0e006f0f00f006f0e30ef0fe00ef00fe0eee0eeeeeeeeeeeeeee99eeeee99eeee07000070e070070ee006700e
00000000e0eeee0eee0ee0eeeee00eeeee00e00eeee00e0ee0ee00ee3300300330033003eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0eeee0eee0ee0eeeee00eee
e111111eee00000eee00000eee00000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00eeeeeeeeeeeeee00eee
11111111e0999aa0e09999a0e0999990eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000eeeee777eeeeeeeeeeeeeeee33eeeeeeeeeeeee0370eeee0000eeee0370ee
e111111e099414100999414009999410eeeeeeeeeeeeeeeee0000000e77777770bb0000070077777ee3bb3eeee3bb3eeeee3bbeee03bb70ee03bb70eee0370ee
eeeeeeee094444400994444009994440ee00000eee77777ee0b333b0e700000703b6606070000707e3bbbb3eeebbbbeeee3bbbeee03bbb0e03bbbb70ee03b0ee
eeeeeeee044455500444455004444450ee000eeeee707eeee0113110e70000070335505070000707e3bbbb3eeebbbbeeeebbb3eee03bbb0e03bbbbb0ee03b0ee
eeeeeeee0333bab003333ba0033333b0eee0eeeeeee7eeeee0000000e77777770550000070077777ee3bb3eeee3bb3eeeebb3eee03bbbbb003bbbbb0e03bbb0e
eeeeeeee05000050e050050ee005500eeee0eeeeeee7eeeeeeeeeeeeeeeeeeee0660eeee7007eeeeeeeeeeeeeee33eeeeeeeeeee03bbbbb003bbbbb003bbbbb0
eeeeeeeee0eeee0eee0ee0eeeee00eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000eeee7777eeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000
ee00000eee0000eeee0000eeee0000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000eeeeeeeeeeeeeeeeeeeeeeeee000000ee000000ee000000e
e0bbbbb0e0999a0ee099aa0ee0999a0eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000eeeeeeeeee6bb6eeeeeeeeee022898900228898002288890
e0777770099999a009999aa0099999a0ee000000ee777777eeee0e0eeeee7e7e0000000000000000eeeaaeeee6bb776eeee00eee0228a8a002288a80022888a0
e0373730099999a009999aa0099999a0e0496660e7000007ee001010ee7707070000000000000000eea77aeeebbbb7beee0000ee022888800228888002288880
e0353530044444400444444004444440e0445550e7000007e055c1c0e70000070000000000000000eea77aeeebbbbbbeee0000ee022767600228767002288760
e033333003333bb00333bbb003333bb0e0400000e7077777e0501010e70707070000000000000000eeeaaeeee6bbbb6eeee00eee022686800228686002288680
e0533350050000500500000000000050ee0eeeeeee7eeeeeee0e0e0eee7e7e7e0000000000000000eeeeeeeeee6bb6eeeeeeeeee02000020e020010ee002100e
ee00000ee0eeee0ee0eeeeeeeeeeee0eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000eeeeeeeeeeeeeeeeeeeeeeee00eeee00ee0ee0eeeee00eee
ee00000eee0000eeee0000eeee0000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000eeeeeeee008200000000000055555555eeeeeeee00000000
e0666660e0999a0ee0999a0ee0999a0eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000ee88eeee109400000000000005000050ee0000ee00000000
e0777770094141a0091414a0094141a0ee00000eee77777eee000000ee7777770000000000000000e000000e21a900000000000000eeee00e009900e00000000
e0dd8dd0094444900944449009444490e076670ee700007ee03bb660e70000070000000000000000e088777031b300000000000050eeee05e0a99a0e00000000
e0d888d0044555400455544004455540e055000ee700777e0453b000700007770000000000000000e055667045c100000000000000eeee00e04aa40e00000000
e0d686d0033babb00339bbb0033babb0e050eeeee707eeee04400eee70077eee0000000000000000e000000e51d100000000000050ffff05e044440e00000000
e0dd6dd005000050000000b003000000ee0eeeeeee7eeeeee00e0eeee77e7eee0000000000000000ee88eeee65e200000000000000822800e004400e00000000
e0000000e0eeee0eeeeeee0ee0eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000eeeeeeee76fd00000000000005000050ee0000ee00000000
444444444444444404040404444444444444444444444444777777777777777777777677777677775c775c5c76666667dddddddddddddddd121212eed2dddddd
44444444449944444040404044444444444444444940044077777777777777777667777777657777ccc7c7c565151516ddddddddd1eddddd21ee21de20ddd2ed
44b4b4444549544404040404494444444444444450450945777777777766667775577777765777771cc7c77c71515177ddddddddd11ddddd11dde212ddd0d02d
435b5344445544444040404045444494444444440444504477777777765555777777777675677777c111ccc565151777dddddddddddddddd21dde121dd02dd0d
453535444444444404040404444444544444444445094544777777777555556777777777775677775c5cc77c51515667ddddddddddddeedd12111212d02d0ddd
44555444444444444040404044494444444444444450949477777777775555577777677777657777c5c5c1c775151557ddddddddddd12e1d2121de21dd0dd0dd
44444444444444440404040444454444444444444440040477777777777755577777777777577777515c7ccc77515717dddddddddddd11dd12121d12d2dd02d0
44444444444444444040404044444444444444440445544477777777777777777777777777777777c115c7c577777777dddddddddddddddd2121212100dd2ddd
e2e2e2e22e2e2e2e1111111155555555555555556666666655555555555555555666666537555575313131313535353500000000000000001111111111111111
0000000e000000021111111105050505050505056666656655555555555555456000000656777763131313135377775300000000000000005151515171717171
02e2e2020e2e2e0e1d1d1d1d00000000000000006666666655555555555555556333333635666655313131313700007500000000000000001515151517171717
0e000e0e02000202dddddddd00000000000000006656666655555555555555556555555653555553131313135600006300000000000000005555555577777777
020202020e0e0e0edddddddd00555500000000006666666655555555544555556333333637555575313131313622206500000000000000005555555577777777
0e0e2e0e0202e202dddddddd005005000000000066666666555555555445555565555556567777631313131355eee65300000000000000005555555577777777
020000020e00000edddddddd00500500000550006666656655555555555554557666666735666655313131313522553500000000000000005555555577777777
0e2e2e2e02e2e2e2dddddddd00500500000550006666666655555555555555555777777553555553131313135322535355555555555555555555555577777777
666166669995999999000009906000606660666600000000dddd11116666666667676666ddddd11d6dddddd65555555599959999999599995555555544444444
661516664495444440445440402222206605066611010111dddd11116555555665656666dddd11116dd77dd6111100004aaaa774449544445555555544444444
615551665555555550095900508000806666666610111011dddd11116000000665656666dddd11116d7667d6111100005acccc75555555555454545447444744
155555169999959990440440908080800066606655555556dddd111160b0280665656666dddd111d6d6666d6dddd11119a333ca9999995994444444441676144
6555556644449544409565904088888065600566655555661111dddd6000000665656666d1dddddd6d5665d61111dddd4a3333a4444495444444444444777444
6655566655555555500454005088088066655666665556661111dddd6677776665656666111ddddd6dd55dd61111dddd5aaaaaa5555555554444444444161444
6665666699959999909959909020502066656666666566661111dddd66666666656566661111dddd6dddddd61111dddd92212229999599994444444444444444
6666666644954444400000004001110066666666666666661111dddd6666666660606666dd1ddddd667777661111dddd44954444449544444444444444444444
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa9111111991111111911111199111111111111119111111111111111911111111
a111111aa1111111a111111aa11111111111111a111111111111111a111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
99999999999999999111111991111111999999999999999911111119111111119999999999999999911111199111111199999999999999991111111911111111
eeeeeeee000000000000000055555555555555555555555000555555eeeee00000eeeeeeeeeeeee00000eeeeeeeeeeeeeeeee00000000eeeeeaaeeee00000000
ee0000ee000000000000000055555550005555555555550eee055555eeee02121200eeeeeeeeee02121200eeeeeeeeeeeeee0666576660eeee99eeee00000000
e07bb70e00000000000000005555550eee05555555555502e2055555eee0700212110eeeeeeee0700212110eeeee0000eeee0666666660eeee88eeee00000000
e0b77b0e000000000000000055555502e20555555555550070055555eeee0ee0000220eeeeeeee0ee0002220eee0eee70eee0777777770eeee00eeee00000000
e03bb30e000000000000000055555500700555555555550101055555eeeeeeeeeee010eeeeeeeeeeeee011200002eee7e0000555555550000000000000000000
e0b77b0e000000000000000055555501010555555555550111055555eeeeeeee0002210eeeeeeeee000222100c02eee7e0cc0555555550cccc00ccc000000000
e03bb30e000000000000000055555011111055555555501111105555eeeeeee02211220eeeeeeee02211220e0c02eee7e0cc0555555550ccc0000cc000000000
ee0000ee000000000000000055000122122100555550012222210005eeee0001122210eeeeee0001122210ee0c02eee7e0cc0066666600cc060060c000000000
0000e000000000000000000050222211111222055502221111122220eee01122212000eeeee0112221200eee0c02eee7e0ccc06655660ccc071170c000000000
0b700bb0000000000000000055000122222100205020012222210005ee0822220002220eee082222000220ee0c02eee7e0cc0665bb5660cc057750c000000000
0bb0bb300000000000000000502221eeeee12205550221eeeee12220ee02282002200020ee022820022020ee0c020000e0cc066bbbb660cc055550c000000000
0bbbb30e00000000000000005500028fef8200205020028fef820005e070202020022001e07020202020020e0c00222200cc066bbbb660cc055550c000000000
0bbbbb0e000000000000000050222122f221220555022122f2212220ee00700102002011ee0070002020101e0c02222220ccc06666660ccc100001c000000000
03b03bb0000000000000000050200070007000205020007000700020ee1101111020011eee1101020102011e0c02200220cc1100000011ccc1111cc000000000
033003b0000000000000000055011101110110205020110111011105e111111111011eeee1111110111011ee0c00000000cc1105555011ccccccccc000000000
0000e000000000000000000055551111111111055501111111111555eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0cc111111cccc11000011cccccccccc000000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeeeeb37beeeeee0cccccccccccccc1111cccccccccccc000000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeeeb3bb7beeeee0777777777777777777777777777777000000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeeb3bbbb7beeee0111111111111111111111111111111000000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeeb3bbbb7beeee0111111111111111111111111111111000000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeb3bbbbbb7beee0111111111111111111111111111111000000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeb3bbbbbb7beee0000000000000000000000000000000000000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeb3bbbbbb7beeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeb3bbbbbb7beeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeb3bbbbbb7beeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeb3bbbbbb7beeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeb3bbbbbb7beeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeb3bbbbbb7beeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeb3bbbbbb7beeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeb3bbbbbb7beeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeb3bbbbbb7beeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
000000000000000000000000000000000000000000000000000000000000000000000000eeeb3bbbbbb7beeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1eeeeeeeeeee00000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeee777777eeeeeeeeeeeeeeeeeeeeeeeee9999eeeeeeeeeeee1111eeeeeeeeeeeee1eeeeeeeeee00000000000000000000000000000000
eeeeeee00eeeeeeeeee7777777777eeeeeeeeeeeeeeeeeeeeee9aaa99eeeeeeeeee199911eeeeeeeee11eeeeeeeeeeee00000000000000000000000000000000
eeeee000000eeeeeeee7777777777eeeeeeeee9999eeeeeeee9aa7799999eeeeee199aa111111eeeee1eeeeeeeeeeeee00000000000000000000000000000000
eeee00000000eeeeee777777777777eeeeeee9aaa99eeeeeee9a79999aaa9eeeee19a111111991eeeeee1eeeeeeeeeee00000000000000000000000000000000
eeee00000000eeeeee777777777777eeeeee9aa77999eeeeee9a7999977aa9eeee19a111e11aa91eeeeeeeeee1eee1ee00000000000000000000000000000000
eee0000000000eeeee777777777777eeeeee9a799999eeeeee9999999997a9eeee1111eeeee1a91eeeeee1e11eeeeeee00000000000000000000000000000000
eee0000000000eeeee777777777777eeeeee9a799999eeeeeee999999997a9eeeee111eeeeeee91eeeeeeee11e11eeee00000000000000000000000000000000
eeee00000000eeeeee777777777777eeeeee99999999eeeeeee9a799999999eeeee1911eeeeee11eeeee1eeeee11eeee00000000000000000000000000000000
eeee00000000eeeeee777777777777eeeeeee999999eeeeeeee9a79999999eeeeee19a11eeee11eeeeeeeeeeeeeeeeee00000000000000000000000000000000
eeeee000000eeeeeeee7777777777eeeeeeeee9999eeeeeeeee9aa779999eeeeeee119aa11e11eeeee1eeee1eeeee1ee00000000000000000000000000000000
eeeeeee00eeeeeeeeee7777777777eeeeeeeeeeeeeeeeeeeeeee9aaa99eeeeeeeeee11111eeeeeeeee11eeeeeeeeeeee00000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeee777777eeeeeeeeeeeeeeeeeeeeeeeeee9999eeeeeeeeeeee111eeeeeeeeeeee1eeeeeeeeeee00000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00000000000000000000000000000000
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

__gff__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001010101018201010101010101010101828201828201010101010101000001010101010101010101010101010101010182828282828282828282828282828282
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
0000000000000000000000000000000045454545454500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
005c5c5c5c5c5c5c5c5c5c5c5c000000455151514545000000007d7d7d7d7d7d7d7d7d7d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
004e4e4e4e4e4e4e4e4e4e4e4e00000050424242524500000000636161616161616161630000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
004c4c4c4c85864c4c4c4c4c4c00000060434343524500000000656565656565656565650000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
004c3d4c4c95964c4c4d2d4c4c000000424444445245000000006460608b8c8d8e6060600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
004c4c4d4c4c4d4c4c4c074c4c000000434444445245000000006060609b9c9d9e6060600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000044444444524500000000606060abacadae6060600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000044444444524500000000606060606060606060600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444444444444444463535362444444444444524500000000606060606060606060600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444444444441635345454560444144444444524500000000606064606060606060600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444440444444615151516042404444444444524500000000606460646060606060600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444444444444424242424243444444444444524500000000606060606060606060600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444444444444434343434344444444444444524500000000606060606060606060600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444444444444444444444444444444444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444444444444444444444444446362444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444463535353536244444444446160444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444461514545456044444444444242444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444442426151604244444444444343444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444443434242424344444444444444444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444444444343434444444444444444444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
4553535353535353535353535353535353535353454500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
4545454545454545454545454545454545454545454500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
0001000025550215502355027550295502b5500000000000000000000027550000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__music__
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344
00 41424344

