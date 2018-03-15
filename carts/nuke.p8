pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
-- nuklear klone
-- by freds72
local time_t,time_dt=0,0
local before_update,after_draw={},{}
local actors={} --all actors in world
local use_mouse=0
-- level globs
local lvl_i,cur_loop,lvl=0,1
local level_cw,level_ch=64,32
-- side
local good_side,bad_side,any_side=0x1,0x2,0x0
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
-- screens
local game_screen,start_screen,cur_screen={},{}
-- collections
local all_parts
local parts={}
-- player settings
local plyr_hpmax,plyr,plyr_playing=8
local all_plyrs=json_parse('[{"strips":[[17,18,19,18,17],[17,33,34,33]]},{"strips":[[49,50,51,50,49],[49,56,57,56]],"palt":3}]')
-- camera
local pause_t=0
local shkx,shky,cam_x,cam_y=0,0
-- particles
_g.draw_spr_part=function(self,x,y)
	local sw=self.sw or 1
	palt(0,false)
	palt(self.palt or 14,true)
	local s=self.frames and self.frames[flr(self.frame)%#self.frames+1] or self.spr
	spr(s,x-4*sw,y-4*sw,sw,sw)
end
_g.draw_rspr_part=function(self,x,y)
	local s=self.frames and self.frames[flr(self.frame)%#self.frames+1] or self.spr
	local sx,sy=band(s*8,127),8*flr(s/16)
	rspr(sx,sy,x-4,y-4,1-self.angle)
end
_g.draw_txt_part=function(self,x,y)
	local l=1.5*#self.txt
	print(self.txt,x-l+1,y-2,0)
	print(self.txt,x-l,y-2,7)
end
_g.draw_blast_part=function(self,x,y)
	local s=self.frames[flr(self.frame)+1]
	x-=8
	y-=8
	palt(0,false)
	palt(14,true)
	spr(s,x,y,1,1)
	spr(s,x+8,y,1,1,true)
	spr(s,x,y+8,1,1,false,true)
	spr(s,x+8,y+8,1,1,true,true)
	palt()
end
_g.update_blast_part=function(self)
	if flr(self.frame)==#self.frames-1 then
		return false
	end
	if self.frame==0 then
		pause_t=8
		cam_shake(rnd(),rnd(),5)
	end
	self.frame=self.frame+0.25
	if self.frame>2 then
		for _,a in pairs(actors) do
			local dx,dy=mid(self.x,a.x-a.w,a.x+a.w)-self.x,mid(self.y,a.y-a.h,a.y+a.h)-self.y
			if a.hit_t<time_t and abs(dx)<2 and abs(dy)<2 then
 			local r=dx*dx+dy*dy
 			if r<4 then
 				r=1-smoothstep(r/4)
 				local u,v=normalize(dx,dy,0.5*r)
 				a.dx+=u
 				a.dy+=v
 				a:hit(flr(8*r)+1)
 			end
 		end
		end
	end
	if self.frame==3 then
		for i=1,8 do
			local a=rnd()
			make_part(self.x,self.y,0,all_parts.blast_smoke,cos(a)/8,sin(a)/8)
		end
		make_splat(self)
	end
	zbuf_write(self)
	return true
end
_g.update_part=function(self)
	if(self.t<time_t or self.r<0) return false
	if bor(self.dx,self.y)!=0 then
		if solid(self.x+self.dx,self.y) then
			self.dx=-self.dx
		end
		if solid(self.x,self.y+self.dy) then
			self.dy=-self.dy
		end
	end
	self.x+=self.dx
	self.y+=self.dy
	self.z+=self.dz
	self.dx=amortize(self.dx,self.inertia)
	self.dy=amortize(self.dy,self.inertia)
	self.dz=amortize(self.dz,self.inertia)
	self.r+=self.dr
	self.frame+=self.df
	zbuf_write(self)
	return true
end

_g.draw_part=function(self,x,y)
 circfill(x,y,8*self.r,self.c)
end

all_parts=json_parse('{"part_cls":{"inertia":1,"r":1,"dr":0,"frame":0,"df":0.01,"draw":"draw_part","update":"update_part"},"flash":{"dly":8,"r":0.8,"c":7,"dr":-0.1},"blood_splat":{"base_cls":"chunk_base","spr":129},"head":{"base_cls":"chunk_base","spr":201},"turret_splat":{"base_cls":"chunk_base","spr":165,"sw":2,"sh":2},"goo_splat":{"base_cls":"chunk_base","spr":130},"fart":{"dy":-0.05,"rnd":{"r":[0.05,0.2],"dly":[24,32],"c":[11,3,true]}},"laser_spark":{"sfx":37,"zorder":3,"dx":0,"dy":0.04,"c":7,"rnd":{"r":[0.1,0.2],"dly":[24,32]}},"hit":{"dr":-0.02,"rnd":{"r":[0.3,0.4],"dly":[8,12],"c":[9,10,true]}},"blast_smoke":{"inertia":0.95,"dr":-0.03,"rnd":{"r":[0.8,1.2],"dly":[15,30]},"c":1},"slash":{"frames":[196,197,198],"draw":"draw_rspr_part","dly":12},"candle":{"w":0.1,"h":0.1,"inertia":0.9,"rnd":{"c":[8,9,10],"r":[0.1,0.2],"dr":[-0.01,-0.02],"dz":[0.04,0.06],"dly":[12,24]}},"bones":{"base_cls":"chunk_base","rnd":{"spr":[202,203,204]}},"goo_chunks":{"base_cls":"chunk_base","rnd":{"spr":[199,200,199]}},"green_chunks":{"base_cls":"chunk_base","rnd":{"spr":[215,216,215]}},"fireimp_chunks":{"base_cls":"chunk_base","rnd":{"spr":[219,220,220]}},"notice":{"zorder":3,"inertia":0.91,"dly":72,"draw":"draw_txt_part"},"blast_splat":{"base_cls":"chunk_base","frames":[212,213,214],"df":0.20},"blast_chunks":{"base_cls":"chunk_base","rnd":{"spr":[217,218,217]}},"blast":{"sfx":51,"w":1,"h":1,"dly":30,"acc":0,"inertia":0,"frames":[192,193,208,209,194,195,210,211],"rnd":{"bones_c":[2,4]},"update":"update_blast_part","draw":"draw_blast_part","splat":"blast_splat","bones":"blast_chunks"},"chunk_base":{"zorder":1,"inertia":0.85,"r":1,"dr":0,"frame":0,"df":0.01,"rnd":{"dly":[600,900]},"draw":"draw_spr_part","update":"update_part"}}')

-- weapons catalog
local all_loot={}

_g.draw_zap=function(self,x,y)
	local x0,y0,x1,y1=x,y,cam_project(self.prevx,self.prevy)
	local dx,dy=shr(x1-x,2),shr(y1-y,2)
	for i=1,8 do
		circfill(x,y,1,12)
		x+=dx
		y+=dy
	end
	line(x0,y0,x,y,7)
end
_g.draw_laser=function(self,x,y)
	local x1,y1=cam_project(0,self.y1)
	local w=self.w-2*rnd()
	rectfill(x-w-2,y+5,x+w+2,y1,2)
	rectfill(x-w,y+3,x+w,y1,8)
	rectfill(x-w/4,y,x+w/4,y1,7)
	circfill(x,y,2*w,7)
end
_g.update_laser=function(self)
	if self.t>time_t then
		if(not self.dw) self.dw=0
		self.dw+=1
		self.w=lerp(0.5,5,smoothstep(self.dw/54))
		local x0,y0,y1=self.x,self.y,self.y1 or self.y
		y1+=self.dy
		if circline_coll(plyr.x,plyr.y,plyr.w,x0,y0,x0,y1,self.w/8) then
			plyr:hit(self.wp.dmg)
			plyr.dy+=self.dy/2
			self.y1=plyr.y
			make_part(plyr.x,plyr.y,0.25,all_parts.hit,0,1.5*self.dy)
		elseif not solid(x0,y1) then
			self.y1=y1
		end
		
		make_part(x0+self.w*(rnd(2)-1)/16,lerp(y0,self.y1,rnd()),0,all_parts.laser_spark)

		zbuf_write(self)
		return true
	end
	return false
end
local weapons=json_parse('{"base_gun":{"sfx":55,"frames":[42],"dmg":1,"spread":0.05,"v":0.1,"ttl":[90,100],"dly":32},"goo":{"frames":[63],"dmg":1,"spread":1,"v":0,"ttl":[120,300],"dly":64,"zorder":1},"acid_gun":{"sfx":49,"frames":[26,27],"blts":3,"bounce":0.9,"spread":0.2,"dmg":3,"v":0.1,"xy":[1,0],"ttl":[160,200],"dly":24},"uzi":{"n":"uzi","sfx":63,"icon":21,"sx":32,"sy":8,"frames":[10,11],"spread":0.04,"dmg":1,"v":0.4,"ttl":[15,24],"dly":5,"ammo":75,"shk_pow":2,"cost":1},"minigun":{"n":"minigun","sfx":55,"icon":25,"sx":64,"sy":8,"frames":[10,11],"spread":0.04,"dmg":2,"v":0.45,"ttl":[25,35],"dly":3,"ammo":250,"shk_pow":2,"cost":4},"shotgun":{"n":"pump","side":"good_side","icon":37,"sx":32,"sy":16,"frames":[10],"spread":0.05,"blts":3,"dmg":3,"inertia":0.97,"v":0.35,"bounce":1,"ttl":[32,48],"dly":56,"ammo":33,"shk_pow":2,"cost":3},"glock":{"n":"g.lock","icon":53,"sfx":50,"sx":32,"sy":24,"frames":[10,11],"spread":0.01,"dmg":4,"v":0.5,"ttl":[30,30],"dly":32,"ammo":17,"shk_pow":2,"cost":2},"rpg":{"n":"rpg","dmg":0,"icon":23,"sx":48,"sy":8,"spr":58,"spread":0.02,"v":0.2,"inertia":1.01,"blast_on_die":true,"ttl":[32,48],"dly":72,"ammo":8,"shk_pow":3,"cost":5,"draw":"draw_rspr_part"},"grenade":{"n":"grenade","icon":55,"sx":48,"sy":24,"dmg":0,"frames":[44],"spread":0.02,"v":0.2,"inertia":0.98,"bounce":1,"blast_on_die":true,"ttl":[60,70],"dly":72,"ammo":12,"shk_pow":2.1,"cost":4},"mega_gun":{"sx":48,"sy":8,"frames":[43,28],"sfx":52,"dmg":5,"spread":0.05,"v":0.1,"ttl":[50,55],"dly":32,"sub_cls":"mega_sub","emitters":5},"mega_sub":{"sx":48,"sy":8,"frames":[26,27],"dmg":5,"spread":0,"v":0.1,"ttl":[900,900],"dly":12,"burst":4},"rifle":{"sfx":50,"sx":64,"sy":16,"frames":[10,11],"dmg":5,"spread":0,"v":0.5,"ttl":[90,90],"dly":80,"sight":true},"laser":{"zorder":3,"sfx":36,"dmg":0.5,"dly":60,"v":1,"dx":0,"dy":1,"spread":0,"ttl":[90,90],"draw":"draw_laser","update":"update_laser"},"bite":{"sfx":37,"dmg":1,"dly":30,"spread":0.02,"v":0.1,"draw":"nop","ttl":[4,4],"hit_part":"slash"},"snowball":{"frames":[60],"dmg":1,"spread":0.01,"v":0.5,"inertia":0.9,"ttl":[70,90],"dly":80},"horror_spwn":{"actor_cls":"horror_cls","spread":1,"v":0.2,"dly":145,"ammo":5},"zapper":{"n":"laser","cost":5,"side":"any_side","bounce":1,"ammo":30,"sfx":53,"sx":48,"sy":16,"icon":39,"dmg":5,"spread":0.01,"v":0.6,"ttl":[90,100],"dly":12,"draw":"draw_zap"},"turret_minigun":{"sfx":55,"frames":[10,11],"spread":0.25,"dmg":1,"v":0.1,"ttl":[60,80],"dly":8,"blts":5},"radiation":{"frames":[12],"spread":0.1,"dmg":3,"v":0.1,"inertia":0.985,"sfx":52,"blts":3,"ttl":[200,240],"dly":120},"cop_spwn":{"actor_cls":"cop_cls","spread":1,"v":0.1,"dly":145,"ammo":4}}')
local max_cost=-1
for k,v in pairs(weapons) do
	_g[k]=v
	if v.cost then
		all_loot[v.cost]=all_loot[v.cost] or {}
		max_cost=max(max_cost,add(all_loot[v.cost],v).cost)
	end
end

-- flashlight shader
local shade={}
function scol(i)
	return sget(88+2*flr(i/8)+1,24+i%8)
end
for i=0,15 do
	local c1=scol(i)
	for j=0,15 do
		shade[bor(i,shl(j,4))]=bor(c1,shl(scol(j),4))
	end
end
local lights=json_parse("[[[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[27],[25],[24],[23],[22],[21],[20],[19],[19,28],[18,26],[17,25],[17,24],[16,23],[16,22],[15,22],[15,21,28],[15,20,27],[14,20,25],[14,19,25],[14,19,24],[13,18,23],[13,18,23],[13,18,22],[13,17,22],[12,17,21],[12,17,21],[12,16,20],[12,16,20],[12,16,20],[11,16,19],[11,16,19],[11,15,19],[11,15,19],[11,15,19],[11,15,19],[11,15,18],[11,15,18],[11,15,18],[11,15,18],[11,15,18],[11,15,18],[10,15,18],[11,15,18],[11,15,18],[11,15,18],[11,15,18],[11,15,18],[11,15,18],[11,15,19],[11,15,19],[11,15,19],[11,15,19],[11,16,19],[11,16,19],[12,16,20],[12,16,20],[12,16,20],[12,17,21],[12,17,21],[13,17,22],[13,18,22],[13,18,23],[13,18,23],[14,19,24],[14,19,25],[14,20,25],[15,20,27],[15,21,28],[15,22],[16,22],[16,23],[17,24],[17,25],[18,26],[19,28],[19],[20],[21],[22],[23],[24],[25],[27],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]],[[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[27],[25],[24],[22],[21],[21],[20],[19],[18,29],[18,27],[17,25],[17,24],[16,23],[16,22],[15,22],[15,21,29],[14,20,27],[14,20,26],[14,19,25],[13,19,24],[13,18,23],[13,18,23],[12,18,22],[12,17,22],[12,17,21],[12,17,21],[12,16,20],[11,16,20],[11,16,20],[11,16,19],[11,15,19],[11,15,19],[11,15,19],[10,15,18],[10,15,18],[10,15,18],[10,15,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,15,18],[10,15,18],[10,15,18],[10,15,18],[11,15,19],[11,15,19],[11,15,19],[11,16,19],[11,16,20],[11,16,20],[12,16,20],[12,17,21],[12,17,21],[12,17,22],[12,18,22],[13,18,23],[13,18,23],[13,19,24],[14,19,25],[14,20,26],[14,20,27],[15,21,29],[15,22],[16,22],[16,23],[17,24],[17,25],[18,27],[18,29],[19],[20],[21],[21],[22],[24],[25],[27],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]],[[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[27],[25],[24],[22],[21],[20],[20],[19],[18,30],[18,27],[17,25],[16,24],[16,23],[15,22],[15,22],[15,21,30],[14,20,28],[14,20,26],[13,19,25],[13,19,24],[13,18,23],[12,18,23],[12,17,22],[12,17,22],[12,17,21],[11,16,21],[11,16,20],[11,16,20],[11,16,20],[11,15,19],[10,15,19],[10,15,19],[10,15,19],[10,15,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,17],[10,14,17],[9,14,17],[10,14,17],[10,14,17],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,14,18],[10,15,18],[10,15,19],[10,15,19],[10,15,19],[11,15,19],[11,16,20],[11,16,20],[11,16,20],[11,16,21],[12,17,21],[12,17,22],[12,17,22],[12,18,23],[13,18,23],[13,19,24],[13,19,25],[14,20,26],[14,20,28],[15,21,30],[15,22],[15,22],[16,23],[16,24],[17,25],[18,27],[18,30],[19],[20],[20],[21],[22],[24],[25],[27],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]]]")

_g.darken=function()
	local m,r=0x6000,flr(rnd(#lights))+1
	for y=1,128 do
		local l=lights[r][y]
		local x0,x1,x2=l[1] or 31,l[2] or 31,l[3] or 31
		memset(m,0,x0+1)
		memset(m+63-x0,0,x0+1)
		for x=x0+1,x1 do
			poke(m+x,shade[shade[peek(m+x)]])
			poke(m+63-x,shade[shade[peek(m+63-x)]])
		end
		for x=x1+1,x2 do
			poke(m+x,shade[peek(m+x)])
			poke(m+63-x,shade[peek(m+63-x)])
		end
		m+=64
	end
end
-- levels
local active_actors
local levels=json_parse('[{"n":"desert","floors":[68,64,65,67,111],"walls":[66],"shadow":110,"bkg_col":1,"w":[8,12],"h":[6,8],"paths":[1,3],"path":{"w":[3,4],"len":[8,12]},"spawn":[[8,12,"bandit_cls"],[5,8,"worm_cls"],[-5,-3,"scorpion_cls"],[2,3,"cactus"],[-9,-5,"cop_box_cls"]]},{"n":"sewers","shader":"darken","floors":[86,87,87,88],"walls":[90,89,91],"shadow":94,"borders":[10,11,3],"bkg_col":3,"w":[2,3],"h":[2,3],"paths":[2,4],"path":{"w":[1,2],"len":[10,12]},"spawn":[[10,15,"slime_cls"],[5,10,"barrel_cls"],[-4,-2,"frog_cls"]]},{"n":"snow plains","cursor":93,"floors":[70,71,72,75],"walls":[74],"shadow":95,"borders":[5,1,7],"bkg_col":7,"w":[6,8],"h":[5,6],"paths":[2,3],"path":{"w":[3,5],"len":[10,12]},"spawn":[[8,10,"dog_cls"],[5,8,"bear_cls"],[-2,-1,"turret_cls"]]},{"n":"lab","floors":[102,105],"walls":[103,104,106],"shadow":107,"borders":[6,7,5],"bkg_col":5,"shader":"darken","w":[4,6],"h":[3,5],"paths":[1,4],"path":{"w":[1,2],"len":[8,12]},"spawn":[[3,4,"cop_cls"],[5,8,"fireimp_cls"],[5,8,"barrel_cls"]]},{"n":"palace","floors":[96,100],"walls":[97,98,99,108],"shadow":101,"borders":[7,0,5],"bkg_col":5,"w":[8,10],"h":[8,10],"paths":[1,3],"path":{"w":[2,3],"len":[10,12]},"spawn":[[4,8,"horror_cls"],[4,4,"horror_spwnr_cls"],[-4,-2,"slime_cls"],[2,3,"candle_cls"]]},{"n":"throne","music":0,"builtin":true,"bkg_col":0,"borders":[7,0,5],"cx":103,"cy":0,"cw":13,"ch":31,"plyr_pos":[110,28],"spawn":[{"a":"throne_cls","x":112,"y":6},{"a":"ammo_cls","x":106,"y":27},{"a":"ammo_cls","x":107,"y":27},{"a":"ammo_cls","x":106,"y":28},{"a":"ammo_cls","x":107,"y":28},{"a":"health_cls","x":114,"y":27},{"a":"health_cls","x":115,"y":27},{"a":"health_cls","x":114,"y":28},{"a":"health_cls","x":115,"y":28}]}]')

local face1strip=json_parse('[false,false,false,true,true,true,false,false]')

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
-- print text helper
local txt_offsets=json_parse("[[-1,0],[0,-1],[0,1],[-1,-1],[1,1],[-1,1],[1,-1]]")
local txt_center,txt_shade,txt_border=false,-1,false
function txt_options(c,s,b)
	txt_center=c or false
	txt_shade=s or -1
	txt_border=b or false
end
function txt_print(str,x,y,col)
	if txt_center then
		x-=flr((4*#str)/2+0.5)
	end
	if txt_shade!=-1 then	
		print(str,x+1,y,txt_shade)
		if txt_border then
			for _,v in pairs(txt_offsets) do
				print(str,x+v[1],y+v[2],txt_shade)
			end
		end
	end
	print(str,x,y,col)
end

-- helpers
function pop(a)
	if #a>0 then
		local p=a[#a]
		a[#a]=nil
		return p
	end
end
-- calls 'fn' method on all elements of a[]
-- pairs allows add/remove while iterating
function forall(a,fn)
	for _,v in pairs(a) do
		if not v[fn](v) then
			del(a,v)
		end
	end
end
function clone(src,dst)
	-- safety checks
	--if(src==dst) assert()
	--if(type(src)!="table") assert()
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
function amortize(x,dx)
	x*=dx
	return abs(x)<0.001 and 0 or x
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
function rotate(a,p)
	local c,s=cos(a),-sin(a)
	return {p[1]*c-p[2]*s,p[1]*s+p[2]*c}
end
function rspr(sx,sy,x,y,a)
	local ca,sa=cos(a),sin(a)
	local ddx0,ddy0,srcx,srcy=ca,sa
	ca*=4
	sa*=4
	local dx0,dy0,c=sa-ca+4,-ca-sa+4
	for ix=0,7 do
		srcx,srcy=dx0,dy0
		for iy=0,7 do
			-- fast boundary check
			if band(bor(srcx,srcy),0xfff8)==0 then
			 c=sget(sx+srcx,sy+srcy)			
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
function sqr_dist(x0,y0,x1,y1)
	local dx,dy=x1-x0,y1-y0
	if abs(dx)>128 or abs(dy)>128 then
		return 32000
	end
	return dx*dx+dy*dy
end

function normalize(u,v,scale)
	scale=scale or 1
	local d=sqrt(u*u+v*v)
	if (d>0) u/=d v/=d
	return u*scale,v*scale
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
-- circle/thick line collision
function circline_coll(x,y,r,x0,y0,x1,y1,w)
	local dx,dy=x1-x0,y1-y0
	local ax,ay=x-x0,y-y0
	local t,d=ax*dx+ay*dy,dx*dx+dy*dy
	if d==0 then
		t=0
	else
		t=mid(t,0,d)
		t/=d
	end
	local ix,iy=x0+t*dx-x,y0+t*dy-y
	r+=(w or 0.2)
	return ix*ix+iy*iy<r*r
end

-- zbuffer
local drawables={}
function zbuf_clear()
	drawables={}
end
function zbuf_write(obj)	
	add(drawables,obj)
end
function zbuf_draw()
	local zbuf={{},{},{}}
	local zdraw,zbuf_zmin,zbuf_zmax={},256,-128
	for _,obj in pairs(drawables) do
 	local xe,ye=cam_project(obj.x,obj.y)
 	local ze=obj.z and 8*obj.z or 0
 	local zi=obj.zorder or 2
 	obj=add(zbuf[zi],{obj=obj,x=xe,y=ye-ze,key=ye+ze})
 	if zi==2 then
 		local z=flr(obj.key)
 		zbuf_zmin,zbuf_zmax=min(zbuf_zmin,z),max(zbuf_zmax,z)
 		local zb=zdraw[z] or {}
 		add(zb,obj)
 		zdraw[z]=zb
 	end
	end
	
	for _,v in pairs(zbuf[1]) do
		v.obj:draw(v.x,v.y)
	end
	for i=max(-16,zbuf_zmin),min(144,zbuf_zmax) do
		local zb=zdraw[i]
		if zb then
			for _,v in pairs(zb) do
				v.obj:draw(v.x,v.y)
			end
		end
	end
	for _,v in pairs(zbuf[3]) do
		v.obj:draw(v.x,v.y)
	end
end

-- collision map
-- provide o(1) lookup for proximity checks
local cmap={}
local cmap_cells=json_parse('[0,1,129,128,127,-1,-129,-128,-127]')
function cmap_op(obj,fn)
	if bor(obj.w,obj.h)!=0 then
		for x=flr(obj.x-obj.w),flr(obj.x+obj.w) do
			for y=flr(obj.y-obj.h),flr(obj.y+obj.h) do
				fn(obj,cmap,x+128*y)
			end
		end
	end
end
function cmap_add(obj,cmap,h)
	cmap[h]=cmap[h] or {}
	add(cmap[h],obj)
end
function cmap_del(obj,cmap,h)
	if cmap[h] then
		del(cmap[h],obj)
		-- remove empty sets
		if #cmap[h]==0 then
			cmap[h]=nil
		end
	end
end
local cmap_session,cmap_i,cmap_cell,cmap_h,cmap_side=0
-- creates a nearby iterator
-- filters by side
-- warning: not reentrant
function cmap_iterator(x,y,side)
	cmap_i,cmap_cell,cmap_side=1,1,side or any_side
	cmap_h=flr(x)+128*flr(y)
	cmap_session+=1
end
function cmap_next()
	while(cmap_cell<=9) do
		local h=cmap_h+cmap_cells[cmap_cell]
		local objs=cmap[h]
		if objs and cmap_i<=#objs then
			local obj=objs[cmap_i]
			cmap_i+=1
			if obj.cmap_session!=cmap_session and band(obj.side,cmap_side)==0 then
				return obj
			end
			obj.cmap_session=cmap_session
		end
		cmap_i=1
		cmap_cell+=1
	end
	return nil
end

-- camera
function cam_shake(u,v,pow)
	shkx,shky=min(4,shkx+pow*u),min(4,shky+pow*v)	
end
function cam_update()
	shkx*=-0.7-rnd(0.2)
	shky*=-0.7-rnd(0.2)
	if abs(shkx)<0.5 and abs(shky)<0.5 then
		shkx,shky=0,0
	end
	camera(shkx,shky)
end
function cam_track(x,y)
	-- fixed woobly actors
	cam_x,cam_y=flr(8*x)-4,flr(8*y)-4
end
function cam_project(x,y)
	return 64+8*x-cam_x,64+8*y-cam_y
end

-- particle
function make_part(x,y,z,src,dx,dy,dz,a)
	local p=clone(all_parts[src.base_cls or "part_cls"],
		clone(src,{
			x=x,
			y=y,
			z=z,
			dx=dx or 0,
			dy=dy or 0,
			dz=dz or 0,
			angle=a or 0}))
 if(p.sfx) sfx(p.sfx)
	p.t=time_t+p.dly
	return add(parts,p)
end
-- spill bones and skull!
function make_splat(self)
	make_part(self.x,self.y,0, all_parts[self.splat or "blood_splat"])
 for i=1,self.bones_c do
		local a=rnd()
		make_part(self.x+rndlerp(-self.w,self.w),self.y+rndlerp(-self.h,self.h),0,all_parts[self.bones or "bones"],cos(a)/10+self.dx,sin(a)/10+self.dy,0,a)
	end
end
function draw_lasersight_part(p,x,y)
	local dx,dy=p.u,p.v
	line(x+2*dx,y+2*dy,x+80*dx,y+80*dy,8)
end
-- bullets
function update_blt(self)
	local x1,y1=self.x,self.y
	if self.t>time_t then
		local x0,y0=self.x,self.y
		x1,y1=x0+self.dx,y0+self.dy
		if self.wp.inertia then
			self.dx*=self.wp.inertia
			self.dy*=self.wp.inertia
		end

		-- actors hit?
		-- note: will be wrong for very fast bullets
		cmap_iterator(x1,y1,self.side)
		local a=cmap_next()
		while a do
			if circline_coll(a.x,a.y,a.w,x0,y0,x1,y1) then
				-- law of conservation!
				if a.acc!=0 then
					a.dx+=self.dx
					a.dy+=self.dy
				end
				a:hit(self.wp.dmg+cur_loop-1)
				goto die
			end
			a=cmap_next()
		end

		local touch,bounce=false,self.bounce or 0
		if solid(x1,y0) then
			x1=x0
			self.dx*=-bounce
			self.u=-self.u
			touch=true
		end
		if solid(x0,y1) then
			y1=y0
			self.dy*=-bounce
			self.v=-self.v
			touch=true
		end

		if touch then
		 if self.bounce then
				self.side=self.wp.side
				make_part(x1,y1,0.25,all_parts.flash)
				sfx(self.wp.bounce_sfx or 58)
			else
				goto die
			end
		end
		self.prevx,self.prevy,self.x,self.y=x0,y0,x1,y1
		zbuf_write(self)
		return true
	end
	
	::die::
	if self.wp.blast_on_die then
		make_part(x1,y1,0,all_parts["blast"])
	else
		make_part(x1,y1,0.25,all_parts[self.wp.hit_part or "hit"],self.dx/4,self.dy/4,0,self.angle)
	end
	-- sub bullet?
	local wp=self.wp.sub_cls
	if wp then
		wp=weapons[wp]
		local side,n=self.side,self.wp.emitters
		futures_add(function()
			local ang,dang=0,1/n
			for k=1,wp.burst do
				ang=0
				for i=1,n do
					make_blt({
						x=x1,y=y1,
						side=side,
						angle=ang},wp)
					ang+=dang
				end
				wait_async(wp.dly)
			end
		end)
	end
	return false
end
function make_blt(a,wp)
	local n=wp.blts or 1
	local ang,da
	if n==1 then
		ang,da=a.angle+wp.spread*(rnd(2)-1),0
	else
		ang,da=a.angle-wp.spread/n,wp.spread/n
	end
	for i=1,n do
		if a.ammo then
			if a==plyr and a.ammo<=0 then
				sfx(57)
				return
			end
			a.ammo-=1
		end
		if wp.sfx then
			sfx(wp.sfx)
		end
		local u,v=cos(ang),sin(ang)
		local x,y=a.x+0.5*u,a.y+0.5*v
		local b={
			u=u,v=v,
			dx=wp.v*u,dy=wp.v*v,
			side=a.side,
			angle=ang,
			facing=flr(8*(ang%1))
		}
		if wp.actor_cls then
			make_actor(x,y,
				clone(all_actors[wp.actor_cls],b))
		else
			clone({
				x=x,y=y,
				wp=wp,
				bounce=wp.bounce,
				zorder=wp.zorder,
				side=a.side,
				-- weapon ttl is a range
				t=time_t+lerp(wp.ttl[1],wp.ttl[2],rnd()),
				-- for fast collision
				prevx=b.x,prevy=b.y,
				spr=wp.spr,
				update=wp.update or update_blt,
				draw=wp.draw or draw_blt},b)
			add(parts,b)
		end
		-- muzzle flash
		if(i==1) make_part(x,y+0.5,0.5,all_parts.flash)
		ang+=da
	end
end
function draw_blt(b,x,y)
	palt(0,false)
	palt(14,true)
	local frames=b.wp.frames
	if #frames==2 then
		local px,py=x-2*b.u,y-2*b.v
		spr(frames[2],px-4,py-4)
	end
	spr(frames[1],x-4,y-4)
end

-- map
local rooms,pos2roomidx
local tile_sides=json_parse('[[0,0],[1,0],[0,1],[-1,0],[0,-1]]')

function make_level()
	-- spawn entities
	active_actors=0
	lvl=levels[lvl_i]
	if lvl.builtin then
		for s in all(lvl.spawn) do
			make_actor(s.x,s.y,all_actors[s.a])
		end
	else
		-- create enough rooms
	 while make_rooms()<7 do
	 end
		for sp in all(lvl.spawn) do
			-- todo: log progression vs linear?
			local n=min(rndrng(sp)+cur_loop*cur_loop,15)
			for i=1,n do
				local r=rooms[flr(rnd()*#rooms)+1]
				local x,y=r.x+rndlerp(1,r.w-1),r.y+rndlerp(1,r.h-1)
				make_actor(x,y,all_actors[sp[3]])
			end
		end
	end
end
function make_rooms()
	rooms={}
	pos2roomidx={}
	for i=0,level_ch-1 do
		memset(0x2000+i*128,127,level_cw-1)
	end
	local cx,cy=level_cw/2,level_ch/2
	make_room(
			cx,cy,0,13)
	make_walls(0,level_cw-1,0,level_ch-2,true)
	return #rooms
end
function whereami(a)
	return pos2roomidx[flr(a.x)+shl(flr(a.y),8)] or 1
end
function ftile(cx,cy)
	local c=0
	for i=0,#tile_sides-1 do
		local p=tile_sides[i+1]
		local s=mget(cx+p[1],cy+p[2])
		if s==0 or fget(s,7) then
			c=bor(c,shl(1,i))
		end
	end
	return c
end

function make_walls(x0,x1,y0,y1,shadow)
	local tf,t
	local walls={}
	for i=x0,x1 do
		for j=y0,y1 do
			-- borders
			tf=ftile(i,j)
			if band(tf,1)!=0 then
				tf=shr(band(tf,0xfffe),1)
				t=112+tf
				mset(i,j,t)
				-- south not solid?
				if band(tf,0x2)==0 then
					if rnd()<0.8 then
						t=lvl.walls[1]
					else
						t=rndarray(lvl.walls)
					end
					add(walls,{i,j+1,t})
				end
			end
		end
	end
	for w in all(walls) do
		mset(w[1],w[2],w[3])
		if(shadow)mset(w[1],w[2]+1,lvl.shadow)
	end
end

function make_room(x,y,a,ttl)
	if(ttl<0) return
	if rnd()>0.5 then
		local wl=rotate(a,{rndrng(lvl.w),rndrng(lvl.h)})
		local r={
			x=x-wl[1]/2,y=y-wl[2]/2,
			w=wl[1],h=wl[2]}
		r=dig(r,#rooms+1)
		if r then
			add(rooms,r)
		end
	end
	local n,arnd=rndrng(lvl.paths),flr(rnd(3))
	local angles={-0.25,0,0.25}
	for i=1,n do
		local a1=a+angles[(arnd+i)%#angles+1]
		make_path(x,y,a1,ttl-1)
	end
end
function make_path(x,y,a,ttl)
	-- rotate
	local wl=rotate(a,{rndrng(lvl.path.len),
		rndrng(lvl.path.w)})
	local c={
		x=x,y=y,
		w=wl[1],h=wl[2]}
	c=dig(c)
	-- stop invalid paths
	if c then
		local u=rotate(a,{1,0})
		make_room(
			x+u[1]*c.w,y+u[2]*c.h,
			a,ttl-1)
	end
end
function dig(r,idx)
	local cw,ch=level_cw-2,level_ch-3
	local x0,y0=mid(r.x,1,cw),mid(r.y,1,ch)
	local x1,y1=mid(r.x+r.w,1,cw),mid(r.y+r.h,1,ch)
	x0,x1=flr(min(x0,x1)),flr(max(x0,x1))
	y0,y1=flr(min(y0,y1)),flr(max(y0,y1))
	cw,ch=x1-x0,y1-y0
	if cw>0 and ch>0 then
		for i=x0,x1 do
			for j=y0,y1 do
				if rnd()<0.9 then
					mset(i,j,lvl.floors[1])
				else
					mset(i,j,rndarray(lvl.floors))
				end
				if(idx) pos2roomidx[i+shl(j,8)]=idx
			end
		end
		return {x=x0,y=y0,w=cw,h=ch}
	end
end

function solid(x, y)
 return fget(mget(x,y),7)
end

function solid_area(a,dx,dy)
	local x,y,w,h=a.x+dx,a.y+dy,a.w,a.h
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

	if(dx==0 and dy==0) return true,0
	
	if dx>=dy then
		error=dy-dx/2
 	while x1!=x2 do
   if (error>0) or (error==0 and ix>0) then
	   error-=dx
 	  y1+=iy
			end

 	 error+=dy
 	 x1+=ix
 	 dist-=1
 	 if(dist<0) return false,-1
	if(solid(x1,y1)) return false,dist
 	end
	else
 	error=dx-dy/2

 	while y1!=y2 do
  	if (error>0) or (error==0 and iy>0) then
  	 error-=dy
  	 x1+=ix
		 end
	
  	error+=dx
  	y1+=iy
			dist-=1
		 if(dist<0) return false,-1
	 	if(solid(x1,y1)) return false,dist
 	end
 end
	return true,dist
end
-- true if a will hit another
-- actor after moving dx,dy
function solid_actor(a,dx,dy)
	cmap_iterator(a.x+dx,a.y+dy,a.w,a.w)
	local a2=cmap_next()
	while a2 do
  if a2!=a then
   local x,y=(a.x+dx)-a2.x,(a.y+dy)-a2.y
   if abs(x)<(a.w+a2.w)/2 and
      abs(y)<(a.h+a2.h)/2
   then 
    -- collision damage?
    if a2.dmg and a.contact_t<time_t and band(a.side,a2.side)==0 then
    	-- avoid repeated collision
    	a.contact_t=time_t+30
    	a:hit(a2.dmg)
    end
    
    if dx!=0 and abs(x) <
	abs(a.x-a2.x) then
     local v=a.dx+a2.dy
     a.dx=v/2
     a2.dx=v/2
     return true 
    end
    
    if dy!=0 and abs(y) <
	abs(a.y-a2.y) then
     local v=a.dy+a2.dy
     a.dy=v/2
     a2.dy=v/2
     return true 
    end    
   end
  end
	a2=cmap_next()
 end
 return false
end

-- checks both walls and actors
function solid_a(a, dx, dy)
	return solid_area(a,dx,dy) or solid_actor(a,dx,dy)
end

-- custom actors
local actor_id=0
function press_start()
	return btnp(4) or btnp(5)
end
function plyr_die(self)
	make_splat(self)
	futures_add(function()
		plyr_playing=false
		local t=0
		while not press_start() do
			local j=48*smoothstep(t/90)
			rectfill(0,0,127,j,0)
			rectfill(0,127,127,128-j,0)
			if t==90 then
				txt_options(true,2,true)
				txt_print("game over",64,32,14)
				txt_print(cur_loop.."-"..lvl_i,64,96,14)
			end
			t=min(t+time_dt,90)
			yield()
		end
		cur_screen=start_screen
	end,after_draw)
end

_g.die_actor=function(self)
	make_splat(self)
	-- last actor?
	if self.npc then
		active_actors-=1
		if active_actors==0 then
			-- create portal
			make_actor(self.x,self.y,all_actors.warp_cls)
			return
		end
		local r=rnd()
		if r>0.7 then
			local wp=rndarray(all_loot[flr(rnd(min(max_cost,lvl_i+cur_loop)))+1])
		make_actor(self.x,self.y,
			clone(all_actors.wpdrop_cls,{
				drop=wp,
				ammo=wp.ammo,
				spr=wp.icon,
				txt=wp.n}))
		elseif r>0.6 or plyr.ammo<2 then
			make_actor(self.x,self.y,all_actors.ammo_cls)
		elseif r>0.4 and plyr.hp!=plyr_hpmax then
			make_actor(self.x,self.y,all_actors.health_cls)
		end
	end
end

_g.hit_actor=function(self,dmg)
	self.hit_t=time_t+8
	self.hp-=dmg
	sfx(61)
	if not self.disable and flr(self.hp)<=0 then
		self.disable=true
		self:die()
		cmap_op(self,cmap_del)
		del(actors,self)
	end
end

-- a-star
local a_sides=json_parse('[[1,0],[0,1],[-1,0],[0,-1]]')
function closest(x,y,nodes)
	local score,node=32000
	for _,v in pairs(nodes) do
		local vscore=sqr_dist(v.x,v.y,x,y)
		if vscore<score then
			node,score=v,vscore
		end
	end
	return node
end
function update_path_async(self)
::seek::
	while self.hp>0 do
		local x1,y1
		if self.flee then
			local pr,cr=whereami(plyr),whereami(self)
			local r=rooms[flr(16*pr+8*cr+self.id)%#rooms+1]
			x1,y1=rndlerp(r.x,r.x+r.w),rndlerp(r.y,r.y+r.h)
		else
			x1,y1=plyr.x,plyr.y
			-- avoid all actors moving to player at once!
			if sqr_dist(x1,y1,self.x,self.y)>96 then
				yield()
				goto seek
			end
		end
	
	 local x,y=self.x,self.y
		local k,pk=flr(x)+96*flr(y),flr(x1)+96*flr(y1)
		local flood,flood_len={[k]={x=x,y=y,k=k}},1
		local closedset,camefrom,current={},{}

		-- a* (+keep cpu/memory limits)
		while flood_len>0 and flood_len<24 do
			current=closest(x1,y1,flood)
			
			x,y,k=current.x,current.y,current.k
			if (k==pk) break
			flood[k],closedset[k]=nil,true
			flood_len-=1
	
			for _,d in pairs(a_sides) do
				local nx,ny=x,y
				-- works only for quadrants
				if not solid_area({x=nx,y=ny,w=self.w,h=self.h},d[1],d[2]) then
					nx+=d[1]
					ny+=d[2]
				end
				k=flr(nx)+96*flr(ny)
				if not closedset[k] and not camefrom[k] then
					flood[k],camefrom[k]={x=nx,y=ny,k=k},current
					flood_len+=1
				end
			end
		end
	
		local path,prev={},current
		while current do
			add(path,current)
			prev,current=current,camefrom[current.k]
		end
		self.path=path

		-- wait path completion or timeout
		local t=time_t+self.seek_dly
		while #self.path>0 do
			if(t<time_t) break
			yield()
		end
		self.input=nil
	end
end

-- custom actors
function warp_draw_async(r0,r1)
	wait_async(90,function(i)
		local r=lerp(r0,r1,1-smoothstep(i/90))
		local r2=r*r
		for j=0,127 do
			local y=64-j
			local x=sqrt(max(r2-y*y))
			rectfill(0,j,64-x,j,0)
			rectfill(64+x,j,127,j,0)
		end
		return true
	end)
end
_g.warp_update=function(self)
	self.frame+=0.25
	if (self.captured) return
	local dx,dy=plyr.x-self.x,plyr.y-self.y
	local d=dx*dx+dy*dy
	if d<4 then
		self.captured=true
		futures_add(function()
			warp_draw_async(16,96)
			warp_draw_async(96,16)
		end,after_draw)
		futures_add(function()
			plyr_playing,d,a=false,sqrt(d),atan2(dx,dy)
			wait_async(90,function(i)
				local dist=lerp(d,0,i/90)
				plyr.x,plyr.y=self.x+dist*cos(a),self.y+dist*sin(a)
				a+=0.1
				return true
			end)
			plyr_playing=true
			next_level()
		end)
	end
end
_g.health_pickup=function(self)
	if sqr_dist(plyr.x,plyr.y,self.x,self.y)<1 then
		plyr.hp=min(plyr_hpmax,plyr.hp+2)
		make_part(self.x,self.y,0,all_parts["notice"]).txt="heal!"
		sfx(60)
		del(actors,self)
	end	
end
_g.ammo_pickup=function(self)
	if sqr_dist(plyr.x,plyr.y,self.x,self.y)<1 then
		local inc=flr(plyr.wp.ammo/2)
		plyr.ammo=min(plyr.wp.ammo,plyr.ammo+inc)
		make_part(self.x,self.y,0,all_parts["notice"]).txt="ammo!"
		sfx(59)
		del(actors,self)
	end
end

_g.npc_update=function(self)
 if(self.hit_t>time_t) return
	if self.move_t<time_t and #self.path>0 then
		local input=self.input
		if not input or sqr_dist(self.x,self.y,input.x,input.y)<0.25 then
			input=pop(self.path)
			self.input=input
		end
		if input then
			local u,v=normalize(input.x-self.x,input.y-self.y,0.8*self.acc)
			self.dx+=u
			self.dy+=v
		end
	end
	
	-- compute path for only 1 actor/frame
 if self.id==(time_t%actor_id) then
		assert(coresume(self.update_path,self))
 end
 
	if self.pause_dly and self.fire_dly_t<time_t then
		self.fire_t=time_t+self.pause_dly
		self.fire_dly_t=time_t+self.pause_dly+self.fire_dly
	end

	if self.wp and self.los_t<time_t and self.fire_t<time_t then
		self.can_fire=false
		if lineofsight(self.x,self.y,plyr.x,plyr.y,self.los_dist) then
			local dx,dy=plyr.x-self.x,plyr.y-self.y
			self.angle=atan2(dx,dy)%1
			self.facing,self.can_fire=flr(8*self.angle),true
			if self.wp.sight then
				self.move_t,self.fire_t=time_t+45,time_t+30
				if abs(dx)>0 and abs(dy)>0 then
					dx,dy=normalize(dx,dy)
					make_part(self.x,self.y,0,{
						u=dx,v=dy,
						dly=30,
						zorder=3,
						draw=draw_lasersight_part
					})
				end
			end
		end
		self.los_t=time_t+self.wp.dly
	end
	if self.can_fire and self.fire_t<time_t then
		make_blt(self,self.wp)
		self.fire_t=time_t+self.wp.dly
	end
end
_g.draw_rspr_actor=function(self,x,y)
	local ang=atan2(self.dx,self.dy)
	rspr(self.sx,self.sy,x-4,y-4,1-ang)
end
_g.draw_txt_actor=function(self,x,y)
	_g.draw_actor(self,x,y)
	if self.near_plyr_t>time_t then
		_g.draw_txt_part(self,x,y-8)
	end
end
_g.wpdrop_update=function(self)
	if self.btn_t<time_t and sqr_dist(plyr.x,plyr.y,self.x,self.y)<4 then
		self.near_plyr_t=time_t+30
		if btnp(5) or stat(34)==2 then
			make_part(self.x,self.y,0,all_parts["notice"]).txt=self.txt
			-- swap weapons
			local wp,ang=plyr.wp,rnd()
			make_actor(plyr.x,plyr.y,
				clone(all_actors.wpdrop_cls,{
					btn_t=time_t+30,
					dx=0.2*cos(ang),
					dy=0.2*sin(ang),
					drop=wp,
					ammo=plyr.ammo,
					spr=wp.icon,
					txt=wp.n}))
			-- pick drop
			plyr.wp=self.drop
			plyr.ammo=self.ammo
			del(actors,self)
		end
	end
end
_g.throne_init=function(self)
	self.angle=0.75
	local isalive=function()
		return plyr.hp>0 and self.hp>0
	end
	futures_add(function()
		local hp=self.hp
		while(abs(plyr.y-self.y)>4 and hp==self.hp) do
			yield()
		end
		wait_async(60,isalive)
		if not isalive() then
			return
		end
		make_blt(self,weapons.laser)
		wait_async(60,isalive)
		local l=1
		while isalive() do
			wait_async(90,isalive)
			if l%4==0 then
				make_blt(self,weapons.laser)
			else
				local dx,dy=plyr.x-self.x,plyr.y-self.y
				local a,angle=lerp(0,0.2,abs(cos(time_t/16))),atan2(dx,dy)%1
				make_blt({x=self.x-2,y=self.y+1,angle=angle-a,side=bad_side},weapons.mega_gun)
				make_blt({x=self.x+2,y=self.y+1,angle=angle+a,side=bad_side},weapons.mega_gun)
			end
			wait_async(20,function()
				cmap_op(self,cmap_del)
				self.y+=0.025
				cmap_op(self,cmap_add)
				return isalive()
			end)
			if self.y>25 then
				-- kill player!
				plyr:hit(plyr_hpmax)
				break
			end
			l+=1
		end
	end)
end
_g.throne_update=function(self)
	local ang=rnd()
	local u,v=0.16*cos(ang),0.15*sin(ang)
	make_part(self.x+u,self.y+v-0.5,0,all_parts.fart)
end
_g.throne_draw=function(a,x,y)
	x,y=x-4*a.cw,y-4*a.ch
	-- shadow
	palt(0,false)
	rectfill(x,y+4,x+8*a.cw,y+4+8*a.ch,1)

	-- hit effect
	local tcol=a.palt or 14
	if a.hit_t>time_t then
		memset(0x5f00,0xf,16)
		pal(tcol,tcol)
 end
	-- actor
	palt(tcol,true)
	map(a.cx,a.cy,x,y,a.cw,a.ch)
	palt(tcol,false)
	pal()
	palt(0,false)
end

_g.draw_actor=function(a,sx,sy)
	if a.safe_t and a.safe_t>time_t and band(time_t,1)==0 then
		return
	end
	
	local sw,sh=max(1,flr(2*a.w+0.5)),max(1,flr(2*a.h+0.5))
	sx,sy=sx-4*sw,sy-4*sh
	-- shadow
	palt(14,true)	
	sspr(0,8,8,8,sx,sy+7*sh,8*sw,8)
	palt(14,false)	
	-- hit effect
	local tcol=a.palt or 14
	if a.hit_t>time_t then
		memset(0x5f00,0xf,16)
		pal(tcol,tcol)
 	end
 	local s,flipx=a.spr,false
 	if a.frames then
		flipx=face1strip[a.facing+1]
		s=a.frames[flr(a.frame%#a.frames)+1]
	end
	-- actor
	palt(0,false)
	palt(tcol,true)
	spr(s,sx,sy,sw,sh,flipx,flipy)
	palt(tcol,false)
	pal()
	local wp=a.wp
	if wp and wp.sx then
		palt(14,true)
		local u,v=cos(a.angle),sin(a.angle)
		-- recoil animation
		local f=-mid(a.fire_t-time_t,0,8)/4
		rspr(wp.sx,wp.sy,sx+4*u+f*u,sy+4*v+f*v,1-a.angle)
	 palt()
	end
	
	-- draw a-star path
	--[[
	if a.path and #a.path>0 then
		local x0,y0=cam_project(a.path[1].x,a.path[1].y)
		for i=2,#a.path do
			x1,y1=cam_project(a.path[i].x,a.path[i].y)
			line(x0,y0,x1,y1,8)
			x0,y0=x1,y1
		end
	end
	]]
end

all_actors=json_parse('{"actor_cls":{"dx":0,"dy":0,"acc":0.02,"frame":0,"inertia":0.6,"bounce":1,"hp":1,"contact_t":0,"path":[],"move_t":0,"rnd":{"seek_dly":[120,180],"los_t":[50,80],"bones_c":[2,4]},"hit_t":0,"can_fire":false,"fire_t":0,"fire_dly_t":0,"w":0.4,"h":0.4,"los_dist":8,"angle":0,"facing":0,"side":"bad_side","draw":"draw_actor","hit":"hit_actor","update":"npc_update","die":"die_actor"},"barrel_cls":{"side":"any_side","spr":128,"inertia":0.7,"splat":"blast","bones":"green_chunks","update":"nop"},"bandit_cls":{"hp":3,"wp":"base_gun","frames":[4,5,6],"dmg":1,"npc":true,"rnd":{"fire_dly":[90,120],"pause_dly":[90,120]}},"scorpion_cls":{"rnd":{"fire_dly":[180,220]},"acc":0.01,"dmg":2,"pause_dly":120,"w":0.8,"h":0.8,"hp":10,"wp":"acid_gun","palt":5,"frames":[131,133],"npc":true},"worm_cls":{"bones_c":0,"flee":true,"palt":3,"w":0.2,"h":0.2,"inertia":0.8,"dmg":1,"frames":[7,8],"npc":true},"slime_cls":{"w":0.2,"h":0.2,"acc":0.02,"inertia":0.75,"dmg":2,"frames":[31,29,30,29],"wp":"goo","npc":true,"splat":"goo_splat","bones":"goo_chunks"},"dog_cls":{"los_dist":1,"inertia":0.2,"hp":5,"acc":0.06,"wp":"bite","frames":[61,62],"npc":true},"bear_cls":{"hp":8,"flee":true,"inertia":0.2,"frames":[1,2,3],"dmg":1,"npc":true,"wp":"snowball"},"throne_cls":{"zorder":1,"w":6,"h":2,"hp":75,"palt":15,"inertia":0,"acc":0,"cx":87,"cy":18,"cw":12,"ch":5,"update":"throne_update","draw":"throne_draw","init":"throne_init","splat":"blast","bones":"blast","rnd":{"bones_c":[10,20]},"npc":true},"health_cls":{"spr":48,"w":0,"h":0,"update":"health_pickup","hit":"nop"},"ammo_cls":{"spr":32,"w":0,"h":0,"update":"ammo_pickup","hit":"nop"},"wpdrop_cls":{"w":0,"h":0,"btn_t":0,"near_plyr_t":0,"draw":"draw_txt_actor","update":"wpdrop_update","hit":"nop"},"cop_cls":{"hp":8,"flee":true,"acc":0.05,"frames":[13,14,15,14],"rnd":{"fire_dly":[160,210],"pause_dly":[120,160]},"wp":"rifle","npc":true},"fireimp_cls":{"hp":5,"dmg":1,"frames":[45,46,47,46],"acc":0.05,"npc":true,"splat":"blast","bones":"fireimp_chunks"},"turret_cls":{"w":1,"h":1,"wp":"turret_minigun","hp":10,"acc":0,"bounce":0,"frames":[163],"fire_dly":180,"pause_dly":120,"splat":"turret_splat","bones":"blast","npc":true},"horror_cls":{"hp":12,"dmg":2,"frames":[160,161,162],"wp":"radiation","fire_dly":180,"pause_dly":120,"splat":"goo_splat","npc":true,"bones":"goo_chunks"},"warp_cls":{"zorder":1,"w":0,"h":0,"acc":0,"captured":false,"frames":[69,82,81,80],"draw":"draw_spr_part","update":"warp_update","hit":"nop"},"cactus":{"hp":5,"acc":0,"spr":83,"update":"nop","splat":"goo_splat","bones":"green_chunks"},"candle_cls":{"part":"candle","part_dly":4,"part_t":0,"acc":0,"spr":178,"die":"nop","update":"nop"},"frog_cls":{"hp":18,"rnd":{"fire_dly":[160,180]},"pause_dly":120,"w":0.8,"h":0.8,"wp":"acid_gun","frames":[231,233,235,233],"npc":true},"horror_spwnr_cls":{"frames":[84],"acc":0,"npc":true,"hp":10,"wp":"horror_spwn","bones":"green_chunks"},"cop_box_cls":{"w":0.8,"h":0.8,"frames":[237],"acc":0,"npc":true,"hp":20,"wp":"cop_spwn","splat":"turret_splat","bones":"blast"}}')

-- actor
-- x,y in map tiles (not pixels)
function make_actor(x,y,src)
	local a=clone(all_actors.actor_cls,
		clone(src,{
			id=actor_id,
			x=x,
			y=y}))
	if(a.init) a:init()
	if(a.npc) active_actors+=1 actor_id+=1 a.update_path=cocreate(update_path_async)
	if(a.fire_dly) a.fire_dly+=33*cur_loop
	cmap_op(a, cmap_add)
	return add(actors,a)
end

function move_actor(a)
	if a.update then
		a:update()
		if a.disable then
			cmap_op(a,cmap_del)
			return
		end
	end

 if a.part and a.part_t<time_t then
 	make_part(
 		a.x+rndlerp(-a.w,a.w),a.y-0.5,0,
 		all_parts[a.part])
 	a.part_t=time_t+a.part_dly
 end

	-- remove old position 
	cmap_op(a,cmap_del)
	-- only player gets blocked by actors
	local solid_test=a==plyr and solid_a or solid_area
 if not solid_test(a,a.dx,0) then
  a.x+=a.dx
 else
  -- otherwise bounce
  a.dx*=-a.bounce
 end

 -- ditto for y
 if not solid_test(a,0,a.dy) then
  a.y+=a.dy
 else
  a.dy*=-a.bounce
 end
 
 -- apply inertia (free clamp)
	a.dx=amortize(a.dx,a.inertia)
	a.dy=amortize(a.dy,a.inertia)
	
 a.frame+=abs(a.dx)*4
 a.frame+=abs(a.dy)*4
 
 -- update collision map
 cmap_op(a, cmap_add)

 zbuf_write(a)
end

-- player actor
function make_plyr()
	plyr_playing=true
	local body=rndarray(all_plyrs)
	plyr=make_actor(18,18,{
		mousex=0,mousey=0,
		acc=0.045,
		hp=plyr_hpmax,
		side=good_side,
		strips=body.strips,
		frames=body.strips[2],
		wp=weapons["uzi"],
		ammo=weapons.uzi.ammo,
		safe_t=time_t+30,
		idle_t=time_t+30,
		palt=body.palt or 14,
		die=plyr_die,
		update=nop,
		splat="head"
	})
	return plyr
end

function control_player()
 if plyr_playing then
		local wp,angle,fire,dx,dy=plyr.wp,plyr.angle,false,0,0
		if(btn(0)) plyr.dx-=plyr.acc dx=-1 angle=0.5
		if(btn(1)) plyr.dx+=plyr.acc dx=1 angle=0
		if(btn(2)) plyr.dy-=plyr.acc dy=-1 angle=0.25
		if(btn(3)) plyr.dy+=plyr.acc dy=1 angle=0.75
	
		if use_mouse==1 then
			fire=stat(34)==1
			dx,dy=stat(32),stat(33)
			plyr.mousex,plyr.mousey=dx,dy
			angle=(0.5+atan2(64-dx,64-dy))%1
		else
			fire=btn(4)
			if(bor(dx,dy)!=0) angle=atan2(dx,dy)
		end
	
		if fire and plyr.fire_t<time_t then
			if plyr.ammo>0 then
				plyr.fire_t=time_t+wp.dly
				plyr.lock_t=time_t+8
				make_blt(plyr,wp)
				local u={cos(angle),sin(angle)}
				plyr.dx-=0.05*u[1]
				plyr.dy-=0.05*u[2]
				cam_shake(u[1],u[2],wp.shk_pow or 0)
			end
		end
		if use_mouse==1 or plyr.lock_t<time_t then
			plyr.angle,plyr.facing=angle,flr(8*angle)
		end
 end
	
 if abs(plyr.dx)+abs(plyr.dy)>0.1 then
  plyr.frames=plyr.strips[1]
  plyr.idle_t=time_t+30
 end
 if plyr.idle_t<time_t then
		plyr.frames=plyr.strips[2]
		if time_t%8==0 then
			plyr.frame+=1
		end
 end
end

function next_level()
	time_t=0
	actor_id=0
	lvl_i+=1
	-- loop?
	local loop
	if lvl_i>#levels then
		cur_loop+=1
		lvl_i=1
		loop=true
	end
	-- stronger
	plyr_hpmax=8*cur_loop
	
	-- clear entities
	cmap,actors={},{}
	parts={}
	make_level()
	add(actors,plyr)
	
	if lvl.builtin then
		plyr.x,plyr.y=lvl.plyr_pos[1]+0.5,lvl.plyr_pos[2]+0.5
	else
		local r=rooms[1]
		plyr.x,plyr.y=r.x+r.w/2,r.y+r.h/2
	end
	plyr.dx,plyr.dy,plyr.hit_t,plyr.fire_t,plyr.lock_t=0,0,0,0,0
	plyr.safe_t=time_t+30*cur_loop
	plyr_playing=true
	
	if loop then
		make_part(plyr.x,plyr.y,0.5,all_parts["notice"]).txt="i feel stronger!"
	end
	
	music(-1,250)
	music(lvl.music or 14)
	--[[
	-- benchmark
	for i=1,10 do
		for j=1,10 do
			local a=make_actor(plyr.x+i,plyr.y+j,all_actors.scorpion_cls)
			a.update=nop
		end
	end
	]]
end

-- start screen
local starting=false
start_screen.update=function()
	if not starting and press_start() then
		starting=true
		futures_add(function()
			warp_draw_async(16,96)
			warp_draw_async(96,16)
			end,after_draw)
		futures_add(function()
			wait_async(90)
			lvl_i,cur_loop=0,1
			plyr_hpmax=8
			plyr=make_plyr()
			next_level()
			starting=false
			cur_screen=game_screen
			wait_async(90)
		end)
	end
end
start_screen.draw=function()
	cls(2)
	fillp(0xa5a5)
	local a,r,x,y=time_t/32,0
	for i=1,196 do
		x,y=r*cos(a),r*sin(a)
		circfill(64+x,64-y,r/8,0x10)
		a+=0.02
		r+=0.5
	end
	fillp()
	
	x,y=cos(time_t/64),sin(-time_t/64)
	rspr(8,8,64+12*x,64+12*y,atan2(x,y))
 
	palt(0,false)
	palt(14,true)
	sspr(0,112,56,16,10,12,112,32)
	palt()
	
	txt_options(true,3)
	if time_t%32>16 then
		txt_print("press start",64,108,11)
	end
	txt_print(use_mouse==1 and "[keyb.+mouse]" or "[keyboard]",64,116,7)
	
	txt_options(true,0,true)
	txt_print("freds72 presents",64,3,6)
end

-- game screen
game_screen.update=function()
	pause_t-=1
	if(pause_t>0) return
	pause_t=0
	
	zbuf_clear()
	control_player(plyr)
	
	for _,v in pairs(actors) do
		move_actor(v)
	end
	forall(parts,"update")
	cam_update()
end
game_screen.draw=function()

 cam_track(plyr.x,plyr.y)

	cls(lvl.bkg_col)
	local cx,cy=lvl.cx or 0,lvl.cy or 0
	local sx,sy=64-cam_x+8*cx,64-cam_y+8*cy-4
	pal()
	palt(0,false)
	map(cx,cy,sx,sy,level_cw,level_ch,1)
	zbuf_draw()

	pal()
	palt() 
	if lvl.borders then
		pal(10,lvl.borders[1])
		pal(9,lvl.borders[2])
		pal(1,lvl.borders[3])
	end
	map(cx,cy,sx,sy,level_cw,level_ch,2)
	pal()
	if(lvl.shader) lvl.shader()
	if use_mouse==1 then
		spr(lvl.cursor or 35,plyr.mousex-3,plyr.mousey-3)
	end

	--[[
	local h
	for i=103,120 do
		for j=0,31 do
			h=i+128*j
			if cmap[h] then
				local x,y=cam_project(i,j)
				print(#cmap[h],x,y,7)
			end
		end
	end
	]]
	if plyr_playing then
		rectfill(1,1,34,9,0)
		rect(2,2,33,8,6)
		local hp=max(flr(plyr.hp))
		rectfill(3,3,3+flr(29*hp/plyr_hpmax),7,8)
		txt_options(false,0)
		txt_print(hp.."/"..plyr_hpmax,12,3,7)
	
		palt(14,true)
		palt(0,false)
		spr(plyr.wp.icon,2,10)
		txt_print(plyr.ammo,14,12,7)
	end
end
--local perf_update,perf_draw=0,0
function _update60()
	time_t+=1
	time_dt+=1
	local t=stat(1)
	futures_update(before_update)
	cur_screen.update()
	--perf_update=stat(1)-t
end

function _draw()
	local t=stat(1)
	cur_screen.draw()
	futures_update(after_draw)

	--[[
	perf_draw=stat(1)-t	
	print(perf_update,2,112,7)
	print(perf_draw,2,120,7)
	]]
	
	time_dt=0
end

function _init()
	-- mouse support
	poke(0x5f2d,1)
	if cartdata("freds72_nuklear_klone") then
		use_mouse=dget(0)
	end
	menuitem(1,"mouse on/off", function() 
		use_mouse=bxor(use_mouse,1)
		dset(0,use_mouse)
	end)
	cur_screen=start_screen
	music(0)
end

__gfx__
00000000e000000ee0000000e000000ee000000ee000000ee000000e333333333333333300000000eeeeeeeeeeeeeeeeee3333eee000000ee000000ee000000e
070000700676767006676760056676700f66ff600f66ff600f66ff60333333333333333300000000eeeeeeeeeeeeeeeee3bbbb3e01111a10011111a001111110
00777700079898600579898006579890055858500558585005585850333333333333333300073000eee99eeeeee99eee3b7777b301c00000011c00000111c000
007777000694047006694040056694000ff66ff00ff66ff00ff66ff0333000333333333300073000ee9aa9eeee9999ee3b7777b30ccc0c000cccc0c00ccccc00
0077770007676760057676700657676006ff66f006ff66f006ff66f0330fef033300003300000000ee9aa9eeee9999ee3b7777b30cccccc00cccccc00cccccc0
007777000444444004444440044444400f66f6600f66f6600f66f660330e0e0330efef0300000000eee99eeeeee99eee3b7777b3055556500555556005555550
0700007005000050e050010ee005100ee06f0ff0e006f0f00f006f0e30ef0fe00ef00fe000000000eeeeeeeeeeeeeeeee3bbbb3e07000070e070070ee006700e
00000000000ee000e000000eeee00eeeee00e00eeee00e0ee0ee00ee330030033003300300000000eeeeeeeeeeeeeeeeee3333eee0eeee0eee0ee0eeeee00eee
e111111eee00000eee00000eee00000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee00eeeeeeeeeeeeee00eee
11111111e0999aa0e09999a0e0999990eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000eeeee777eeeeeeee33eeeeeeeeeeeee7777eeee0370eeee0000eeee0370ee
e111111e099414100999414009999410eeeeeeeeeeeeeeeee0000000e77777770bb0000070077777ee3bb3eeeee33eeee777777ee03bb70ee03bb70eee0370ee
eeeeeeee094444400994444009994440ee00000eee77777ee0b333b0e700000703b6606070000707e3b77b3eee3333eee777777ee03bbb0e03bbbb70ee03b0ee
eeeeeeee044455500444455004444450ee000eeeee707eeee0113110e70000070335505070000707e3b77b3eee3333eee777777ee03bbb0e03bbbbb0ee03b0ee
eeeeeeee0333bab003333ba0033333b0eee0eeeeeee7eeeee0000000e77777770550000070077777ee3bb3eeeee33eeee777777e03bbbbb003bbbbb0e03bbb0e
eeeeeeee05000050e050050ee005500eeee0eeeeeee7eeeeeeeeeeeeeeeeeeee0660eeee7007eeeeeee33eeeeeeeeeeeee7777ee03bbbbb003bbbbb003bbbbb0
eeeeeeeee0eeee0eee0ee0eeeee00eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000eeee7777eeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000
ee00000eee00000eeeeeeeee77077000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee7777eeeeeeeeeee000000ee000000ee000000e
e0bbbbb0e0999aa0ee00000e70007000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee7bbbb7eee00eeee022898900228898002288890
e077777009944440e0999aa000700000ee000000ee777777eeee0e0eeeee7e7eeeeeeeeeeeeeeeeeeeeaaeee7b3333b7e0e00eee0228a8a002288a80022888a0
e0373730094414100994141070007000e0496660e7000007ee001010ee770707e0000000e7777777eea77aee7b3333b7ee03b0ee022888800228888002288880
e0353530044444400944444077077000e0445550e7000007e055c1c0e70000070046666077000007eea77aee7b3333b7ee0130ee022767600228767002288760
e0333330044455500444555000000000e0400000e7077777e0501010e70707070410000070077777eeeaaeee7b3333b7eee00eee022686800228686002288680
e05333500333bab00333bab000000000ee0eeeeeee7eeeeeee0e0e0eee7e7e7ee00eeeeee77eeeeeeeeeeeeee7bbbb7eeeeeeeee02000020e020010ee002100e
ee00000ee000000ee000000e00000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee7777eeeeeeeeee00eeee00ee0ee0eeeee00eee
ee00000e330000033300000333000003eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee3300000333333333eeeeeeee0082018feeeeeeeee0e0eeeee0e0eeeeeeeeeeee
e066666030222ee0302222e030222220eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee30222ee033000003ee88eeee10941d9aeeeeeeee090900ee0909000eeeeeeeee
e0777770022f1f100222f1f002222f10ee00000eee77777eee000000ee777777022ffff030222ee0e000000e21a92ea7eee55eee0dd8480e0dd84540eeeeeeee
e0dd8dd0022ffff00222fff002222ff0e076670ee700007ee03bb660e7000007022f1f10022f1f10e088777031b33bb6ee5675ee0d4454400d447070eeeebbee
e0d888d00ffff8f00fffff800ffffff0e055000ee700777e0453b000700007770f2ffff0022ffff0e055667045c149c7ee5665ee0447070e0441110eeebbbbbe
e0d686d0055555500555555005555550e050eeeee707eeee04400eee70077eee0ffff8f00ffff8f0e000000e51d156d6eee55eee044444400447070eee3bbb3e
e0dd6dd0070000703070060330067003ee0eeeeeee7eeeeee00e0eeee77e7eee0555555005555550ee88eeee65e267efeeeeeeee0404004004044440eee333ee
e0000000303333033303303333300333eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee3000000330000003eeeeeeee76fd77f7eeeeeeeee0e0ee0ee0e0000eeeeeeeee
4444444444444444040404044444444444444444eee222ee666666666666666666666166666166665c775c5c66666666d1dddddddddddddd121212eed2dddddd
4444444444994444404040404444444444444444ee21112e66666666666666666116666666156666ccc7c7c561111116d01dddddd1eddddd21ee21de20ddd2ed
44b4b44445495444040404044944444444444444e2122212666666666666666665566666615666661cc7c77c15d5d5d1d00dddddd11ddddd11dde212ddd0d02d
435b5344445544444040404045444494444444442121122166666666611116666666666165166666c111ccc56d5d5d66ddddd11ddddddddd21dde121dd02dd0d
4535354444444444040404044444445444444444212121216666666615d5d66666666666665166665c5cc77c15d5d666ddd11001ddddeedd12111212d02d0ddd
445554444444444440404040444944444444444421222121666666665d5d51666666166666156666c5c5c1c75d5d51161dd00000ddd12e1d2121de21dd0dd0dd
4444444444444444040404044445444444444444e21112216666666665d5d5666666666666566666515c7ccc65d5d5560dd0000ddddd11dd12121d12d2dd02d0
4444444444444444404040404444444444444444ee22221e66666666666d5d666666666666666666c115c7c5665d56d6dddd00dddddddddd2121212100dd2ddd
ee222eeee12222eeeee1111eeee00eeeeee00eee66666666555555555555555555dddd5536111161313131313535353555555155110110001111111111111111
e21112ee1221112eee122221e00bb0eee00bb00e6666656655555555555555455d5555d515666653131313135377775355151055100010005151515161616161
2122212e12122212e12211220b05300ee07bb70e666666665555555555555555d55dd55d31555511313131313700007551000015001000001515151516161616
2121121212121212e1212212030350b0e037730e665666665555555555555555d5d51d5d13111113131313135600006355000001100010005555555566666666
2122121212211212e1211212e0353530e033330e666666665555555554455555d5d11d5d36111161313131313622206555500000110110005555555566666666
221122122122212ee2122212ee03500ee033330e666666665555555554455555d55dd55d156666531313131355eee65355100005000000005555555566666666
1222212ee21112eeee21112eee0530eee003300e6666656655555555555554555d5555d531555511313131313522553551500055000000005555555566666666
e11112eeee222eeeeee222eeee0000eeeee00eee66666666555555555555555555dddd5513111113131313135322535355150515000000005555555566666666
666166669991999999000009906000606660666600000000dddd11116666666667676666ddddd11d6dddddd65555555599959999eeeeeeee5555555544444444
661516664491444440445440402222206605066611010111dddd11116555555665656666dddd11116dd77dd6111100004aaaa774ee00000e5555555544444444
615551661111111110095900108000806666666610111011dddd11116000000665656666dddd11116d7667d6111100005acccc75ee06940e5454545447444744
155555169999919990440440908080800066606655555556dddd111160b0280665656666dddd111d6d6666d6dddd11119a333ca9ee09a60e4444444441676144
6555556644449144409565904088888065600566655555661111dddd6000000665656666d1dddddd6d5665d61111dddd4a3333a4ee05450e4444444444777444
6655566611111111100454001088088066655666665556661111dddd6677776665656666111ddddd6dd55dd61111dddd5aaaaaa5ee04540e4444444444161444
6665666699919999909959909020502066656666666566661111dddd66666666656566661111dddd6dddddd61111dddd92212229ee05450e4444444444444444
6666666644914444400000004001110066666666666666661111dddd6666666660606666dd1ddddd667777661111dddd44954444ee00000e4444444444444444
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa9111111991111111911111199111111111111119111111111111111911111111
a111111aa1111111a111111aa11111111111111a111111111111111a111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
99999999999999999111111991111111999999999999999911111119111111119999999999999999911111199111111199999999999999991111111911111111
eeeeeeeeee88eeeeeeeeeeee55555555555555555555555000555555ffffffffccccccccccccccccffffffffffffffffcccc00000000ccccfffaafff00000000
ee0000eee22ee22eeeeeeeee55555550005555555555550eee055555ffffffffccccccccccccccccffffffffffffffffccc0666576660cccfff99fff00000000
e07bb70ee8eee28eeee3eeee5555550eee05555555555502e2055555ffffffffcccccc0000ccccccffffffffffffffffccc0666666660cccfff88fff00000000
e0b77b0eeeeeeeee3eeb2ee355555502e20555555555550070055555ffffffffccccc0eee70cccccffffffffffffffffccc0777777770cccfff00fff00000000
e03bb30ee8ee8ee8ebe2eebe5555550070055555555555010105555500ffff00cccc02eee7e0cccc0000000000000000ccc0555555550ccc0000000000000000
e0b77b0eeee888e2eeeeeeee55555501010555555555550111055555c000000ccc6002eee7e006ccccccccc00cccccccccc0555555550cccccc00ccc00000000
e03bb30eeee28eeeee3eee3e55555011111055555555501111105555c1cccc1ccc6602eee7e066ccccccccc00cccccccccc0555555550ccccc0000cc00000000
ee0000eeeeeeeeeeeb2eeeeb55000122122100555550012222210005cccccccccc5502eee7e055ccccccccc00cccccccccc0066666600cccc060060c00000000
0000e000eeeeeeeeeeeeeeee50222211111222055502221111122220cc0000cccc1102eee7e011ccccccccc00ccccccccccc06655660ccccc071170c00000000
0b700bb0eaaaaaaeeee82eee55000122222100205020012222210005c0bbbb0ccccc02eee7e0cccccccc00000000ccccccc0665bb5660cccc057750c00000000
0bb0bb30e919119eeeee82ee502221eeeee12205550221eeeee12220c0bbbb0ccc60020000e006cccccc01100110ccccccc066bbbb660cccc055550c00000000
0bbbb30ee999999eeeee82ee5500028fef8200205020028fef820005c077770ccc660025620066cccccc05500550ccccccc066bbbb660cccc055550c00000000
0bbbbb0ee911919eeee82eee50222122f221220555022122f2212220c033330ccc550256762055cccccc00000000cccccccc06666660ccccc100001c00000000
03b03bb0eee55eeeee82eeee50200070007000205020007000700020c033330ccc110256762011ccccccccc00cccccccccc1100000011ccccc1111cc00000000
033003b0eee99eeeeeeeeeee55011101110110205020110111011105cc1111cccccc00567600ccccccccccc00cccccccccc1105555011ccccccccccc00000000
0000e000eee99eeeeeeeeeee55551111111111055501111111111555cccccccccccc1156760cccccccccccc00ccccccccccc11000011cccccccccccc00000000
eee0ee0eeeee0eeeee0ee0eeeeeeee0000eeeeeeeeeeeeeeeeeeeeeecccccccccccccc56671cccccccccccc00cccccccccc7e222222e7cccccccccc000000000
e00b00b0e000b00ee0b00b0eeeee00666600eeeeeeeeeeeeeeeeeeee777777777777775005777777777777700777777777777777777777777777777000000000
0b0b0bb00bb0b0b00bbb0b0eeee0666666660eeeeeeeeeeeeeeeeeee111111111111115005111111111111100111111111152222222251111111111000000000
0bbbbbb00bbbbbb00bbbbbb0eee0666666660eeeeeeeeeeeeeeeeeee111111111111111551111111111111100111111111152222222251111111111000000000
0bbb33300bbbb3300bbbbb30ee056666666650eeeeeeeeeeeeeee55e111111111111111111111111111111100111111111152222222251111111111000000000
0bbbbbb00bbbbbb00bbbbbb0e06577666677560ee0eeeeeeeeee000e000000000000000000000000000000000000000000055555555550000000000000000000
0b0000b0e0b0030e000b3000056055777755065005e00e5eddeeee50cccccccccccccc0000ccccccccccccc00cccccccccc7eeeeeeee7ccccccccccc00000000
00eeee00ee0ee0eeeee00eee0560005555000650eeedee5ed000eeeeccccccccccccccccccccccccccccccc00cccccccccc7eeeeeeee7ccccccccccc00000000
eeeeeeeeeeeeeeeeeeeeeeee0556000000006550eeee00ee0500eeeeccccccccccccccccffffffffccccccc00cccccccccc7eeeeeeee7ccc6667eeeeeeee7666
eeeeeeeeeeeeeeeeeeeaeeee0555660000665550eee1051e1111eeee7777777777777777ffffffff777777700777777777777777777777776617eeeeeeee7666
0eeeeeeee0eeeeeeee070eee055555666655555005ee11eeeeeeeeee1111111111000111ffffffff111111100111111111152222222251116157eeeeeeee7166
e0eee0ee0eeee0eee06760ee05555555555555500dee5ee5eeeeeeee1111111110567011ffffffff111111100111111111152222222251111557eeeeeeee7516
0ee00f0ee0e00f0eee060eeee05550505055550ee0deee11eeeeee0e1111111110567011ffffffff111111100111111111152222222251116557eeeeeeee7566
0e05580e0e05580eee050eeeee055151515550ee500eeeeeee5ed55e0000000000576000ffffffff000000000000000000000000000000006657eeeeeeee7666
e0555550e0555550e05050eeeee0055555500eee555ee55deeeee50efffffffff00650ffffffffffffffffffffffffffffffffffffffffff6667eeeeeeee7666
ee00000eee00000ee00e00eeeeeee000000eeeeeeeeee0000eeee00effffffffff000fffffffffffffffffffffffffffffffffffffffffff6667eeeeeeee7666
eeeee777eeeeeeeeeeeee111eeeeee1eeee8eeeeeee8eeeeeee8eeeeeee0eeeeeeeeeeeeeee000eeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000
eee77777eeeeeeeeeee11111eee1e1e1eeee8eeeeeee8eeeeeee8eeeee0b0eeeeee000eeee07770eeeeee0eeee0e0eeeeee00eee000000000000000000000000
ee777777eeeee000ee111888ee1e1e1eeeee88eeeeee88eeeeee88eeeee0eeeeee03330ee0707070ee0e070ee07070eeee0770ee000000000000000000000000
e7777777eeee0000e1188888e1e1eeeeeeee8eeeeeee88eeeeee88eeeeeee0eee0b33b0ee0670760e07070eeee070eeeeee0070e000000000000000000000000
e7777777eee00000e1188999ee1eeeeeeeeeeeeeeee888eeeee888eee0ee030eee0bb0eeee07770eee07070eee070eeeeeeee0ee000000000000000000000000
77777777ee00000011889999e1eeeeeeeeeeeeeeeeeee8eeee8888ee030e0bb0eee00eeeee06660ee070e0eee07070eee0eeeeee000000000000000000000000
77777777ee0000001188999a1eeeeeeeeeeeeeeeeeeeeeeee8888eeee0eee00eeeeeeeeeeee000eeee0eeeeeee0e0eee070eeeee000000000000000000000000
77777777ee000000118899a7e1eeeeeeeeeeeeeeeeeeeeeeee88eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0eeeeee000000000000000000000000
eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0eeeeeeee0eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0eeeeeeeeeeee000000000000000000000000
eeeeeeeeeeeeee88eeeee1e1eeeee1e1ee080eeeeee080eeeee00eeeeee00eeeeee00eeeeeeeeeeeeeeeeeeeee080eeeeee000ee000000000000000000000000
eeeeeeeeeeee8899ee1eeeeeee1eeeeeee080eeeee0080eeee0880eeee0bb0eeee0b70eeeeee0eeeeee00eeeeee0eeeeee02220e000000000000000000000000
eeeee888eee899aaeee1eeeeeee1eeeeee0990eee08990eee099980ee0377b0eee07330eeeeeeeeeee0560eeeeeee0eee082280e000000000000000000000000
eeee8999ee89aaaaeeeeeeeeeeeeeeeee09a990ee09a790ee09aa90ee033370eee0330eee00eeeeeee05560ee0ee020eee0880ee000000000000000000000000
eee899aaee89a777e1eeeeeee1eeeeeee0a7aa0ee0a77a0ee0a77a0ee003300eee10011ee00eeeeee1100011020e0880eee00eee000000000000000000000000
eee89aa7e89aa7771eeeeeee1eeeeeeeee0000eeee0000eeee0000eee110011eeee111eeeeeeee0eee11111ee0eee00eeeeeeeee000000000000000000000000
eee89a77e89aa777eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee1111eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000
000ee000000ee0000000000000000eee0000000ee000000e0000000eeeee00000000eeeeeeee00000000eeeeeeee00000000eeeeeeeeeeeeeeeeeeee00000000
07700770077007700b700bb007770eee0777770e0077770007777700ee00bbbbbbbb00eeee00bbbbbbbb00eeee00bbbbbbbb00eeeeee000000000eee00000000
07770770077007700bb0bb3007770eee0776660e0770077007700770e0773bbbbbb3770ee0773bbbbbb3770ee0333bbbbbb3330eeee0ccc161ccc0ee00000000
07777770077007700bbbb30007770000077000ee0770077007700760078873bbbb378870078873bbbb378870033333bbbb333330eee0cc15751cc0ee00000000
07767770077007700bbbbb0007777770077770ee077667700777770e068073bbbb378060068073bbbb370860013331bbbb133310eee0ccc111ccc0ee00000000
067067700677776003b03bb0067777700677770e0777777007777770e0663bbbbbb3660ee0663bbbbbb3660ee0111bbbbbb1110eeee0ccccccccc0ee00000000
0660066000666600033003b0066666600666660e0660066006606660ee056666666650eeee056666666650eeee056666666650eeeee07777777770ee00000000
000ee000e000000e00000000000000000000000e00000000000e0000e03333333333330ee03333333333330ee03333333333330eeee01111011110ee00000000
eeeeeeeeeeeeeeee0000000000000eeee000000e000ee0000000000ee03000000022030ee03000000220030ee03000002200030eeee01661016610ee00000000
eeeeeeeeeeeeeeee0b700bb007770eee00777700077007700777770ee03333333312330ee03333331223330ee03333332133330eeee01771017710ee00000000
eeeeeeeeeeeeeeee0bb0bb3007770eee07766770077707700776660ee00111111111100eee011111111110ee00011111111110eeeee01111011110ee00000000
eeeeeeeeeeeeeeee0bbbb300077700000770077007777770077000ee0bb3301001033bb000011333333110000bb3313133311000eee00101010100ee00000000
eeeeeeeeeeeeeeee0bbbbb00077777700770077007767770077770ee03333131131333300bb3313113133bb00333313113133bb0eee01010001010ee00000000
eeeeeeeeeeeeeeee03b03bb00777777006777760077067700677770e066001311310066003333131131333300660003113133330eee00101010100ee00000000
eeeeeeeeeeeeeeee033003b00666666000666600066006600666660e000ee0b11b0ee00006600b3113b00660000ee0b103b00660eee01010001010ee00000000
eeeeeeeeeeeeeeee0000000000000000e000000e000ee0000000000eeeeee000000eeeee000ee00ee00ee000eeeee000e00ee000eee00000000000ee00000000
__gff__
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001010501010101010101050101010501010101008201010101050505010101010105050501010105050105010501010182828282828282828282828282828282
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010100000000000000000000000000000000000000000000008282000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
7f7f7f7d7d7d7d7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7f7f7f7f7f7f7f7f7f0000000000000000000000000000000000000000000000007f7f7f7f7f7f7f7f7f7f7f7f0000007f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f00000000000000
7f7f7e424242427b7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7d7d7d7d7d7d7d7e4242424242424242424242424242424242427b7f7d7d7d7d7d7d7f0000000000000000000000000000000000000000000000007f7f7d7d7d7d7d7d7d7d7f7f0000007f7f7d7d7d7d7d7d7d7d7d7d7d7d7d7d7f7f00000000000000
7f7d7c6e6e6e6e797d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7d7c42424242424242786e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e7b7e4242424242427b0000000000000000000000000000000000000000000000007f7e42424242424242427b7f0000007f7e63616161616161616161616161637b7f00000000000000
7e424244444444424242424242424242424242424242424242424242426e6e6e6e6e6e6e424444444444444444444444444444444444417b7e6e6e6e6e6e6e7b0000000000000000000000000000000000000000000000007f7e6e6e6e6e6e6e6e6e7b7f0000007f7e65656565656565656565656565657b7f00000000000000
7e6e6e4444446f6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e6e444444414443446e404444444044446f444444444444444444447b7e4444444444447b0000000000000000000000000000000000000000000000007f7e44444444444444447b7f0000007f7e60646064606060646064606460607b7f00000000000000
7e4444444444444344444444414444444444444444444444444444444444446f4444444444446f444444446f4444404444444444444441797c4444444444447b0000000000000000000000000000000000000000000000007f7e4441444444446f447b7f0000007f7e64605555556055555555555555607b7f00000000000000
7e444444444444444444444444444444444444444444444444444444444444444444444444446f4444444444444444444444444444444442424444444444447b0000000000000000000000000000000000000000000000007f7e44444444444444447b7f0000007f7e60605560555555646055555555607b7f00000000000000
7f7777777777777777777644444444444444737777764140446f737776444444444444444444444444444444446f4444444444444444446e6e4444444444447b0000000000000000000000000000000000000000000000007f7e44444444444444447b7f0000007f7e60555555645555645564645560607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7e444444444444407b7f7f7e4444446f7b7f7e444444444444444444444444444444444444444444444444444444444044444444447b0000000000000000000000000000000000000000000000007f7e44454443444440447b7f0000007f7e60646460645555556055605564607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7e444444444144447b7f7f7e44444444797d7c444444444444444444444444444441444444444444446f44444444444444444444447b0000000000000000000000000000000000000000000000007f7e44444444444444457b7f0000007f7e606060606060bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7e444044444444447b7f7f7e44444444424242446f43444444444444444444444444444444444444444444444044444444444444447b0000000000000000000000000000000000000000000000007f7e44444344444444447b7f0000007f7e606064606060bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7f7d7c44444444444441797d7d7c444444446e6e6e4444444444444471777777777644446f4444444444444444444444444444444444447b0000000000000000000000000000000000000000000000007f7e44444440404444447b7f0000007f7e606460646060bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7e424244444444444444424242424444444441444444444444444444427b7f7f7f7e4444444444444344444444444444444144444444447b0000000000000000000000000000000000000000000000007f7f77777777777777777f7f0000007f7e606060606060bebf6064606060607b7f00000000000000
7f7f7f7f7f7f7f7f7e6e6e444444444444446e6e6e6e4444444444444444446f444444436e7b7f7f7f7e4444444444444444444444444444434444444444447b0000000000000000000000000000000000000000000000007f7f7f7f7f7f7f7f7f7f7f7f0000007f7e606060606060bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7e434444444444444444444444444444446f40444444444444444444447b7f7f7f7e4444444441444444444444414444444444444444447b0000000000000000000000000000000000000000000000000000000000000000000000000000007f7e606060606060bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7e4441444444444444444443444444444444446f4444444444446f4444797d7d7d7c444444444444444444444444446f444444444473777f0000000000000000000000000000000000000000000000000000000000000000000000000000007f7e606060606060bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7e444444444444444444447377764444444444444444444444444044444242424242444443444444444473777777764444444444447b7f7f0000000000000000000000000000000000000000000000000000000000000000000000000000007f7e606060606060bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7e444444444444444444447b7f7e4444444444444440444444444444446e6e6e6e6e44737776444444447b7f7f7f7e4444444444447b7f7f0000000000000000000000000000000000000000000000000000000000000000000000000000007f7e646060646060bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7e444444444444444444447b7f7e4444444144444440444444444444444144444444407b7f7e444144447b7f7f7f7e4444444444447b7f7f00000000000000000000000000000000000000000000008b8b8e8787878787878e8a8a000000007f7e606060606060bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7f777644444444737777777f7f7e444444444444446f444444444440444444444343447b7f7e44444444797d7d7d7c4444444444447b7f7f00000000000000000000000000000000000000000000009b9b9e88898c8d88899e9a9a000000007f7e606060646460bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7e444444447b7f7f7f7f7f7f7777777777777777777777777777777777777777777f7f7e4444404442424242424444444444447b7f7f00000000000000000000000000000000000000000000009b9b9798999c9d9899979a9a000000007f7e606060606060bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7e444444447b7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7e44446f446e6e6e6e6e4444444444447b7f7f00000000000000000000000000000000000000000000009baba7a8a9acada8a9a7aa9a000000007f7e646060646060bebf6464606064607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7e444444447b7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7e4444444444434444444444444444447b7f7f0000000000000000000000000000000000000000000000bbb8b8b8b8bcbdb8b8b8b8ba000000007f7e606060606060bebf6064606060607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7e444344447b7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7e444444444444434044444444444444797d7f0000000000000000000000000000000000000000000000000000000000000000000000000000007f7e606060646460bebf6060646060607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7e444444447b7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7e44444444737777777644444444444342427b0000000000000000000000000000000000000000000000000000000000000000000000000000007f7e646060646060bebf6064646060607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7f777777777f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7e4444446f7b7f7f7f7e4444444444446e6e7b0000000000000000000000000000000000000000000000000000000000000000000000000000007f7e606060606060bebf6060646060607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f777777777f7f7f7f7e44444444444444447b0000000000000000000000000000000000000000000000000000000000000000000000000000007f7e646060646060bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7e44444444444344447b0000000000000000000000000000000000000000000000000000000000000000000000000000007f7e606060606060bebf6060606464607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7e44444444737777777f0000000000000000000000000000000000000000000000000000000000000000000000000000007f7e606060646460bebf6060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7e6f4444447b7f7f7f7f0000000000000000000000000000000000000000000000000000000000000000000000000000007f7e60606060606060606060606060607b7f00000000000000
7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f777777777f7f7f7f7f0000000000000000000000000000000000000000000000000000000000000000000000000000007f7f77777777777777777777777777777f7f00000000000000
7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f000000000000000000000000000000000000000000000000000000000000000000000000000000007f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f7f00000000000000
__sfx__
010a00000b1500b1300c1300b1200b1200c1200b1200b1200c1200b1200b1200c1200b1200b1200c1200b1200b1200c1200b1200b1200c1200b1200b1200c1200b1200b1200c1200b1200b1300c1300b1300b130
010a00000c14000000000000c12000000000000c12000000000000c12000000000000c12000000000000c12000000000000c12000000000000c12000000000000c12022110221151811022110221151611022120
010a0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001a1101a115211101a1101a1151f1101a120
010a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000022110221152111022110221151f11022120
010a00000c1300b1300b1300c130131201a1200c130181201a1200c130161201a1200c130181301a1300c1400b1400b1400c1400b1400b1400c1400b1400b1400c1400b1500b1500c150131401a1400c15018140
010a00002212518120221202212513120221202212518120221202212516120221202212518130221302213513130221302213518130221302213516130221302213518130221302213513140221402214518140
010a00001a125211201a1201a1252612022120221252112022120221251f1202212022125211302213022135261301a1301a135211301a1301a1351f1301a1301a135211301a1301a13526140221402214521140
010a000022125211202212022125261201a1201a125211201a1201a1251f1201a1201a125211301a1301a1352613022130221352113022130221351f1302213022135211302213022135261401a1401a14521140
010a00001a1400c150161401a1400c150181401a1400c1502614022140221452114022140221451f14022140221452114022140221452614022140221452114022140221451f1402214022145211402214022145
010a000022140221451614022140221451814022140221451f1401a1401a145181401a1401a145161401a1401a145181401a1401a1452614022140221452114022140221451f1402214022145211402214022145
010a000022140221451f14022140221452114022140221451f1401a1401a145181401a1401a145161401a1401a145181401a1401a145131401a1401a145181401a1401a145161401a1401a145181401a1401a145
010a00001a1401a1451f1401a1401a145211401a1401a1452614022140221452114022140221451f1402214022145211402214022145131401a1401a145181401a1401a145161401a1401a145181401a1401a145
010a00002714024140241452314024140241451f14024140241452314024140241452714024140241452314024140241451f14024140241452314024140241452614021140211451f14021140211451e14021140
010a00002714024140241451a1401b1401b145181401b1401b1451a1401b1401b145131401b1401b1451a1401b1401b145181401b1401b1451a14024140241452614021140211451f14021140211451e14021140
010a00001f1401b1401b1451a1401b1401b145181401b1401b1451a1401b1401b145131401b1401b1451a1401b1401b145181401b1401b1451a1401b1401b145211401a1401a145181401a1401a145151401a140
010a00001f1401b1401b1452314024140241451f14024140241452314024140241452714024140241452314024140241451f1402414024145231401b1401b145211401a1401a145181401a1401a145151401a140
010a0000211451f14021140211452614021140211451f14021140211451e14021140211451f1402114021145221401f1401f1451e1401f1401f1451a1401f1401f1451e1401f1401f145221401f1401f1451e140
010a0000211451f14021140211452614021140211451f14021140211451e1402114021145181402114021145221401f1401f1451e1401f1401f1451a1401f1401f1451e1401f1401f145221401f1401f1451e140
010a00001a145181401a1401a145121401a1401a145181401a1401a145151401a1401a145181401a1401a1451f1401a1401a145181401a1401a145161401a1401a145181401a1401a145131401a1401a14518140
010a00001a145181401a1401a145121401a1401a145181401a1401a145151401a1401a1451f1401a1401a1451f1401a1401a145181401a1401a145161401a1401a145181401a1401a145131401a1401a14518140
010a00001f1401f1451a1401f1401f1451e1401f1401f145241401f1401f1451e1401f1401f1451b1401f1401f1451e1401f1401f145241401f1401f1451e1401f1401f1451b1401f1401f1451e1401f1401f145
010a00001a1401a145161401a1401a145181401a1401a1452414018140181451514018140181451314018140181451514018140181450f1401814018145151401814018145131401814018145151401814018145
010a0000221401f1401f1451e1401f1401f1451a1401f1401f1451e1401f1401f145221401f1401f1451e1401f1401f1451a1401f1401f1451e1401f1401f145211401e1401e1451c1401e1401e1451a1401e140
010a00002214016140161451514016140161451314016140161451514016140161450e14016140161451514016140161451314016140161451514016140161452114015140151451314015140151451214015140
010a00001e1451c1401e1401e145211401e1401e1451c1401e1401e1451a1401e1401e1451c1401e1401e1451f1400b1300c1300b1200b1200c1200b1200b1200c1200b1200b1300c130211400b1300c1300b130
010a00001e1451c1401e1401e145211401e1401e1451c1401e1401e1451a1401e1401e1451c1401e1401e1451f1401a1401a1450c1201a1401a1450c1201a1401a1450c1301a1401a145211401a1401a1450c140
010a0000151451314015140151450e1401514015145131401514015145121401514015145131401514015145221401a1401a145181401a1401a145161401a1401a145151401a1401a145211401a1401a14518140
010a0000151451314015140151450e140151401514513140151401514512140151401514513140151401514522140221402214518140181450000016140161450000015140151450000021140211402114518140
010a00000b1400c1400e1400b1400c1500b1500b1500c1502614022140221452114022140221451f14022140221452114022140221452614022140221452114022140221451f1402214022145211402214022145
010a00001a1401a1450e1401a1401a1450c1501a1401a1452614022140221452114022140221451f14022140221452114022140221452614022140221452114022140221451f1402214022145211402214022145
010a00001a1401a1452a1401a1401a145181401a1401a1451f1401a1401a145181401a1401a145161401a1401a145181401a1401a145131401a1401a145181401a1401a145161401a1401a145181401a1401a145
010a000018145000002a1402a1402a1451814018145000001f1401a1401a145181401a1401a145161401a1401a145181401a1401a145131401a1401a145181401a1401a145161401a1401a145181401a1401a145
010a0000221401f1401f1451e1401f1401f1451a1401f1401f1451e1401f1401f145221401f1401f1451e1401e145000001a1401f1401f1451e1401f1401f145211401e1401e1451c1401e1401e1451a1401e140
010a00002214016140161451514016140161451314016140161451514016140161450e14016140161451514015145000001314016140161451514016140161452114015140151451314015140151451214015140
010f000023140000002314000000211400000023140000000014000145000000000000000000000000000000001400014500000071450622500000171400000018140000001f1401f14500140001450000000000
010f0000231402314523140231453662500000231402314500140001450714007145001400014507140071450014000145071400714500140001451f1401f1451f1401f1451f1401f14500140001450714007145
00020000144601d470317703b77032470284701e4701a4701747013470116700f470135700e4600c4600b4600e6600b4500a450095500a4400944009440094400b64008430084300843009630074300642006420
000200000f4500e4600d4700d4500d44017440124400d4500b4600a4700a4700a460094600a4500b450144500d45009450074500646005460054500545006450104500e4600b460094600846008460084600d460
010f00001c140000001c140062201a140000001c14000000366250000009140091450014000145091400914536625000000914006220001400014515140000001714000000306253661536625000000914009145
010f0000211402114530625366151f1401f1453062536615306250000030625366153662500000306253661506220062253062536615366250000030625366153662500000181400622030625000003062536615
010f0000211400000021140000001f14000000211400000000140001450000000000000000000000000000000014000145000000914506225000001c140000001c140000001c1401c14500140001450000000000
010f0000306250000021140211453662500000211402114500140001450914009145001400014509140091450014000145091400914500140001451c1401c1451c1401c1451c1401c14500140001450914009145
010f00001d140000001d140062201c140000001d14000000366250000005140051450014000145051400514536625000000514006220001400014515140000001714000000306253661536625000000514005145
010f00001d1401d14530625366151f1401f1453062536615306250000030625366153662500000306253661506220062253062536615366250000030625366153662500000181400622030625000003062536615
010f0000211400000021140000001f14000000211400000000140001450000000000000000000000000000000014000145000000514506225000001d140000001d140000001d1401d14500140001450000000000
010f0000211402114521140211453662500000211402114500140001450514005145001400014505140051450014000145051400514500140001451d1401d1451d1401d1451d1401d14500140001450514005145
010f00001f140000001f140062201d140000001f1400000036625000000714007145001400014507140071453662500000071400622000140001451f140000001f14000000306253661536625000000714007145
010f00001f1401f145306253661521140211453062536615306250000030625366153662500000306253661506220062253062536615366250000030625366153662500000062200622030625000003062536615
010f000023140000002314000000211400000023140000000014000145000000000000000000000000000000001400014500000071450622500000171400000018140000001f1401f14500140001450000000000
00010000203502436024370213701a370173601e3501c34017340153501b3501b33017340123401734018330133300e3301032011320093200a320073200a3100001000000000000000000000000000000000000
000200002f66032660306702a63026610216501d6501965014650116500d6400a6500863007630066200561004600076000760006600056000560004600036000360003600026000160002600016000160001600
0002000029670216700e6702c670246600b6502763013660256301c640126300c6200864005620016200162001600076000760006600056000560004600046000460004600046000360002600026000160001600
000100000936012370203702f37024370133700b3700a370093700836008350073300734006350063500534005330043200432004320043100432004330043300434004340033400534004340043300433004330
00010000381503b1503b1603916035160321602e16028160231501c1501615014150111500f1500e1400c1400b1300b1300a1200a1200a1100a1100a1000d3001430013200142001320012000120000000000000
0002000037650386603667004660015602b5002050016500025000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200000b660106600c65007640016300161002600176000f6000960000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0002000006220092400a26009250082500625005240052300421005200022002220022200202002020020200202002020022200117000e7000d7000e7000f700036000360003600036000360004600046000d600
000100003f6603c750046000260002600026000160001600016000160002400024000160001600016000240002400034000240003400024000240002400024000240002400024000240002400024000240001400
000200002b1602a6602a160291602366020160166600b160071500514002130011200111001110011100111001610000000000000000000000000000000000000000000000000000000000000000000000000000
000200002c4402e460334502e4502d4502c440294502a43027430244202241021450144501f4501e4501e4501e4501f45024450384503240023400134001e40024400304002d400194001e400294002e4002d400
00030000263402f350323601b360113600235002300013002c3502235014320013000230021350143500735000000000000000000000000000000000000000000000000000000000000000000000000000000000
00030000186502f25017650242400d640012002f600296001e600156000d60008600116000e6000c6000960006600036000560004600036000260001600000000000000000000000000000000000000000000000
00020000085500d550065500255001550000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000100002c6702b6702a6702067015670116600464007630046100160006600026000160002600016000160004500035000350003500025000250002500025000150001500047000470004700047000470004700
__music__
01 00010203
00 04050607
00 08090a0b
01 0c0d0e0f
00 10111213
00 14141515
00 16161717
00 18191a1b
00 1c1d1e1f
00 0c0c0e0e
00 10101212
00 14141515
00 20202121
02 18191a1b
01 26272829
00 2a2b2c2d
02 2e2f2223

