pico-8 cartridge // http://www.pico-8.com
version 9
__lua__
local before_update={c=0}
local after_draw={c=0}

local actors = {} --all actors in world

-- side
local good_side,bad_side,any_side=0x1,0x2,0x3

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
	if(c=='"') return val,pos+1
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
local table_delims={
	['{']="}",
	['[']="]"}
-- register json context here
local _g={
	['true']=true,
	['false']=false,
	['dmg_phys']=0x0100,
	['dmg_contact']=0x0200,
	['dmg_energy']=0x0400,
	['dmg_poison']=0x0800,
	['dmg_mask']=0xff
}

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
 	-- parse a string.
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
local plyr_playing
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
local weapons=json_parse('{"base_gun":{"id":1,"sx":48,"sy":8,"blt_frames":[42,42,42],"dmg_type":dmg_phys,"dmg":1,"spread":0.05,"v":0.1,"ttl":90,"dly":32,"ammo":50},"acid_gun":{"id":1,"blt_frames":[26,27,28],"blts":3,"spread":0.1,"bounce":true,"dmg_type":dmg_poison,"dmg":3,"v":0.1,"xy":[1,0],"ttl":30,"dly":5,"ammo":50},"uzi":{"id":2,"icon":21,"sx":32,"sy":8,"blt_frames":[10,12,11],"spread":0.05,"dmg_type":dmg_phys,"dmg":2,"v":0.4,"ttl":30,"dly":5,"ammo":50,"shk_pow":2},"shotgun":{"id":2,"icon":37,"sx":32,"sy":16,"blt_frames":[10,12,11],"spread":0.05,"blts":3,"dmg_type":dmg_phys,"dmg":2,"inertia":0.95,"v":0.3,"ttl":30,"dly":56,"ammo":50,"shk_pow":2}}')

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
local cur_level=4
local levels=json_parse('[{"name":"desert","ground_tiles":[68,64,65,67],"wall_tiles":[66],"solid_tiles_base":112,"bkg_col":1,"depth":3,"cw":32,"ch":32,"w":[4,6],"h":[4,6],"paths":[1,2],"path":{"bends":[1,2],"w":[1,2],"len":[2,3]}},{"name":"sewers","ground_tiles":[86,87,87,88],"wall_tiles":[90,89,91],"solid_tiles_base":112,"shadow_tile":94,"borders":[10,11,3],"bkg_col":3,"depth":4,"cw":32,"ch":32,"w":[4,6],"h":[4,6],"paths":[1,2],"path":{"bends":[1,2],"w":[1,2],"len":[2,3]}},{"name":"snow plains","ground_tiles":[70,71,72],"wall_tiles":[74],"solid_tiles_base":112,"shadow_tile":95,"borders":[1,12,7],"bkg_col":7,"depth":5,"cw":32,"ch":48,"w":[4,6],"h":[4,6],"paths":[1,2],"path":{"bends":[1,2],"w":[1,2],"len":[2,3]}},{"name":"palace","ground_tiles":[96,100],"wall_tiles":[97,98,99,108],"solid_tiles_base":112,"shadow_tile":101,"borders":[7,0,5],"bkg_col":9,"depth":5,"cw":32,"ch":48,"w":[4,6],"h":[4,6],"paths":[1,2],"path":{"bends":[1,2],"w":[1,2],"len":[2,3]}},{"name":"lab","ground_tiles":[102,105],"wall_tiles":[103,104,106],"solid_tiles_base":112,"shadow_tile":107,"borders":[6,7,5],"bkg_col":5,"depth":4,"cw":32,"ch":48,"w":[4,6],"h":[3,5],"paths":[4,4],"path":{"bends":[0,2],"w":[1,2],"len":[8,12]}}]')

local blts={len=0}
local parts={len=0}
local zbuf={len=0}
local time_t=0

local face2unit=json_parse('[[1,0],[0.6234,-0.7819],[-0.2225,-0.9749],[-0.901,-0.4338],[-0.901,0.4338],[-0.2225,0.975],[0.6234,0.7819],[1,0]]')
local face2rndunit=json_parse('[[1,0],[0.6234,-0.7819],[-0.2225,-0.9749],[-0.901,-0.4338],[-0.901,0.4338],[-0.2225,0.975],[0.6234,0.7819],[1,0],[[0.9998,-0.02146],[0.9917,-0.1289],[0.9999,-0.01495],[0.9932,0.1167],[0.9994,-0.03527],[0.9929,-0.1194],[0.9886,-0.1509],[0.9953,0.09727],[0.9987,-0.05059],[0.9881,-0.1539],[0.9996,-0.0276],[0.9967,0.0816],[0.9931,-0.1175],[0.9995,-0.03066],[0.9932,-0.1163],[0.995,0.1003],[0.9959,-0.09037],[1,0.009972],[0.9886,0.1505],[0.9976,0.06974],[0.992,-0.1266],[0.9959,0.09039],[0.9995,-0.0322],[0.9968,0.08045],[0.999,-0.046],[0.9935,-0.114],[0.9992,-0.04025],[0.9932,-0.1163],[1,-0.006126],[0.9911,0.1334],[0.9993,-0.03796],[0.992,0.1266]],[[0.5233,-0.8521],[0.7157,-0.6984],[0.6995,-0.7147],[0.5702,-0.8215],[0.4942,-0.8693],[0.5973,-0.8021],[0.6984,-0.7157],[0.5911,-0.8066],[0.5578,-0.83],[0.65,-0.76],[0.6932,-0.7208],[0.5581,-0.8298],[0.6482,-0.7614],[0.6724,-0.7402],[0.5789,-0.8154],[0.5511,-0.8344],[0.5171,-0.8559],[0.6854,-0.7282],[0.6653,-0.7466],[0.6125,-0.7905],[0.5898,-0.8075],[0.6246,-0.781],[0.6681,-0.744],[0.7293,-0.6842],[0.5282,-0.8491],[0.6879,-0.7258],[0.6161,-0.7876],[0.5645,-0.8254],[0.5581,-0.8298],[0.7379,-0.6749],[0.5495,-0.8355],[0.6532,-0.7572]],[[-0.3412,-0.94],[-0.3325,-0.9431],[-0.3166,-0.9486],[-0.1422,-0.9898],[-0.2811,-0.9597],[-0.1581,-0.9874],[-0.1973,-0.9803],[-0.2693,-0.9631],[-0.2135,-0.9769],[-0.1842,-0.9829],[-0.1289,-0.9916],[-0.1845,-0.9828],[-0.2634,-0.9647],[-0.1891,-0.982],[-0.09992,-0.995],[-0.177,-0.9842],[-0.357,-0.9341],[-0.3484,-0.9373],[-0.1395,-0.9902],[-0.2593,-0.9658],[-0.343,-0.9393],[-0.1292,-0.9916],[-0.09228,-0.9957],[-0.2359,-0.9718],[-0.2045,-0.9789],[-0.1205,-0.9927],[-0.1762,-0.9843],[-0.237,-0.9715],[-0.1426,-0.9898],[-0.3115,-0.9502],[-0.2859,-0.9583],[-0.2277,-0.9737]],[[-0.9133,-0.4073],[-0.8559,-0.5171],[-0.9258,-0.3781],[-0.9023,-0.431],[-0.8481,-0.5298],[-0.9466,-0.3224],[-0.933,-0.3599],[-0.9119,-0.4105],[-0.8934,-0.4493],[-0.8875,-0.4609],[-0.9454,-0.326],[-0.9544,-0.2983],[-0.9463,-0.3231],[-0.8706,-0.4919],[-0.8965,-0.4431],[-0.8555,-0.5177],[-0.924,-0.3823],[-0.8993,-0.4372],[-0.9565,-0.2917],[-0.8744,-0.4852],[-0.8239,-0.5667],[-0.9153,-0.4028],[-0.9331,-0.3595],[-0.9007,-0.4345],[-0.9023,-0.431],[-0.9196,-0.3929],[-0.9379,-0.347],[-0.9322,-0.362],[-0.9404,-0.3401],[-0.8372,-0.5469],[-0.852,-0.5236],[-0.9066,-0.422]],[[-0.9269,0.3752],[-0.9275,0.3738],[-0.9181,0.3965],[-0.8934,0.4493],[-0.8523,0.523],[-0.9397,0.3419],[-0.8987,0.4386],[-0.9101,0.4143],[-0.8823,0.4707],[-0.9165,0.4],[-0.9281,0.3724],[-0.9412,0.338],[-0.874,0.4859],[-0.9534,0.3016],[-0.9505,0.3108],[-0.8285,0.56],[-0.9359,0.3524],[-0.8442,0.536],[-0.8246,0.5657],[-0.8473,0.5311],[-0.8344,0.5511],[-0.9521,0.3057],[-0.8674,0.4976],[-0.8668,0.4986],[-0.8939,0.4482],[-0.8862,0.4633],[-0.9548,0.2973],[-0.8319,0.5549],[-0.9557,0.2943],[-0.8565,0.5161],[-0.9301,0.3674],[-0.889,0.4578]],[[-0.08044,0.9968],[-0.273,0.962],[-0.2026,0.9793],[-0.09037,0.9959],[-0.2833,0.959],[-0.1845,0.9828],[-0.2811,0.9597],[-0.1664,0.9861],[-0.1114,0.9938],[-0.2526,0.9676],[-0.077,0.997],[-0.1804,0.9836],[-0.1623,0.9867],[-0.2734,0.9619],[-0.08503,0.9964],[-0.3667,0.9304],[-0.2903,0.9569],[-0.2232,0.9748],[-0.3534,0.9355],[-0.3042,0.9526],[-0.3699,0.9291],[-0.1581,0.9874],[-0.2478,0.9688],[-0.1441,0.9896],[-0.2656,0.9641],[-0.3595,0.9331],[-0.3702,0.9289],[-0.08694,0.9962],[-0.0682,0.9977],[-0.3115,0.9502],[-0.07777,0.997],[-0.3159,0.9488]],[[0.611,0.7916],[0.6228,0.7824],[0.5527,0.8334],[0.6679,0.7443],[0.632,0.775],[0.5314,0.8471],[0.6954,0.7187],[0.6015,0.7989],[0.6207,0.7841],[0.5607,0.8281],[0.6285,0.7779],[0.6856,0.7279],[0.6168,0.7872],[0.6976,0.7165],[0.7019,0.7122],[0.6741,0.7386],[0.544,0.8391],[0.6859,0.7277],[0.7157,0.6984],[0.6558,0.755],[0.6249,0.7807],[0.5181,0.8554],[0.6171,0.7869],[0.56,0.8285],[0.7168,0.6973],[0.7371,0.6758],[0.5607,0.8281],[0.6161,0.7877],[0.5035,0.864],[0.5543,0.8323],[0.5239,0.8518],[0.5379,0.843]],[[0.992,-0.1262],[0.9929,0.1194],[0.9999,-0.01571],[0.9956,0.09382],[0.9951,-0.09916],[0.9984,-0.05711],[0.9998,0.02186],[0.9968,0.08007],[0.9936,0.1129],[0.999,-0.04485],[0.9919,0.1274],[0.9996,0.02799],[0.9961,0.08848],[0.9919,0.1274],[0.9882,-0.1532],[0.9988,-0.04906],[0.9983,-0.05825],[0.9926,-0.1213],[0.9947,0.1034],[0.9981,0.06095],[0.9992,-0.0391],[0.9931,0.1171],[0.9953,0.09727],[0.9998,0.01879],[0.9998,-0.01917],[0.9986,-0.05366],[0.9982,0.05941],[0.9999,-0.01456],[1,0.008827],[0.998,0.06324],[0.9878,-0.1558],[0.998,-0.06284]]]')

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
function txt_options(c,s)
	txt_center=c or false
	txt_shade=s or -1
end
function txt_print(s,x,y,col)
	if txt_center then
		x-=flr((4*#s)/2+0.5)
	end
	if txt_shade!=-1 then
		print(s,x+1,y,txt_shade)
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
	zbuf.len+=1
	zbuf[zbuf.len]={obj,{xe,ye},key=ye}
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
		h=flr(obj.x)+128*flr(obj.y)
		cmap[h]=cmap[h] or {}
		add(cmap[h],obj)
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
function make_part(x,y,dly)
	local p={
		x=x,y=y,
		dx=0,dy=0,
		inertia=0,
		dly=dly,
		t=time_t+dly,
		update=update_part
	}
	parts.len+=1
	parts[parts.len]=p
	return p
end
function make_static_spr_part(x,y,spr,sw)
	local p={
		x=x,y=y,
		sw=sw or 1,
		update=update_part,
		draw=draw_spr_part
	}
	parts.len+=1
	parts[parts.len]=p
	return p
end
function make_flash_part(x,y,r)
	local p={
		x=x,y=y,
		r=r or 1,
		t=time_t+4,
		update=update_static_part,
		draw=function(self,x,y)
			local r=self.r*(self.t-time_t)/4
			circfill(x,y,8*r,7)
		end
	}
	parts.len+=1
	parts[parts.len]=p
	return p
end

function update_static_part(self)
	if(self.t<time_t) return false
	zbuf_write(self)
	return true
end

function update_part(self)
	if(self.t<time_t) return false
	self.x+=self.dx
	self.y+=self.dy
	self.dx*=self.inertia
	self.dy*=self.inertia
	zbuf_write(self)
	return true
end
function draw_circ_part(self,x,y)
	local t=flr(#self.ramp*(self.t-time_t)/self.dly)
	local c=self.ramp[t+1]
	circfill(x,y,8*self.r,c)
end
function draw_spr_part(self,x,y)
	local sw=self.sw
	spr(self.spr,x-4*sw,y-4*sw,sw,sw)
end

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
				a:hit(self.dmg)
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
		local ang=a.angle+wp.spread*(rnd(2)-1)
		local u,v=cos(ang),sin(ang)
		local b={
			x=a.x+0.5*u,y=a.y+0.5*v,
			wp=wp,
			dx=wp.v*u,dy=wp.v*v,
			t=time_t+wp.ttl,
			side=a.side,
			facing=flr(8*(ang%1)),
			update=blt_update,
			draw=draw_blt
		}
		-- muzzle flash
		if(i==1) make_flash_part(b.x,b.y,0.5)
		-- for fast collision
		b.prevx,b.prevy=b.x,b.y
		blts.len+=1
		blts[blts.len]=b
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

function hit_actor(self,dmg)
	self.hit_t=time_t+8
	self.hp-=band(self.dmg_mask,dmg)
	if self.hp<=0 then
		del(actors,self)
	end
end

function make_blast(x,y)
	pause_t=4
	local p=make_actor(x,y,4)
	p.w=0.8
	p.dmg=bor(dmg_phys,15)
	p.side=any_side
	p.t=time_t+12
	p.ttl=12
	p.frames=blast_frames
	p.draw=draw_anim_spr
	p.update=function(a)
		if(a.t<time_t) del(actors,a)
	end
	p.hit=nop
	return p
end

function make_barrel(x,y) 
 local barrel=make_actor(x,y)
 barrel.side=no_side
 barrel.inertia=0.8
 barrel.spr=128
 barrel.side=all_side
 barrel.hit=function(self,dmg)
		if(band(dmg_contact,dmg)!=0) return
		self.hit_t=time_t+8
		self.hp-=1--band(dmg_mask,dmg)
		if self.hp<=0 then
			make_blast(self.x,self.y)
			del(actors,self)
		end
	end
	return barrel
end
function make_sandman(x,y)
	local bad_guy=make_actor(x,y)
	bad_guy.hp=5
	bad_guy.wp=weapons.base_gun
	bad_guy.frames={
		{4,5,6}
	}
 bad_guy.move_t=0
 bad_guy.update=function(self)
 	if lineofsight(self.x,self.y,plyr.x,plyr.y,4) then
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
 		self.move_t=time_t+30
 	end
 end
	return bad_guy
end

function make_scorpion(x,y)
	local scorpion=make_actor(x,y)
	scorpion.w=1.8
	scorpion.hp=10
	scorpion.frames={
	 	{135,137}
	 }
 scorpion.move_t=0
 scorpion.update=function(self)
 	if self.move_t<time_t then
 		self.dx,self.dy=0.05*(rnd(2)-1),0.05*(rnd(2)-1)
 		self.move_t=time_t+30
 	end
 end
	return scorpion
end
function make_worm(x,y)
 local worm=make_actor(x,y)
 worm.palt=3
 worm.w=0.2
 worm.inertia=0.8
 worm.dmg=bor(dmg_contact,1)
 worm.frames={
 	{7,8}
 }
 worm.move_t=0
 worm.update=function(self)
 	if self.move_t<time_t then
 		self.dx,self.dy=0.05*(rnd(2)-1),0.05*(rnd(2)-1)
 		self.move_t=time_t+30
 	end
 end
return worm
end
function make_warp(x,y)
	local warp=make_actor(x,y)
	warp.draw=nop
	warp.hit=nop
	warp.w=0
	warp.captured=false
	warp.frames={80,81,82,83,84}
	warp.update=function(self)
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
end

-- actor
-- x,y in map tiles (not pixels)
function make_actor(x,y)
 local a={
	 x=x,
	 y=y,
	 dx=0,
	 dy=0,
	 frame=0,
	 inertia=0.6,
	 bounce=1,
	 hp=1,
	 hit_t=0,
	 fire_t=0,
	 fire_dly=0,
	 w=0.4, -- actors are round!
	 angle=0,
	 facing=0, -- trig order e/n/w/s
	 side=bad_side,
	 draw=draw_actor,
	 hit=hit_actor
 }
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
	
 if not solid_a(a,a.dx,0) then
  a.x+=a.dx
 else   
  -- otherwise bounce
  a.dx*=-a.bounce
  sfx(2)
 end

 -- ditto for y
 if not solid_a(a,0,a.dy) then
  a.y+=a.dy
 else
  a.dy*=-a.bounce
  sfx(2)
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
	local wp=a.wp
	-- shadow
	spr(16,sx,sy+7)
	-- hit effecit
	if a.hit_t>time_t then
		for i=0,15 do pal(i,7) end
	end
 local s,flipx,flipy=a.spr,false,false
 if a.frames then 
 	local spr_options=(#a.frames==3 and face3strip or face1strip)[a.facing+1]
		local frames=a.frames[spr_options.strip or 1]
		s=frames[flr(a.frame%#frames)+1]
		flipx,flipy=spr_options.flipx,spr_options.flipy
	end
	-- actor
	palt(a.palt or 14,true)
	spr(s,sx,sy,sw,sw,flipx,flipy)
	palt(a.palt or 14,false)
	pal()
 palt(0,false)
 palt(14,true)
	if wp then
		local u,v=cos(a.angle),sin(a.angle)
		-- recoil animation
		local f=-2*max(0,a.fire_t-time_t)/8
		rspr(wp.sx,wp.sy,sx+4*u+f*u,sy+4*v+f*v,1-a.angle)
	end
end

-- player actor
function make_plyr()
	plyr_score=0
	plyr_playing=true
	plyr=make_actor(18,18)
	plyr.hp=5
	plyr.hpmax=5
	plyr.side=good_side
	-- todo: rename to strips
	plyr.frames=plyr_frames
	plyr.wp=weapons.uzi
	plyr.safe_t=time_t+30
	return plyr
end

function control_player()
	local wp,angle=plyr.wp,plyr.angle
 -- how fast to accelerate
 if(btn(0)) plyr.dx-=plyr_acc angle=0.5
 if(btn(1)) plyr.dx+=plyr_acc angle=0
 if(btn(2)) plyr.dy-=plyr_acc angle=0.25
 if(btn(3)) plyr.dy+=plyr_acc angle=0.75
	
	if wp and btn(4) and plyr.fire_dly<time_t then
	 -- todo: rename recoil
		plyr.fire_t=time_t+8
		plyr.fire_dly=time_t+wp.dly
		make_blt(plyr,wp)
		local u=face2unit[plyr.facing+1]
		plyr.dx-=0.05*u[1]
		plyr.dy-=0.05*u[2]
		cam_shake(u[1],u[2],wp.shk_pow)
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
	-- todo spawn entities
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
	rectfill(3,3,flr(32*hp/plyr.hpmax),7,8)
	txt_options(false,0)
	txt_print(hp.."/"..plyr.hpmax,12,3,7)

	palt(14,true)
	palt(0,false)
	spr(plyr.wp.icon,2,10)
	txt_print("34",14,12,7)
	
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
	if not plyr then
		plyr=make_plyr()	
	end
	local r=rooms[1]
	plyr.x=r.x+r.w/2
	plyr.y=r.y+r.h/2
	cam_track(plyr.x,plyr.y)
	
	make_warp(12,12)
end

function spawner(n,fn)
	for i=1,n do
		local x,y=0,0
		local ttl=5
		while(solid(x,y) and ttl>0) do
			x,y=flr(rnd(16)),flr(rnd(16))
			ttl-=1
		end
		if(ttl<0) return
		-- found empty space!
		fn(x+0.5,y+0.5)
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
function warp_screen:draw()
	cls(0)
	local x,y,y2,a,r,u,v
	for y=0,31,2 do
		y2=y*y
		for x=0,31,2 do
			a=4*atan2(y,x)
			r=sqrt(x*x+y2)
			u=gr*r+ga
			v=flr(4+4*cos(u+a))
			bpset(31+x,31-y,v)
			v=flr(4+4*cos(u+2-a))
			bpset(31-x,31-y,v)
			v=flr(4+4*cos(u+a+2))
			bpset(31-x,31+y,v)
			v=flr(4+4*cos(u+4-a))
			bpset(31+x,31+y,v)
		end
	end
		
	x,y=cos(time_t/64),sin(time_t/64)
	rspr(8,24,64+16*x,64+16*y,time_t/16)
	
	txt_options(true,0)
	txt_print("next: "..levels[cur_level].name,64,12,7)
end
function warp_screen:init()
	ga,gia,gr=0,.01,.01
end

local title_screen={}
function title_screen:update()
end
function title_screen:draw()
	cls(1)
	txt_options(true,3)
	txt_print("nuklear",64,2,11)
	txt_options(true,4)
	txt_print("klone",64,12,7)
end
function title_screen:init()
	cur_level=1
end

-- game loop
function _update60()
	time_t+=1
	futures_update(before_update)
	sm_update()
end
function _draw()
	sm_draw()
	futures_update(after_draw)
end
function _init()
	cls(0)
	sm_push(warp_screen)
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
e111111eee00000eee00000eee00000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000
11111111e0999aa0e09999a0e0999990eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee000eeeee777eeeeeeeeeeeeeeee33eeeeeeeeeee000000000000000000000000
e111111e099414100999414009999410eeeeeeeeeeeeeeeee0000000e77777770bb0000070077777ee3bb3eeee3bb3eeeee3bbee000000000000000000000000
eeeeeeee094444400994444009994440ee00000eee77777ee0b333b0e700000703b6606070000707e3bbbb3eeebbbbeeee3bbbee000000000000000000000000
eeeeeeee044455500444455004444450ee000eeeee707eeee0113110e70000070335505070000707e3bbbb3eeebbbbeeeebbb3ee000000000000000000000000
eeeeeeee0333bab003333ba0033333b0eee0eeeeeee7eeeee0000000e77777770550000070077777ee3bb3eeee3bb3eeeebb3eee000000000000000000000000
eeeeeeee05000050e050050ee005500eeee0eeeeeee7eeeeeeeeeeeeeeeeeeee0660eeee7007eeeeeeeeeeeeeee33eeeeeeeeeee000000000000000000000000
eeeeeeeee0eeee0eee0ee0eeeee00eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000eeee7777eeeeeeeeeeeeeeeeeeeeeeeeeeee000000000000000000000000
ee000eeeee0000eeee0000eeee0000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000
e0bbb0eee0999a0ee099aa0ee0999a0eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000eeeeeeeeee6bb6ee00000000000000000000000000000000
e07770ee099999a009999aa0099999a0ee000000ee777777eeee0e0eeeee7e7e0000000000000000eeeaaeeee6bb776e00000000000000000000000000000000
e03330ee099999a009999aa0099999a0e0496660e7000007ee001010ee7707070000000000000000eea77aeeebbbb7be00000000000000000000000000000000
e03a30ee044444400444444004444440e0445550e7000007e055c1c0e70000070000000000000000eea77aeeebbbbbbe00000000000000000000000000000000
e03930ee03333bb00333bbb003333bb0e0400000e7077777e0501010e70707070000000000000000eeeaaeeee6bbbb6e00000000000000000000000000000000
e00000ee050000500500000000000050ee0eeeeeee7eeeeeee0e0e0eee7e7e7e0000000000000000eeeeeeeeee6bb6ee00000000000000000000000000000000
e11111eee0eeee0ee0eeeeeeeeeeee0eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000eeeeeeeeeeeeeeee00000000000000000000000000000000
ee000eeeee0000eeee0000eeee0000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000eeeeeeee0000000000000000000000000000000000000000
e06660eee0999a0ee0999a0ee0999a0eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000ee88eeee0000000000000000000000000000000000000000
e07770ee094141a0091414a0094141a0ee00000eee77777eee000000ee7777770000000000000000e000000e0000000000000000000000000000000000000000
e0d8d0ee094444900944449009444490e076670ee700007ee03bb660e70000070000000000000000e08877700000000000000000000000000000000000000000
e08880ee044555400455544004455540e055000ee700777e0453b000700007770000000000000000e05566700000000000000000000000000000000000000000
e0d8d0ee033babb00339bbb0033babb0e050eeeee707eeee04400eee70077eee0000000000000000e000000e0000000000000000000000000000000000000000
e00000ee05000050000000b003000000ee0eeeeeee7eeeeee00e0eeee77e7eee0000000000000000ee88eeee0000000000000000000000000000000000000000
e11111eee0eeee0eeeeeee0ee0eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000eeeeeeee0000000000000000000000000000000000000000
444444444444444404040404444444444444444444444444777777777777777777777677777677775c775c5c76666667dddddddddddddddd121212eed2dddddd
44444444449944444040404044444444444444444940044077777777777777777667777777657777ccc7c7c565151516ddddddddd1eddddd21ee21de20ddd2ed
44b4b4444549544404040404494444444444444450450945777777777766667775577777765777771cc7c77c71515177ddddddddd11ddddd11dde212ddd0d02d
435b5344445544444040404045444494444444440444504477777777765555777777777675677777c111ccc565151777dddddddddddddddd21dde121dd02dd0d
453535444444444404040404444444544444444445094544777777777555556777777777775677775c5cc77c51515667ddddddddddddeedd12111212d02d0ddd
44555444444444444040404044494444444444444450949477777777775555577777677777657777c5c5c1c775151557ddddddddddd12e1d2121de21dd0dd0dd
44444444444444440505050544454444444444444440040477777777777755577777777777577777515c7ccc77515717dddddddddddd11dd12121d12d2dd02d0
44444444444444445454545444444444444444440445544477777777777777777777777777777777c115c7c577777777dddddddddddddddd2121212100dd2ddd
cccccccc555555555555555555555555555555550000000055555555555555555666666537555575313131313535353500000000000000001111111111111111
c505050ccccccccc0505050505050505050505050000000055555555555555456000000656777763131313135377775300000000000000005151515171717171
c000000cc000000c0cccccc000000000000000000000000055555555555555556333333635666655313131313700007500000000000000001515151517171717
c000000cc000000c0c0000c000000000000000000000000055555555555555556555555653555553131313135600006300000000000000005555555577777777
c000000cc000000c0c0000c000555500000000000000000055555555544555556333333637555575313131313622206500000000000000005555555577777777
c000000cc000000c0c0000c0005005000000000000000000555555555445555565555556567777631313131355eee65300000000000000005555555577777777
c000000cc000000c0c0000c000500500000550000000000055555555555554557666666735666655313131313522553500000000000000005555555577777777
ccccccccc000000c0c0000c000500500000550000000000055555555555555555777777553555553131313135322535355555555555555555555555577777777
666166669995999999000009906000606660666600000000dddd11116666666667676666dddd11116dddddd65555555599959999999599990000000000000000
661516664495444440445440402222206605066611010111dddd11116555555665656666dddd11116dd77dd6111100004aaaa774449544440000000000000000
615551665555555550095900508000806666666610111011dddd11116000000665656666dddd00116d7667d6111100005acccc75555555550000000000000000
155555169999959990440440908080800066606655555556dddd111160b0280665656666dddddd006d6666d6dddd11119a333ca9999995990000000000000000
6555556644449544409565904088888065600566655555661111dddd6000000665656666111ddddd6d5665d61111dddd4a3333a4444495440000000000000000
6655566655555555500454005088088066655666665556661111dddd66777766656566661111dddd6dd55dd61111dddd5aaaaaa5555555550000000000000000
6665666699959999909959909020502066656666666566661111dddd66666666656566660111dddd6dddddd61111dddd92212229999599990000000000000000
6666666644954444400000004001110066666666666666661111dddd6666666660606666d000dddd667777661111dddd44954444449544440000000000000000
aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa9111111991111111911111199111111111111119111111111111111911111111
a111111aa1111111a111111aa11111111111111a111111111111111a111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
91111119911111119111111991111111111111191111111111111119111111119111111991111111911111199111111111111119111111111111111911111111
99999999999999999111111991111111999999999999999911111119111111119999999999999999911111199111111199999999999999991111111911111111
eeeeeeee000000000000000000000000000000000000000000000000eeeee00000eeeeeeeeeeeee00000eeee0000000000000000000000000000000000000000
ee0000ee000000000000000000000000000000000000000000000000eeee02121200eeeeeeeeee02121200ee0000000000000000000000000000000000000000
e07bb70e000000000000000000000000000000000000000000000000eee0700212110eeeeeeee0700212110e0000000000000000000000000000000000000000
e0b77b0e000000000000000000000000000000000000000000000000eeee0ee0000220eeeeeeee0ee00022200000000000000000000000000000000000000000
e03bb30e000000000000000000000000000000000000000000000000eeeeeeeeeee010eeeeeeeeeeeee011200000000000000000000000000000000000000000
e0b77b0e000000000000000000000000000000000000000000000000eeeeeeee0002210eeeeeeeee000222100000000000000000000000000000000000000000
e03bb30e000000000000000000000000000000000000000000000000eeeeeee02211220eeeeeeee02211220e0000000000000000000000000000000000000000
ee0000ee000000000000000000000000000000000000000000000000eeee0001122210eeeeee0001122210ee0000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000eee01122212000eeeee0112221200eee0000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000ee0822220002220eee082222000220ee0000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000ee02282002200020ee022820022020ee0000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000e070202020022001e07020202020020e0000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000ee00700102002011ee0070002020101e0000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000ee1101111020011eee1101020102011e0000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000e111111111011eeee1111110111011ee0000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee0000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000300003330000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000021270330000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000010003330000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000020330030000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000010008200000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000022222700000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000020202030000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000303030330000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001010101018201010101010101010101828282828200010101010101000001010101010101010101010001010101000082828282828282828282828282828282
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__map__
5555555555555555555555555545454545454545454500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5555555555556e6e6e6e6e6e4545454545515151454500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
555c5c5c5c5c6e6e6e6e6e6e4545454550424242524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5067676768676768676767675145454560434343524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
506b6b6b6b6b6b6b666666424261516042444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5066666666696666666666434342424243444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5066666966666666696666444443434344444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5040444444444444444044444444444144444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444444444444444463535362444444444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444444444441635345454560444144444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444440444444615151516042404444444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444444444444424242424243444444444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
5044444444444444434343434344444444444444524500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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

