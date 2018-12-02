pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
local points={}
local mouse={
	x=0,
	y=0,
	lmb=false,
	cursor=1,
	drag=false,
	dragx=0,
	dragy=0
}
-- binary export helpers
local int={}
function int:tostr(v)
	return sub(tostr(v,true),3,6)
end
local byte={}
function byte:tostr(v)
	return sub(tostr(v,true),5,6)
end

function points_tostr()
	local s=byte:tostr(128)
	for i=0,127 do
		s=s..byte:tostr(points[i] or 0)
	end
	return s
end

-- cursor
local mousec=1
function _draw()
	cls(0)
	fillp(0xa5a5)
	line(0,127,0,0,13)
	line(0,127,127,127,13)
	line(32,0,32,127,1)
	line(63,0,63,127,1)
	line(0,64,127,64,1)
	print("50%",116,66,1)
	line(0,32,127,32,1)
	print("75%",116,34,1)
	fillp()
	
	for i=0,127 do
		local p=points[i]
		if p then
			pset(i,127-p,7)
		else
			pset(i,0,8)
		end
	end
	
	if mouse.drag then
		local x0,y0=mouse.x,mouse.y
		local x1,y1=mouse.dragx,mouse.dragy
		line(x0,y0,x1,y1,1)
	end
	spr(mouse.cursor,mouse.x,mouse.y)
	
	local x,y=unproject(mouse.x,mouse.y)
	print(x.."/"..y,mouse.x+6,mouse.y,1)
end
function unproject(x,y)
	return x,127-y
end

function _update()
	local mx,my=mid(stat(32),0,127),mid(stat(33),0,127)
	local lmb,rmb=stat(34)==1,stat(34)==2
	
	mouse.cursor=1
	local drop=mouse.drag
	local drag=mouse.drag
	mouse.drag=false
	if lmb then
		mouse.drag=(mx==mouse.x and my==mouse.y)==true and false or true
	end 
	-- was dragging?
	drop=lmb==false and drop
	drag=mouse.drag==true and drag==false
	
	if drag==true then
		mouse.dragx,mouse.dragy=mouse.x,mouse.y
		mouse.cursor=2
	elseif drop==true then
		local xmin,ymin=unproject(mouse.dragx,mouse.dragy)
		local xmax,ymax=unproject(mouse.x,mouse.y)
		if xmin!=xmax then
 		if(xmin>xmax) xmin,xmax,ymin,ymax=xmax,xmin,ymax,ymin
 		local dy=(ymax-ymin)/(xmax-xmin)
 		local y=ymin
 		for i=xmin,xmax do
 			points[i]=y
 			y+=dy
 		end
 	end
	end
	
	mouse.x,mouse.y=mx,my
	mouse.lmb,mouse.rmb=lmb,rmb
end

function _init()
 -- mouse support
	poke(0x5f2d,1)
	
	-- export to clipboard
	menuitem(1,"export", function()
		printh(points_tostr(),"@clip")
		rectfill(0,120,127,127,8)
		print("export done.",2,121,0)
		for i=1,15 do
			flip()
		end
	end)
end

__gfx__
00000000110000001110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000171000001771000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700177100001717100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000177710000177710000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000171110000017771000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700010000000001710000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
