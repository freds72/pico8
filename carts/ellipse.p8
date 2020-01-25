pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
local angle,dangle=0,0
local x,y=0,0
local v=0

function _update()
	if(btn(0)) dangle+=0.01
	if(btn(1)) dangle-=0.01

 if(btn(2))	v+=0.4
 
 angle+=dangle
 local dx,dy=cos(angle),-sin(angle)
 x+=v*dx
 y+=v*dy
 
	v*=0.8
	dangle*=0.8
end

function _draw()
	cls()
	
 -- ellipse
 local a,b=54,12
 local w=4
 local cc,ss=cos(angle),sin(angle)
	fillp(0xa5a5.ff)
	ellipsefill(64+x+a*cc-w*ss,64-y+a*ss+w*cc,a,b,angle-0.01,7)
	fillp(0x5a5a.ff)
	ellipsefill(64+x+a*cc+w*ss,64-y+a*ss-w*cc,a,b,angle+0.01,7)
  	 
	print(100*stat(1),2,2,1)
end
-->8
function ellipsefill(x0,y0,a,b,angle,c)
 local asq,bsq=a*a,b*b

 -- max. extent
 local ab=max(a,b)
	
	local cc,ss=cos(angle),sin(angle)
	local csq,ssq=cc*cc,ss*ss
	local rb0=2*cc*ss*(1/asq-1/bsq)
	local rc0=ssq/asq+csq/bsq
 local ra=csq/asq+ssq/bsq
	
	color(c)
	for y=max(y0-ab,0),min(y0+ab,127) do
	 -- roots
	 local yy=y-y0+0.5
	 local rb,rc=rb0*yy,yy*yy*rc0-1
	 
	 local r=rb*rb-shl(ra*rc,2)
	 if r==0 then
	 	pset(x0-shr(rb/ra,1),y)
	 elseif r>0 then
	 	r=r^0.5
	  rectfill(x0+shr(-rb+r,1)/ra,y,x0+shr(-rb-r,1)/ra,y)
	 end
 end
end
__gfx__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00700700000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
