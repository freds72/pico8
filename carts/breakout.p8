pico-8 cartridge // http://www.pico-8.com
version 16
__lua__
local ball={
	w=2,
	pos={0,0},
	v={0,1},
	vel=32,
	draw=function(self)
 	local x,y=project(self.pos)
		circfill(x+1,y+1,2,0)
		spr(1,x-1,y-1)
	end
	}

function make_block(pos)
 local block={
  pos=pos,
  w=8,
  collide=false,
  side=-1,
  draw=function(self)
  	local x,y=project(self.pos)
  	rectfill(x-self.w,y-self.w,x+self.w,y+self.w,self.collide==true and 9 or 8)
   if self.side==0 then
   	line(x-self.w,y-self.w,x+self.w,y-self.w,7)
   elseif self.side==1 then
   	line(x+self.w,y-self.w,x+self.w,y+self.w,7)
   elseif self.side==2 then
   	line(x-self.w,y+self.w,x+self.w,y+self.w,7)
   elseif self.side==3 then
   	line(x-self.w,y-self.w,x-self.w,y+self.w,7)
   end
  end,
  update=function(self)
  	self.side=-1
  	local collide=circbox_coll(ball.pos,ball.w,self.pos,self.w,self.w)
   if collide==true then
   	-- side?
   	local x,y=ball.pos[1]-self.pos[1],ball.pos[2]-self.pos[2]
 			local pos,v=ball.pos,ball.v
   	if y>=abs(x) then
   		-- from above
   		self.side=0
   		-- fix pos
 			 pos[2]=self.pos[2]+self.w+ball.w
 			 v[2]=-v[2] v[1]+=rnd(1)-0.5
   	elseif y<=-abs(x) then
   	 -- from below
   	 self.side=2
   		-- fix pos
 			 pos[2]=self.pos[2]-self.w-ball.w
 			 v[2]=-v[2] v[1]+=rnd(1)-0.5
   	elseif x>=abs(y) then
   		-- from right
   	 self.side=1
   		-- fix pos
 			 pos[1]=self.pos[1]+self.w+ball.w
 			 v[1]=-v[1] v[2]+=rnd(1)-0.5
   	else
   	 -- from left
   	 self.side=3
 			 pos[1]=self.pos[1]-self.w-ball.w
 			 v[1]=-v[1] v[2]+=rnd(1)-0.5
   	end
   	v_normz(v)
   end
   self.collide=collide
  end
  }
	return block
end

function circbox_coll(a,ra,b,w,h)
	local dx,dy=a[1]-mid(a[1],b[1]-w,b[1]+w),a[2]-mid(a[2],b[2]-h,b[2]+h)
	if(abs(dx)>ra or abs(dy)>ra) return
	local d2=dx*dx+dy*dy
	return (d2<ra*ra)
end

function project(v)
	return 64+v[1],64-v[2]
end
	
local blocks={}
function _init()
	for i=-3,3 do
		add(blocks,make_block({i*18,48}))
	end
	for i=-2,2 do
		add(blocks,make_block({i*18,24}))
	end
end

function _update()
	local pos,v=ball.pos,ball.v
	pos[1]+=ball.vel*v[1]/30
	pos[2]+=ball.vel*v[2]/30

 if(pos[1]<-64) pos[1]=-64 v[1]=-v[1] v[2]+=rnd(1)-0.5
 if(pos[2]<-64) pos[2]=-64 v[2]=-v[2] v[1]+=rnd(1)-0.5
 if(pos[1]>64) pos[1]=64 v[1]=-v[1] v[2]+=rnd(1)-0.5
 if(pos[2]>64) pos[2]=64 v[2]=-v[2] v[1]+=rnd(1)-0.5
 v_normz(v)

 for _,b in pairs(blocks) do
	 b:update()
	end
end

function _draw()
	cls(1)
	print(ball.pos[1].."/"..ball.pos[2],2,2,7)
	
	for _,b in pairs(blocks) do
		b:draw()
	end
	
	ball:draw() 
	 
 camera()
end
-->8
function v_normz(a)
	local dx,dy=a[1],a[2]
	local d=dx*dx+dy*dy
	if d>0 then
		d=sqrt(d)
		a[1]/=d
		a[2]/=d
	end
end
__gfx__
00000000066000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000067c500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
007007006cc500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00077000055000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
