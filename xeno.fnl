; title:  xeno/amob
; author: ergodicbreak
; desc: made for Lisp Game Jam Spring 2020  
; script: fennel

(fn any? [pred obj coll]
 (var result nil)
	(var i 1)
	(var n (length coll))
	(var done? false)
	(while (not done?)
	 (if
		 (and (<= i n) (not result))
			 (do 
				 (set result (pred obj (. coll i)))
					(set i (+ i 1)))
			(set done? true)))
	result)
	
(fn one-of [coll item]
 (when (any? #(= $1 $2) item coll)
	 true))
		
(fn index-of [obj coll]
 (var idx  nil)
 (each [i v (ipairs coll)] 
	 (when (= obj v) (set idx i)))
	idx)
	
(local x-min 0)
(local x-max 240)
(local y-min 0)
(local y-max 136)

(local up {:d 1 :dx 0 :dy -1})
(local down {:d 2 :dx 0 :dy 1}) 
(local left {:d 3 :dx -1 :dy 0})
(local right {:d 4 :dx 1 :dy 0})
(local dirs [up down left right])

(local bg 5)

(var t 0)

(local logs [])
(var logs? false)

(fn log [msg]
 (when (> (length logs) 10)
  (table.remove logs 10))
 (table.insert logs 1 (.. msg ": " t "\n")))

(local modes [])
(local events [])

(local p
 {:x 194 :y 65 :img 256 :d 1  :speed 1.05 
	 :mov? false :energy 10})

(local es
	[{:x 40 :y 10 :img 320 :d 4 :speed 1 :mov? false :dt 30} 
	 {:x 60 :y 20 :img 320 :d 4 :speed 1.05 :mov? false :dt 60}])

(local r-ps 
 [{:x 176 :y 112 :img 16 :d 1 :mov? true :dt 60}])

(local s-ps
 [{:x 16 :y 16 :img 32 :d 1 :mov? true :dt 60}])
	  
(local d-us [])

(fn spawn-pos [thing] 
 (let
	 [dir (. dirs thing.d)
		 nx (- thing.x (* 8 dir.dx))
			ny (- thing.y (* 8 dir.dy))]
		[nx ny]))

(fn make-e [host]
 (let [[x y] (spawn-pos host)] 
  {:x x :y y :img 320 :d 4 :speed 1 
		 :mov? false :dt (math.random 30 60)}))
	
(fn make-d-u []
 (let [[x y] (spawn-pos p)]
  {:x x :y y :img 0 :d 1
	  :mov? true :dt (math.random 30 180)}))
		
(fn spawn [coll f ...]
 (let [obj (f ...)]
	 (if 
		 (and 
			 (not (= obj.img 128))
		  (<= x-min obj.x x-max)
			 (<= y-min obj.y y-max))
	    (table.insert coll obj)
			(= obj.img 128)
			  (table.insert coll obj))))

(fn verts [obj nx ny]
 (let
	 [x (or nx obj.x) y (or ny obj.y) 
		 xx (+ x 7) yy (+ y 7)]
		{:ax x :ay y :bx xx :by y  
			:cx x :cy yy :dx xx :dy yy}))	
			
(fn hit [obj1 obj2]
 (let 
	 [dir (. dirs obj1.d)
		 nx (+ obj1.x dir.dx (* dir.dx obj1.speed)) 
		 ny (+ obj1.y dir.dy (* dir.dy obj1.speed))
		 box1 (verts obj1 nx ny)
		 box2 (verts obj2)]
 (if
	 (= obj1 obj2) 
		 false
		(or (and  
	  (<= box2.ax box1.ax box2.bx) 
			(<= box2.ay box1.ay box2.cy))
			;
	  (and  
	   (<= box2.ax box1.bx box2.bx) 
			 (<= box2.ay box1.by box2.cy))
			;
	  (and  
	   (<= box2.ax box1.cx box2.bx) 
			 (<= box2.ay box1.cy box2.cy))
			;
	  (and  
	   (<= box2.ax box1.dx box2.bx) 
			 (<= box2.ay box1.dy box2.cy)))
		obj2
	false)))	

(fn collide [obj] 
		(or 
			(any? hit obj d-us)
			(any? hit obj es)
		 (and (one-of es obj) (hit obj p))
			(and (= obj p) (any? hit obj s-ps))
			:none))

(fn new-pos! [obj dir] 
	(set obj.x (+ obj.x dir.dx (* dir.dx obj.speed)))
	(set obj.y (+ obj.y dir.dy (* dir.dy obj.speed)))
	(set obj.mov? true))

(fn mpos [obj]
	(let 
	 [dir (. dirs obj.d)
		nx (+ obj.x  dir.dx (* dir.dx obj.speed) 4) 
		ny (+ obj.y  dir.dy (* dir.dy obj.speed) 4)]
	(values (// nx 8) (// ny 8))))
	
(fn move [obj dir]
 (set obj.d dir.d)
	(local hit (collide obj))
	(if 
		(>= (mget (mpos obj)) 128)
   nil
		(and (= obj p) (one-of d-us hit))
		 (do 
				(table.remove d-us (index-of hit d-us))
				(set p.energy (+ p.energy 1))
			 (new-pos! obj dir))
		(and (= obj p) (one-of s-ps hit))
		 (table.insert events :win)
		(and (one-of es obj) (one-of d-us hit))
   nil
	 (and (= obj p) (one-of es hit))
		 (table.insert events :lose)
		(and (one-of es obj) (= hit p))
		 (table.insert events :lose)
		(and (one-of es obj) (one-of es hit))
		 (do 
			 (new-pos! obj dir) 
			 (let [n (math.random)] 
				 (if 
					 (> n .96) 
						 (spawn es make-e obj) 
						(and (< n .04) (> (length es) 2))
						 (table.remove es (index-of hit es)))))
		(= hit :none)
	  (new-pos! obj dir)))
				
(fn frame [obj dt]
 (let 
	 [cur (// (% t (* dt 3)) dt)
	  a (* (- obj.d 1) 16)]
		(if 
		 (not obj.mov?)
			 (+ a obj.img)
		 (+ a cur obj.img))))
			
(local play {
:input
(fn []
 (if
	 (btnp 5) (set logs? (not logs?))
		(btnp 4) 
		 (when (> p.energy 0) 
			 (do 
				 (spawn d-us make-d-u)
					(set p.energy (- p.energy 1))))
	 (btn 0) (move p up)
		(btn 1) (move p down)
		(btn 2) (move p left)
		(btn 3) (move p right)
	 (set p.mov? false)))

;play
:update 
(fn []
 (each [_ e (ipairs es)] 
  (if
	  (> (math.random) 0.90)
		  (set e.mov? false)
	  e.mov? 
	   (move e (. dirs e.d))
   (> (math.random) 0.90)
    (let 
	    [dir (. dirs (math.random 1 4))]
		   (move e dir)))))
					
; play	
:draw 
(fn []
	(map)
	(each [_ v (ipairs s-ps)]
	 (spr (frame v v.dt) v.x v.y bg))
	(each [_ v (ipairs r-ps)]
	 (spr (frame v v.dt) v.x v.y bg))
	(spr (frame p 10) p.x p.y bg)
	(each [_ e (ipairs es)]
	 (spr (frame e 30) e.x e.y bg))
	(each [_ v (ipairs d-us)]
	 (spr (frame v v.dt) v.x v.y bg))
	(if logs?
	 (for [i 0 9] 
	  (print (. logs (+ i 1)) 10 (+ 10 (* i 8))12))))

; play
:sound 
(fn []
	(when p.mov? 
		(sfx 00 "c-1" 4 0 1))
	(each [_ e (ipairs es)] 
	 (when 
		 (and e.mov? (= 0 (% t e.dt)))
	   (sfx 01 "c-3" 30 1 2)))
	(each [_ v (ipairs d-us)]
	 (when (= 0 (% t v.dt))
		 (sfx 02 "c-5" 16 2 2))))
			
;play
:music
(fn [])

;play
:music? :stop})

(local attract {
:input
(fn []
 (if 
	 (btnp 4) 
		 (do 
		  (table.remove modes 1))))

;attract
:update 
(fn [])

;attract
:draw 
(fn []
 (cls bg)
	(spr 432 40 40 bg 2 0 0 10 2)
	(print "press 'z' to start" 72 88 15))

;attract
:sound 
(fn [])

;attract
:music 
(fn [] 
 (music 0))

;attract
:music? :play})

(local lose {
:input 
(fn []
 (if 
	 (btnp 4) 
		 (do 
		  (table.remove modes 1)
				(table.insert modes 1 attract))))

;lose
:update
(fn [])

;lose 
:draw 
(fn [] 
 (print "You lost" 100 65 15)
	(print "press 'z' to start over" 70 73 15))

;lose 
:sound  
(fn [])

;lose
:music 
(fn [])

;lose 
:music? :none})

(local win {
:input 
(fn []
 (if 
	 (btnp 4) 
		 (do 
		  (table.remove modes 1)
				(table.insert modes 1 attract))))

;win
:update
(fn [])

;win
:draw 
(fn [] 
 (print "You won" 100 65 15)
	(print "press 'z' to start over" 70 73 15))

;lose 
:sound  
(fn [])

;lose
:music 
(fn [])

;lose 
:music? :none})

(table.insert modes attract)
(table.insert modes play)


(global TIC  
  (fn []
		 (let [mode (. modes 1)]
	   (mode:input)
		  (mode:update)
	   (mode:draw)
		  (mode:sound)
			 (if
	 		 (= mode.music? :play)
			   (do 
				   (mode:music)
						 (tset mode :music? :on))
					(= mode.music? :stop)
					 (do 
						 (music)
							(tset mode :music? off)))
				(when (one-of events :lose)
				 (table.insert modes 1 lose)
					(table.remove events 1))
				(when (one-of events :win)
				 (table.insert modes 1 win)
					(table.remove events 1))					
		  (set t (+ t 1)))))