module FormMake where


import Keyframes as K 
import Window
import Color
import Text as T
import Graphics.Element as E
import Graphics.Collage as C exposing (defaultLine)
import Graphics.Input exposing (button, customButton)
import Time exposing (Time)
import Signal exposing (Mailbox, mailbox)
import Keyboard
import Char
import String
import Random


line1 : Color.Color -> (Float,Float) -> C.LineStyle
line1 c (w,h) = 
    {color = c, width = (w/275), cap = C.Round, join = C.Smooth, 
    dashing = [], dashOffset = 0}

thickLine c (w,h) = 
    {color = c, width = (w/160), cap = C.Round, join = C.Smooth, dashing = [], dashOffset = 0}

thickerLine c (w,h) = 
    {color = c, width = (w/90), cap = C.Round, join = C.Smooth, dashing = [], dashOffset = 0}

thickestLine c (w,h)= 
    {color = c, width = 30.0, cap = C.Round, join = C.Smooth, dashing = [], dashOffset = 0}

{-borderLine c = 
    {color = c, width = 50.0, cap = C.Round, join = C.Smooth, dashing = [], dashOffset = 0}-}

zigLine c (w,h)= 
    {color = c, width = (w/275), cap = C.Flat, join = C.Sharp 10, dashing = [], dashOffset = 0}

circLine c (w,h)= 
    {color = c, width = (w/40), cap = C.Flat, join = C.Sharp 10, dashing = [], dashOffset = 0}

--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------
hPink = Color.rgb 219 39 99

bGreen = Color.rgb 176 221 67

bBlue = Color.rgb 18 234 234

lpurp = Color.rgb 196 146 177

lblue = Color.rgb 188 231 253

trans : (Float, Float) -> Int -> (Float,Float)
trans (x,y) num = 
    if num == 1 || num == 10 then (x,y)
    else if num == 2 || num == 9 then (y,x)
    else if num == 3 then (x,-y)
    else if num == 4 then (-y,x)
    else if num == 5 then 
        let x' = (cos (pi/4))*x - (sin (pi/4))*y in 
        let y' = (sin (pi/4))*x + (cos (pi/4))*y in 
        (x',y')
    else if num == 6 then 
        let x' = (cos (3*pi/4))*x - (sin (3*pi/4))*y in 
        let y' = (sin (3*pi/4))*x + (cos (3*pi/4))*y in 
        (x',y')
    else if num == 7 then 
        let x' = (cos (-pi/4))*x - (sin (-pi/4))*y in 
        let y' = (sin (-pi/4))*x + (cos (-pi/4))*y in 
        (x',y')
    else 
        let x' = (cos (-3*pi/4))*x - (sin (-3*pi/4))*y in 
        let y' = (sin (-3*pi/4))*x + (cos (-3*pi/4))*y in 
        (x',y')


drawCircle : Float -> Color.Color -> C.Form
drawCircle r c = 
    C.filled c (C.circle r)

drawOval : Float -> Float-> Color.Color -> C.Form
drawOval w h c =
    C.filled c (C.oval w h)

drawRipngon : Int -> Float -> Color.Color->(Float,Float) -> C.Form
drawRipngon n r c (w,h)= 
    C.outlined (line1 c(w,h)) (C.ngon n r)

drawngon : Int -> Float -> Color.Color->(Float,Float) -> C.Form
drawngon n r c (w,h)= 
    C.outlined (thickLine c(w,h)) (C.ngon n r)

drawthickngon : Int -> Float -> Color.Color -> (Float,Float) -> C.Form
drawthickngon n r c (w,h) = 
    C.outlined (thickerLine c (w,h)) (C.ngon n r)

fillngon : Int -> Float -> Color.Color -> C.Form
fillngon n r c = 
    C.filled c (C.ngon n r)


multngon : Int -> Float -> Float ->Color.Color ->(Float,Float) -> C.Form
multngon rand size angle c (w,h)= 
    let 
    ngon1 = C.rotate (angle) (drawngon rand size c (w,h))
    ngon2 = C.rotate (-angle+1) (drawngon rand (size*(3/4)) c (w,h))
    ngon3 = C.rotate (angle-1) (drawngon rand (size*(1/2)) c (w,h))
    ngon4 = C.rotate (-angle) (drawngon rand (size*(1/4)) c (w,h))
    in 
    C.group[ngon1, ngon2,ngon3,ngon4]

fourmultngon: Int -> Float -> Float ->(Float, Float) ->Color.Color -> C.Form
fourmultngon rand size angle (wfloat, hfloat) c = 
    let 
    wmove = wfloat*(11/32)
    hmove = hfloat*(9/32)
    ngon1 = C.move (wmove, hmove) (multngon rand size angle c (wfloat,hfloat))
    ngon2 = C.move (-wmove, hmove) (multngon rand size angle c (wfloat,hfloat))
    ngon3 = C.move (wmove, -hmove) (multngon rand size angle c (wfloat,hfloat))
    ngon4 = C.move (-wmove, -hmove) (multngon rand size angle c (wfloat,hfloat))
    in 
    C.group [ngon1,ngon2,ngon3,ngon4]

ripple : Float -> Int -> Int -> Int -> (Float,Float) -> C.Form 
ripple f r g b (w,h) = 
    let 
        r0 = drawRipngon 4 (0.1*f) (Color.rgba r g b (1- (0.3*f/w))) (w,h)
        r1 = drawRipngon 4 (0.3*f) (Color.rgba r g b (1- (0.3*f/w))) (w,h)
        r2 = drawRipngon 4 (0.5*f) (Color.rgba r g b (1 - (0.5*f/w))) (w,h)
        r3 = drawRipngon 4 (0.8*f) (Color.rgba r g b (1-(0.8*f/w))) (w,h)
        r4 = drawRipngon 4 f (Color.rgba r g b (1-(f/w))) (w,h)
        r5 = drawRipngon 4 (1.5*f) (Color.rgba r g b (1-(1.5*f/w))) (w,h)
        r6 = drawRipngon 4 (2*f) (Color.rgba r g b (1-(2*f/w))) (w,h)
    in 
    C.group [r1,r2,r3,r4,r5,r6]

goldHelp : Float -> Float -> Float -> Float -> Float -> List (Float, Float) -> List (Float, Float)
goldHelp currang endang c a inc acc = 
    if currang <= endang then acc
    else 
        let 
            r = a*(c^currang) 
            x = r*(cos (degrees currang))
            y = r*(sin (degrees currang))
        in 
        goldHelp (currang - inc) endang c a inc ((x,y)::acc)

goldenSpiral : Float -> Float -> (Float, Float)-> C.Form 
goldenSpiral angle a (w,h)= 
    let 
        c = 1.0053611 
        goldlist = goldHelp 1440 angle c a 5 []
    in 
    C.traced (thickLine Color.black (w,h)) (C.path goldlist)
 
zigRight : Float -> Float -> Float -> Int -> List (Float,Float) -> List (Float,Float)
zigRight f x y n acc = 
    if n == 0 then acc
    else zigRight f (x+f) (-y) (n-1) ((x,y)::acc)

zigLeft : Float -> Float -> Float -> Int -> List (Float,Float) -> List (Float,Float)
zigLeft f x y n acc = 
    if n == 0 then acc
    else zigLeft f (x-f) (-y) (n-1) ((x,y)::acc)

zigPathHelp : Float -> Float -> Float -> Int -> List (Float,Float) -> List (Float,Float)
zigPathHelp f x y n acc = 
    if n == 0 then acc
    else zigPathHelp f (x-f) (-y) (n-1) ((x,y)::acc)

zigPathGen : Float -> Float -> Int -> List (Float, Float)
zigPathGen f y n = 
    let 
        zr = List.reverse (zigRight f f -y (n//2) [])
        zl = zigLeft f -f -y (n//2) []
    in 
    List.append zl ((0,y)::zr)

zigPathHelp2 : Int -> Float -> Float -> Float -> List (Float,Float) -> List (Float,Float)
zigPathHelp2 n x inc y acc = 
    if n == 0 then acc
    else zigPathHelp2 (n-1) (x + inc) inc (-y) ((x,y)::acc)

zigPathGen2 : Int -> Float -> Float -> Float -> List (Float,Float)
zigPathGen2 n startx inc y = 
    zigPathHelp2 n startx inc y []


drawPath : List (Float,Float) -> (Float, Float) -> C.Form
drawPath pts (w,h) =
    C.traced (thickLine (Color.rgba 255 255 0 1) (w,h)) (C.path pts)

doubleZig : List (Float,Float) -> (Float, Float)-> C.Form
doubleZig pts (wfloat,hfloat)= 
    let f1 = C.moveX ((3/8)*wfloat) (C.rotate 90 (drawPath  pts (wfloat,hfloat))) in
    let f2 = C.moveX (-(3/8)*wfloat) (C.rotate 180 (drawPath  pts (wfloat,hfloat))) in
    C.group [f1,f2]

drawPath2 : List (Float,Float)-> (Float,Float) -> C.Form
drawPath2 pts (w,h) =
    C.traced (zigLine (Color.rgba 50 171 252 1) (w,h)) (C.path pts)

sineHelp : Float -> Float -> Float -> Float -> List (Float,Float) -> List (Float,Float)
sineHelp endx currx inc mult acc = 
    if currx >= endx then acc
    else let y = mult * (sin (currx/50)) in
        sineHelp endx (currx+inc) inc mult ((currx,y)::acc)


drawSine : Float -> Float -> Float -> (Float,Float) -> C.Form
drawSine startx currx mult (w,h) = 
    C.traced (thickLine Color.red (w,h)) (C.path (sineHelp currx startx 5 mult []))

{-border : Float -> Float -> Float -> (Float,Float) -> List (Float,Float) -> List (Float,Float)
border currx endx inc (w,h) acc = 
    if currx >= endx then acc 
    else 
        let newx = if currx < w then (currx - (w/2))
                   else if currx < (w + h) then w/2
                   else if currx < (2*w + h) then ((3*w/2 + h) - currx)
                   else -w/2
            newy = if currx < w then h/2 
                   else if currx < (w + h) then ((w + h/2) - currx)
                   else if currx < (2*w + h) then -h/2
                   else (currx - (2*w + 3*h/2))
        in
        border (currx + inc) endx inc (w,h) ((newx,newy)::acc)

drawBorder : Float -> (Float,Float) -> C.Form 
drawBorder x (w,h) = 
    let 
        borderlist = border 0 x (w/100) (w,h) []
    in 
    C.traced (borderLine (Color.rgba 255 153 204 1)) (C.path borderlist)-}

circleHelp : Float -> Float -> Float -> Float -> List (Float, Float) -> List (Float, Float)
circleHelp r currtheta endtheta inc acc = 
    if currtheta >= endtheta then acc
    else let x = r*(cos (degrees currtheta)) in 
        let y = r*(sin (degrees currtheta)) in
        circleHelp r (currtheta + inc) endtheta inc ((x,y)::acc)

gradualCircle : Float -> Float -> (Float,Float)-> C.Form 
gradualCircle r theta (w,h)= 
    C.traced (circLine Color.lightGreen (w,h)) (C.path (circleHelp r 0 theta 5 []))

sparkle1 : Float -> Int -> (Float,Float) -> C.Form
sparkle1 f num (w,h) = 
    let 
        c1 = C.move (trans (0, f) num) (drawCircle 10.0 Color.white) 
        c2 = C.move (trans (0.5*f, 0.5*f) num) (drawCircle 7.0 Color.white)
        c3 = C.move (trans (0.2*f, 2.8*f) num) (drawCircle 5.0 Color.white)
        c4 = C.move (trans (-0.5*f, 0.3*f) num) (drawCircle 9.0 Color.white)
        c5 = C.move (trans (-0.3*f, 0.8*f) num) (drawCircle 5.0 Color.white)
        c6 = C.move (trans (f, f) num) (drawCircle 8.0 Color.white)
        c7 = C.move (trans (0.2*f, 0.3*f) num) (drawCircle 8.0 Color.white)
        c8 = C.move (trans (-0.75*f, 1.3*f) num) (drawCircle 6.0 Color.white)
        c9 = C.move (trans (0.4*f, 1.6*f) num) (drawCircle 6.0 Color.white)
        c10 = C.move (trans (0, 0.6*f) num) (drawCircle 3.0 Color.white)
        c11 = C.move (trans (-0.4*f, 0.5*f) num) (drawCircle 4.0 Color.white)
        c12 = C.move (trans (-0.6*f, 1.6*f) num) (drawCircle 6.0 Color.white)
        c13 = C.move (trans (0.6*f, f) num) (drawCircle 4.0 Color.white)
        c14 = C.move (trans (0.3*f, 0.8*f) num) (drawCircle 6.0 Color.white)
    in
    C.group [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14]

sparkle2 : Float -> (Int, Int) -> C.Form
sparkle2 f (w,h)= 
    let 
        rad = toFloat (w//50)
        x = f*sin((degrees 45))
        y = f*cos((degrees 45))
        c1 = C.move (f, 0) (drawCircle rad Color.orange) 
        c2 = C.move (0, f) (drawCircle rad Color.darkOrange)
        c3 = C.move (-f, 0) (drawCircle rad Color.yellow)
        c4 = C.move (0, -f) (drawCircle rad Color.lightOrange)
        c5 = C.move (x,y) (drawCircle rad Color.darkOrange)
        c6 = C.move (x,-y) (drawCircle rad Color.orange)
        c7 = C.move (-x,-y) (drawCircle rad Color.lightOrange)
        c8 = C.move (-x,y) (drawCircle rad Color.yellow)
    in
    C.group [c1,c2,c3,c4,c5,c6,c7,c8]


circleDotHelp : Float -> Float -> Float -> Float -> Color.Color -> List C.Form -> List C.Form 
circleDotHelp f currangle inc r c acc = 
    if currangle >= 360 then acc 
    else
        let circ = C.move (f*(cos (degrees currangle)), f*(sin (degrees currangle))) (drawCircle r c) in
        circleDotHelp f (currangle + inc) inc r c (circ::acc)

circleDots : Float -> Float -> Float -> Color.Color -> (Float, Float) -> C.Form
circleDots circr dotr inc c (w,h) = 
    let 
        circlist = circleDotHelp circr 0 inc dotr c [] 
    in 
    C.group circlist



dotCluster : Float -> Int -> (Float, Float) -> (Int, Int)-> Color.Color-> C.Form
dotCluster f num (x, y) (w,h) c= 
    let 
        wfloat = toFloat w 
        hfloat = toFloat h
        --f = (toFloat num) * 4
        c1 = C.move (trans (f, f*(toFloat num)) (round f)) (drawCircle (wfloat/40) c) 
        c2 = C.move (trans (f*f, 5*f) num) (drawCircle (hfloat/41) c)
        c3 = C.move (trans (x*f, 2.8*f) (round f)) (drawCircle (hfloat/40) c)
        c4 = C.move (trans (-0.5*f, x*f) (num*2)) (drawCircle (wfloat/43) c)
        c5 = C.move (trans (-y*f, -6*f) num) (drawCircle (hfloat/42) c)
        c6 = C.move (trans (f, f) num) (drawCircle (wfloat/47) c)
        c7 = C.move (trans (-10*f, 8*f) (num*5)) (drawCircle (hfloat/44) c)
        c8 = C.move (trans (-y*f, 11*f) num) (drawCircle (wfloat/48) c)
        c9 = C.move (trans (f*f, 13*f) (round (f*5))) (drawCircle (hfloat/43) c)
        c10 = C.move (trans (y*y, x*f) num) (drawCircle (wfloat/49) c)
        c11 = C.move (trans (-x*f, x*f) (round f)) (drawCircle (hfloat/46) c)
        c12 = C.move (trans (-f*f, 1.6*f) (w//15)) (drawCircle (wfloat/47) c)
        c13 = C.move (trans (-10*f, f) num) (drawCircle (hfloat/44) c)
        c14 = C.move (trans (x*f, y*f) (h//12)) (drawCircle (wfloat/50) c)
    in
    C.group [c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14]

dotClusters : Float ->Int -> (Float, Float) -> (Int, Int) ->C.Form
dotClusters f num (x,y) (w,h) = 
    let 
        ang = (toFloat num)*35
        cluster1 = dotCluster f num (x,y) (w,h) Color.darkPurple
        cluster2 = C.moveX (toFloat (-w//12)) (C.rotate ang (dotCluster f (num*2) (x, y) (w,h) Color.purple))
        cluster3 = C.moveY (toFloat(h//10)) (C.rotate 220 (dotCluster f num (x+(toFloat num), y) (w,h) Color.lightPurple))
    in 
    C.group [cluster1,cluster2,cluster3]

oneLine : C.LineStyle -> (Float,Float) -> Float -> Float -> C.Form 
oneLine line (x0,y0) f angle= 
    let 
        x = f*cos((degrees angle))
        y = f*sin((degrees angle))
    in
    C.traced line (C.segment (x0,y0) (x,y))

singLine: (Float, Float) -> Int -> Float -> C.Form
singLine (wfloat, hfloat) rand f = 
    let ((mvx,mvy), ang) = 
        if rand <2 then ((wfloat/2,-hfloat/2), 115)
        else if (rand<4 && rand>=2) then ((-wfloat/2, -hfloat/2), 70)
        else if (rand<6 && rand>=4) then ((wfloat/2, 0), 210)
        else if (rand<8 && rand>=6) then ((-wfloat/2, 0), 25)
        else if (rand<9 && rand>=8) then ((wfloat/2, hfloat/2), 260)
        else ((wfloat/3, hfloat/2), 235)
    in 
        C.move (mvx,mvy) (oneLine (thickLine Color.yellow (wfloat,hfloat)) (0,0) f ang)
    

twoLines : (Int,Int) -> Float -> C.Form 
twoLines (w,h) f = 
    let 
        wfloat = toFloat w
        hfloat = toFloat h
        l1 = C.move (toFloat (-w//2),toFloat (-h//2)) 
            (oneLine (thickLine Color.darkGreen (wfloat,hfloat)) (0,0) f 35.0)
        l2 = C.move (toFloat (w//2),toFloat (-h//2)) 
            (oneLine (thickLine Color.darkGreen (wfloat,hfloat)) (0,0) f 145.0)
    in 
    C.group [l1,l2]


manyLines : (Int,Int) -> Float -> C.Form 
manyLines (w,h) f = 
    let 
        wfloat = (toFloat w)
        hfloat = (toFloat h)
        frac = wfloat/13
        opp = -f
        l1 = C.move (-frac*6,0) (oneLine (thickLine Color.darkRed (wfloat,hfloat)) (0, 0.2*opp) f 90.0)
        l2 = C.move (-frac*5,0) (oneLine (thickLine Color.red (wfloat,hfloat)) (0, 0.2*opp) f 90.0)
        l3 = C.move (-frac*4,0) (oneLine (thickLine Color.darkOrange (wfloat,hfloat)) (0, 0.2*opp) f 90.0)
        l4 = C.move (-frac*3,0) (oneLine (thickLine Color.orange (wfloat,hfloat)) (0, 0.2*opp) f 90.0)
        l5 = C.move (-frac*2,0) (oneLine (thickLine Color.yellow (wfloat,hfloat)) (0, 0.2*opp) f 90.0)
        l6 = C.move (-frac*1,0) (oneLine (thickLine Color.lightYellow (wfloat,hfloat)) (0, 0.2*opp) f 90.0)
        l7 = oneLine (thickLine Color.green (wfloat,hfloat)) (0, 0.2*opp) f 90.0
        l8 = C.move (frac*1,0) (oneLine (thickLine Color.darkGreen (wfloat,hfloat)) (0, 0.2*opp) f 90.0)
        l9 = C.move (frac*2,0) (oneLine (thickLine Color.blue (wfloat,hfloat)) (0, 0.2*opp) f 90.0)
        l10 = C.move (frac*3,0) (oneLine (thickLine Color.darkBlue (wfloat,hfloat)) (0, 0.2*opp) f 90.0)
        l11 = C.move (frac*4,0) (oneLine (thickLine Color.darkPurple (wfloat,hfloat)) (0, 0.2*opp) f 90.0)
        l12 = C.move (frac*5,0) (oneLine (thickLine Color.purple (wfloat,hfloat)) (0, 0.2*opp) f 90.0)
        l13 = C.move (frac*6,0) (oneLine (thickLine Color.lightPurple (wfloat,hfloat)) (0, 0.2*opp) f 90.0)
    in
    C.group [l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13]

drawLines : K.State -> Int -> (Int,Int) -> Bool -> List C.Form 
drawLines kstate n (w,h) neg = 
    case kstate of 
        (prop, valstr)::rest -> 
            let spacing = w//13 in
            let offset = toFloat (spacing*n) in 
            let inc = 
                if neg then n-1 else n+1
            in
            let valres = String.toFloat valstr in
            case valres of 
                Ok val -> 
                    let opp = -val in 
                    (C.move (offset,0) 
                        (oneLine (thickLine Color.darkGrey ((toFloat w, toFloat h)))
                            (0,0.2*opp) val 90.0))::(drawLines rest inc (w,h) neg)
                _ -> Debug.crash "invalid line"
        [] -> []

twoTriangles : Float -> Float -> Float -> Int -> C.Form
twoTriangles w y r num = 
    let 
        c = if num == 1 || num == 10 then Color.rgb 102 0 0
            else if num == 2 || num == 9 then Color.rgb 156 15 15 
            else if num == 3 || num == 8 then Color.rgb 153 0 0
            else if num == 4 || num == 7 then Color.rgb 247 47 47 
            else  Color.rgb 204 51 51 
        t2 = C.rotate (degrees 90) (fillngon 3 r c)
        t1 = C.rotate (degrees 270) (fillngon 3 r c)
        x = {-if num == 1 || num == 10 || num == 5 then w/4 
            else if num == 2 || num == 9 || num == 6 then -w/4
            else if num == 3 || num == 8 then -3*w/8
            else 3*w/8-}
            if num % 2 == 0 then -3*w/8 else 3*w/8
    in 
    C.group [C.move (x,y) t1, C.move(x,-y) t2]

