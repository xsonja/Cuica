module Draw where


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
import FormMake exposing (..)
import RandSelect exposing (..)
import Animations exposing (..)

empty : C.Form
empty = drawCircle 0 Color.black

-- helper function to turn string from K.State to float
getVal : String -> Float
getVal str = 
    let res = String.toFloat str in
    case res of 
        Ok r -> r
        _ -> Debug.crash "getVal not valid result"


-- DRAWS THE GIVEN FRAME OF THE GIVEN ANIMATION 
draw : (Int, Int) -> (Float, (Char, Int)) -> C.Form  
draw (w,h) (frame, (id, num)) = 
    let wfloat = (toFloat w) in
    let hfloat = (toFloat h) in
    if id == 'a' then 
        let list = expandShrink 1.0 (toString (wfloat/50)) (toString (wfloat/5)) frame in
        case list of 
            [(prop, rstr)] -> let r = getVal rstr in
                                drawCircle r Color.darkBlue 
            _ -> empty
    else if id == 'j' then 
        let list = getnFrames 7 (w,h) frame in 
        let llines = drawLines list 0 (w,h) True in 
        let rlines = drawLines list 0 (w,h) False in 
        C.group (List.append llines rlines)
    else if id == 'h' then 
        let list = growShrink 1.0 "2.0" (toString (wfloat/5))  (toString (-(wfloat/5)))  frame in 
        case list of 
            [(prop,lstr)] -> let l = getVal lstr in
                                 manyLines (w,h) l
            _ -> empty
    else if id == 's' then 
        let list = angle 2.5 "0" "360" frame in 
        case list of 
            [(prop, degstr)] -> 
                let deg = getVal degstr in
                let octagon = drawngon 8 (wfloat/ 4.5) hPink in 
                    C.rotate (degrees deg) (octagon (wfloat,hfloat))
            _ -> empty
    else if id == 'c' then 
        let list = angle 2.5 "0" "360" frame in 
        case list of 
            [(prop, degstr)] -> 
                let deg = getVal degstr in
                fourmultngon 4 (wfloat/ 10) deg (wfloat,hfloat) bBlue
            _ -> empty
    else if id == 'd' then 
        let list = quadOutexpandwait 3.0 "0" (toString(wfloat/4)) frame in
        case list of 
            [(prop,rstr)] -> let r = getVal rstr in 
                                C.move (trans (0,r) num) (sparkle1 r num (wfloat,hfloat))
            _ -> empty
    else if id == 'f' then
        let list = expand 2.0 "0" (toString w) frame in 
        case list of 
            [(prop,rstr)] -> let r = getVal rstr in sparkle2 r (w,h) 
            _ -> empty
    else if id == 'g' then 
        let hfloat = (toFloat h) in
        let list = expand 1.0 "0" (toString ((sqrt 2)*wfloat)) frame in 
        case list of 
            [(prop,rstr)] -> let r = getVal rstr in twoLines (w,h) r
            _ -> empty
    else if id == 'q' then 
        let list = quadExpand 0.1 "0" (toString ((sqrt 2)*wfloat)) frame in 
        case list of 
            [(prop,rstr)] -> let r = getVal rstr in singLine (wfloat,hfloat) num r
            _ -> empty
    else if id == 'y' then 
        let list = expandwait 1.5 "3" "8" frame in 
        case list of 
            [(prop,rstr)] -> let n = round (getVal rstr) in 
                fillngon n (wfloat/6) Color.black
            _ -> empty
    else if id == 'x' then 
        C.filled Color.white (C.rect wfloat hfloat)
    else if id == 'm' then
        let list = expandwait 1.0 (toString (hfloat/2)) (toString (-hfloat/14)) frame in 
        case list of 
            [(prop, ystr)] -> let y = getVal ystr in 
                                twoTriangles wfloat y (hfloat/7) num 
            _ -> empty
    else if id == 'n' then 
        let list = moveShrink 0.95 (toString (hfloat/2)) (toString (-hfloat/4)) (toString (wfloat/10)) (toString 0) frame in
        case list of 
            (prop1, str1)::[(prop2,str2)] -> 
                let pos = if prop1 == "position" then getVal str1 else getVal str2 in
                let size = if prop1 == "width" then getVal str1 else getVal str2 in
                let x = if num % 2 == 0 then -wfloat/4 else wfloat/4 in
                let y = if num % 2 == 0 then pos else -pos in 
                C.move (x,y) (drawCircle size (Color.rgba 255 255 255 (size/(wfloat/10))))
            _ -> empty
    else if id == 't' then 
        let list = shrinkRotate 8.0 "0" "10" (toString (wfloat/2)) "0" frame in  
        case list of 
            [(rot, val1),(wid, val2)] -> 
                let deg = getVal val1 in
                let s = getVal val2 in
                let square = drawthickngon 4 s Color.orange in 
                    C.rotate deg (square (wfloat,hfloat))
            _ -> empty
    else if id == 'p' then 
        let (x, y) = nPos (wfloat, hfloat) num in
        let list = expand 1.0 "0" (toString ((sqrt 2)*wfloat)) frame in 
        case list of 
            [(prop,rstr)] -> let r = getVal rstr 
            in oneLine (thickestLine Color.lightRed (wfloat,hfloat)) (x,y) r (10.0*r)
            _ -> empty
    else if id == 'o' then 
        let (x,y) = nPos (wfloat,hfloat) num in 
        let list = fadeExpand 1.0 "0.70" "0" frame in 
        case list of 
            [(rot, val1),(wid, val2)] -> let r = getVal val1 in let r2 = getVal val2 
            in C.alpha r2 (dotClusters r num (r+x,y-r2) (w,h))
            _ -> drawCircle 0 Color.black 
    else if id == 'b' then 
        let list = quadExpand 0.5 (toString (wfloat/2)) (toString (wfloat/5)) frame in
        let (x,y) = randStart num hfloat in
        case list of 
            [(prop, rstr)] -> let r = getVal rstr in
                                C.move (x,y) (drawOval wfloat r Color.lightGrey)
            _ -> empty    
    else if id == 'k' then 
        let list = quadInOutexpandShrink 1.0 "0" (toString (wfloat/15)) frame in 
        case list of 
            [(prop, rstr)] -> 
                let size = getVal rstr in 
                doubleZig (zigPathGen size (hfloat/10) 9) (wfloat,hfloat)
            _-> empty
    else if id == 'l' then
        let list = expand 1.0 "0" "10" frame in
        case list of 
            [(prop, xstr)] -> let x = round (getVal xstr) in
            let startx = if num % 2 == 0 then -7*wfloat/16 else 7*wfloat/16 in
            let newy = if num <= 5 then hfloat/4 else -hfloat/4 in 
            let inc = if num % 2 == 0 then wfloat/10 else -wfloat/10 in
            C.move (0,newy) (drawPath2 (zigPathGen2 x startx inc (hfloat/10)) (wfloat,hfloat))
            _ -> empty
    else if id == 'z' then
        let list = expand 1.0 "0" (toString (wfloat/2)) frame in 
        case list of 
            [(prop,xstr)] -> let x = getVal xstr in 
            C.move (-wfloat/4,0) (drawSine 0 x (hfloat/8) (wfloat,hfloat))
            _ -> empty
    else if id == 'w' then 
        let start = if num % 2 == 0 then wfloat else hfloat in
        let list = quadExpand 0.85 (toString start) "0" frame in
        case list of 
            [(prop, rstr)] -> let r = getVal rstr in 
            let (rw, rh) = if num % 2 == 0 then (r, hfloat) else (wfloat, r) in
            C.filled Color.lightPurple (C.rect rw rh)
            _ -> empty
    else if id == 'e' then 
        let list = expand 0.8 "0" "400" frame in 
        case list of 
            [(prop,thetastr)] -> let theta = getVal thetastr in 
            gradualCircle (wfloat/4) theta (wfloat,hfloat)
            _ -> empty
    else if id == 'r' then 
        let list = expand 0.80 "0" "180" frame in 
        case list of 
            [(prop, angstr)] -> 
                let angle = getVal angstr in
                let pink = Color.rgba 177 96 247 0.8 in 
                C.rotate (degrees angle) (circleDots (min (11*wfloat/24) (11*hfloat/24)) (wfloat/100) 18 pink (wfloat,hfloat))
            _ -> empty
    else if id == 'u' then 
        C.filled Color.black (C.rect wfloat hfloat)
    else if id == 'i' then 
        let list = expExpand 0.9 "0" (toString (2.8*wfloat)) frame in 
        case list of 
            [(prop, rstr)] -> let r = getVal rstr in 
            ripple r 0 0 153 (wfloat,hfloat)
            _ -> empty
    else if id == 'v' then 
        let list = expand 1.0 "1080" "0" frame in 
        case list of 
            [(prop, angstr)] -> let angle = getVal angstr in 
            C.move (wfloat/4, -hfloat/4) (goldenSpiral angle (wfloat/200) (wfloat,hfloat))
            _ -> empty
    else empty
        {-let list = fade 1.0 "1" "0" frame in 
        case list of 
            [(prop, ostr)] -> 
                let opac = getVal ostr in
                let purplecircle = drawCircle 10.0 Color.lightPurple in 
                C.alpha opac (C.move (100,100) purplecircle)
            _ -> empty-} 
