Project Update
---------------

What we have done so far:
-overall structure for signals and state are pretty much set 
-figured out mapping sounds to keys (using Ports)
-some animations (6-7 of them) using keyframes library and have also mapped them to keys
-can change background color
-it works

Description of Implementation: 
1) State is (Int, List (Float, (Char, Int))) 
- the first int is corresponds to the background color
- the list of tuples keeps track of which animations are currently going on
	- the char comes from the key that is pressed
	- the float ranges from 0 to 1 and keeps track of how far the animation has progressed 
	- the int allows us to have random aspects to our animations
2) Events
- events are either a time or a key
- when time elapses, all of the ongoing animations progress (their floats increase)
- when the float of a tuple in the state reaches 1 (i.e. the animation has finished), it is removed from the state
- when a key is pressed, a new item is added to the list indicating that another animation must be drawn
3) Animations 
- animations are drawn using Collage, Element, and KeyFrame libraries 
- KeyFrame animations start at 0 and go to 1, which is why we have floats in the list in our State
4) Randomness 
- for some keys, we don’t want the same exact animation to play every time. in order to do this, we must have multiple different animations associated with the same key. the animation that is played is indicated by the Int in the State’s list 
- this int is chosen randomly, using the Random library. we decided to make the seed either the background color int or a product of the background color int, the length of the state list, and the previous animation’s KeyCode
5) Sound
- a port sends a string to Audio.js whenever a key is pressed
- this string, which is just the key that was pressed, determines what sound is played 

Future goals:
-name the project (have a title page before the actual typing fun)
-Add more animations and aesthetically pleasing sounds
	-hopefully cover range of alphabet
-Use keyframes.easing to alter speed at which animations run
-Add a random element to animations in :
	-color, position, size
-consider basing colors off of the background color (if there’s time)
-Considering whether looping is something we want

To compile:
elm make Main.elm --output=Main.js

To run:
open up Testing.html

Enjoy!
- the different animations correspond to a, s, d, f, g, h, j, y
- change background color using the spacebar