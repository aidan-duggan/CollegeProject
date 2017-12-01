-----------------------------------------------------------------------------------------
{-| Module      : Main
    Copyright   : 
    License     : All Rights Reserved

    Maintainer  : 
    Stability   : 
    Portability : 
-}
-----------------------------------------------------------------------------------------

module Main where
import qualified Control.Monad.State as State
import qualified Control.Exception as C
import Graphics.UI.WXCore as WC
import Graphics.UI.WX as W
import Data.Char
import GHC.Float
import Random
import System.Exit

main :: IO ()
main
  = start gui

gui :: IO ()
gui = do
		--Create the variable to store the graphical state
		var <- varCreate defaultQuicksilver
		f  <- frameFixed [text := "Quicksilver"]
		--Set the panel on which the graphics is drawn
		p <- panel f []
		set p [bgcolor := (rgb 125 125 125), on paint := onpaint var p]
		set p [ on motion := mouseMo var, on click := mouseCl var True, on clickRight := mouseCl var False]
		windowOnKeyDown p $ keyPr var
		--Set up the timer which will repaint the graphics every 30 ticks 
		updateState var (settingup p)
		t <- timer p []
		v <- varGet var
		v <- return v {time = [t]}
		varSet var v
		set t [interval := 30, on command := refreshPro p t var]
		--Add the panel to the frame
		set f [clientSize := (sz (screenX defaultUserRecord) (screenY defaultUserRecord)), layout := fill $ widget p] 
		return ()

settingup :: Panel () -> Quick()
settingup p = do 
			b <- State.get
			State.put b {drawPanel = [p]}
			b <- State.get
			c <- setup (user b)
			b <- State.get
			State.put defaultQuicksilver { user = c, drawPanel = [p], mouseX = mouseX b, mouseY = mouseY b, time = time b}

--Called when the graphics are to be repainted
onpaint :: Var Quicksilver -> Panel ()-> DC a -> t -> IO ()
onpaint var p dc viewArea
	= do
		--Get the state
		v <- varGet var		
		--paint the screen and update the state
		updateState var (paintScreen dc p)
		v <- varGet var
		timerStart (head (time v)) 30 False
		return ()

--Paint the user graphics to the screen and updates the State
paintScreen :: DC a -> Panel () -> Quick ()
paintScreen dc p = do 
				--Get the state
				b <- State.get
				--Run the users code passing in their record state
				c <- painting dc (user b)
				b <- State.get
				--Reset the various flags and save user record
				State.put defaultQuicksilver {user = c, drawPanel = [p], mouseX = mouseX b, mouseY = mouseY b, time = time b}

--Repaint the graphics and update the background colour
refreshPro p t var = do
				timerStop t
				repaint p

--Log where the mouse moves too
mouseMo :: Var Quicksilver -> Point2 Int -> IO ()
mouseMo var p
	= do
		updateState var (updateMousePos p)

--Update the states mouse positions
updateMousePos :: Point2 Int -> Quick ()
updateMousePos p = do
				a <- State.get
				State.put a {mouseX = (pointX p), mouseY = (pointY p)}

--Update if the mouse button has been clicked
mouseCl :: Var Quicksilver -> Bool -> Point2 Int -> IO ()
mouseCl var lr p
	= do
		updateState var (updateClick lr)

--Update which mouse button was clicked in the state
updateClick :: Bool -> Quick ()
updateClick lr = do
				a <- State.get
				if lr == True
					then State.put a {leftClick = True}
					else State.put a {rightClick = True}

--Log key presses, limited to alphanumeric keys
keyPr :: Var Quicksilver -> EventKey -> IO ()
keyPr var ch = do
				updateState var (storeChar ch)

--Adds the pressed character to a list previous characters pressed between repaints
storeChar :: EventKey -> Quick ()
storeChar ch = do
				a <- State.get
				b <- return( calChar (shiftDown (keyModifiers ch)) (keyKey ch))
				State.put a {charNumPressed = (charNumPressed a) ++ b}
				

--Map the key to its upper or lower case
calChar :: Bool -> Key -> [Char]
calChar True (KeyChar c) = [(toUpper c)]
calChar False (KeyChar c) = [(toLower c)]
calChar _ KeySpace = [' ']
calChar _ _ = []

--Helper function to run execStateT function on the passed in function and return the results
updateState :: Var Quicksilver -> Quick a -> IO ()
updateState var func = do
					v <- varGet var
					a <- State.execStateT( func) v
					varSet var a

--The state for managing the graphics
type Quick a = State.StateT Quicksilver IO a

data Quicksilver = Quicksilver{
	user :: UserRecord
	,fred :: Int
	,fblue :: Int
	,fgreen ::Int
	,lred :: Int
	,lblue :: Int
	,lgreen ::Int
	,bgred :: Int
	,bgblue :: Int
	,bggreen :: Int
	,pfill :: Bool
	,lineThick :: Int
	,centreMode :: Bool
	,mouseX :: Int
	,mouseY :: Int
	,leftClick :: Bool
	,rightClick :: Bool
	,charNumPressed :: [Char]
	,drawPanel :: [Panel ()]
	,time :: [W.Timer]
	}
--The state value for the state, including the start value the user defines for their state
defaultQuicksilver = Quicksilver (defaultUserRecord) 255 255 255 0 0 0 125 125 125
								True 1 False 0 0 False False [] [] []

--Set the colour which will fill in shapes
setFillColour :: (Int, Int, Int) -> Quick ()
setFillColour (r, g, b) = do
					a <- State.get
					State.put (a { fred = r, fgreen = g, fblue = b})

--Get the last colour used to fill in shapes
getFillColour ::  Quick (Int, Int, Int)
getFillColour = do
				a <- State.get
				return (fred a, fgreen a, fblue a)

--Set the colour which lines will be drawn with
setLineColour :: (Int, Int, Int) -> Quick ()
setLineColour (r, g, b) = do
					a <- State.get
					State.put (a { lred = r, lgreen = g, lblue = b})

--Get the last colour used to draw lines
getLineColour :: Quick (Int, Int, Int)
getLineColour = do
				a <- State.get
				return (lred a, lgreen a, lblue a)

--Set weither shapes are to be filled in or left transparent
setFill :: Bool -> Quick ()
setFill x = do
			a <- State.get
			State.put ( a {pfill = x})

--Get if shapes are being filled in
getFill :: Quick (Bool)
getFill = do
			a <- State.get
			return (pfill a)

--Set the background colour, will only be performed on the next cycle
setBackgroundColour :: (Int, Int, Int) -> Quick ()
setBackgroundColour (r, g, b) = do
							a <- State.get
							State.put (a { bgred = r, bggreen = g, bgblue = b})
							State.liftIO (set (head (drawPanel a)) [bgcolor := (rgb r g b)])

--Get the colour the background was last set to
getBackgroundColour :: Quick (Int, Int, Int)
getBackgroundColour = do
						a <- State.get
						return (bgred a, bggreen a, bgblue a)

setLineThickness :: Int -> Quick ()
setLineThickness var = do 
						a <- State.get
						State.put (a {lineThick = var})
						
setCentreMode :: Bool -> Quick ()
setCentreMode var = do
						a <- State.get
						State.put (a { centreMode = var})

--Draw a line between the 2 given tuples, (x1,y1) (x2,y2)
linePro :: DC a -> (Int, Int) -> (Int, Int) -> Quick ()
linePro dc xy1 xy2 = do
						rec <- State.get
						implementColours rec dc
						State.liftIO (line dc (pt (fst xy1) (snd xy1)) (pt (fst xy2) (snd xy2)) [])

--Draws an ellipse enclosed by the given rectangle, whose top left point is (x,y) and width and height is (w,h)
ellipsePro :: DC a -> (Int, Int) -> (Int, Int) -> Quick ()
ellipsePro dc xy wh = do 
					a <- State.get
					implementFill dc (pfill a)
					implementColours a dc
					xy <- return (implementCentre xy wh (centreMode a))
					State.liftIO (ellipse dc (rect  (pt (fst xy) (snd xy)) (sz (fst wh) (snd wh))) [])

--Draws a circles whose centre is at (x,y) and radius is r
circlePro :: DC a -> (Int, Int) -> Int -> Quick ()
circlePro dc xy r = do 
					a <- State.get
					implementFill dc (pfill a)
					implementColours a dc
					State.liftIO (circle dc (pt (fst xy) (snd xy)) r [])	

--Draws and arc, whose ellipse fits within the rectangle with top point (x,y) and width and height of (w,h), the arc section drawn is given by the angle (start angle, end angle)
arcPro :: DC a -> (Int, Int) -> (Int,Int) -> (Double, Double) -> Quick ()
arcPro dc xy wh rg = do
				a <- State.get
				implementFill dc (pfill a)
				implementColours a dc
				xy <- return (implementCentre xy wh (centreMode a))
				State.liftIO (ellipticArc  dc (rect (pt (fst xy) (snd xy)) (sz (fst wh) (snd wh))) (fst rg) (snd rg) [])

--Draws a point at the given location (x,y)
pointPro :: DC a -> (Int, Int) -> Quick ()
pointPro dc xy = do
					a <- State.get
					implementFill dc (pfill a)
					implementColours a dc
					State.liftIO ( drawPoint dc (pt (fst xy) (snd xy)) [] )

--Draws a polygon in the order given in the list of points [(x,y)], the first and the last will be connected and possibly filled in
polygonPro :: DC a -> [(Int, Int)] -> Quick ()
polygonPro dc tl = do
					a <- State.get
					implementFill dc (pfill a)
					implementColours a dc
					State.liftIO ( polygon dc (tlTopl tl) [])

--Draws a polyline connecting each point in the list [(x,y)] the first and last point will not be connected.
polylinePro :: DC a -> [(Int, Int)] -> Quick ()
polylinePro dc tl = do
					a <- State.get
					implementFill dc (pfill a)
					implementColours a dc
					State.liftIO ( polyline dc (tlTopl tl) [])

--Converts a list of tuples to a list of Points
tlTopl :: [(Int,Int)] -> [Point]
tlTopl lt = map tupleToPoint lt

--Converts a tuple to a Point
tupleToPoint :: (Num b) => (b,b) -> Point2 b
tupleToPoint t = (pt (fst t) (snd t))

--Draws a rectangle, whose top left point is (x,y) and width and height is (h,w)
rectPro :: DC a -> (Int ,Int) -> (Int ,Int) -> Quick ()
rectPro dc xy wh = do 
					a <- State.get
					implementFill dc (pfill a)
					implementColours a dc
					xy <- return (implementCentre xy wh (centreMode a))
					State.liftIO (drawRect dc (rect  (pt (fst xy) (snd xy)) (sz (fst wh) (snd wh))) [])

--Draws text, the text is given by String, centred at (x,y) at an angle of Angle
drawTextPro :: DC a -> String -> (Int, Int) -> Double -> Quick ()
drawTextPro dc str xy ang = do
						State.liftIO (rotatedText dc str (pt (fst xy) (snd xy)) ang [])

--Implements weither the shape should be filled in or not
implementFill :: DC a -> Bool -> Quick ()
implementFill dc x = do
			if x == True
				then State.liftIO (set dc [brushKind := BrushSolid])
				else State.liftIO (set dc [brushKind := BrushTransparent])

--Implements what line colour and fill colour is to be used for the shape
implementColours :: Quicksilver -> DC a -> Quick ()
implementColours a dc = do
					State.liftIO (set dc [brushColor := (rgb (fred a) (fgreen a) (fblue a)), penColor := (rgb (lred a) (lgreen a) (lblue a)), penWidth := lineThick a])

implementCentre:: (Int, Int) -> (Int, Int) -> Bool -> (Int, Int)
implementCentre xy _ False = xy
implementCentre xy wh True = ((fst xy) - (fst wh) `div` 2, (snd xy) - (snd wh) `div` 2)
					
--Returns if the right mouse button was clicked
getRightClick :: Quick Bool
getRightClick = do
				a <- State.get
				return (rightClick a)

--Returns if the left mouse button was clicked
getLeftClick :: Quick Bool
getLeftClick = do
				a <- State.get
				return (leftClick a)

--Gets the location of the mouse
getMousePosition :: Quick (Int, Int)
getMousePosition = do
			a <- State.get
			return ((mouseX a), (mouseY a))
			
getKeysPressed :: Quick [Char]
getKeysPressed = do
				a <- State.get
				return (charNumPressed a)

--Prints a string to the terminal
stringToTerminal :: String -> Quick ()
stringToTerminal str = do
						State.liftIO (putStr str)

--Prints anything implementing Show to the terminal
printToTerminal :: Show a => a -> Quick ()
printToTerminal s = do State.liftIO(putStr (show s))

--Return a random number between the given range
randomNumRange :: (Random a) =>(a,a) -> Quick a
randomNumRange x = do State.liftIO (randomRIO x)

drawPixelList :: DC a -> [(Int,Int,Int)] -> (Int,Int) -> (Int,Int) -> Quick ()
drawPixelList dc listC (x,y)(sx, sy) = do
									image <- State.liftIO(imageCreateFromPixels (sz sx sy) (tupleToColour listC))
									State.liftIO (drawImage dc image (pt x y) [])
									return ()
									
tupleToColour :: [(Int,Int,Int)] -> [Color]
tupleToColour [] = []
tupleToColour (x:xs) = (toColour x):(tupleToColour xs)

toColour :: (Int,Int,Int) -> Color
toColour (r,g,b) = rgb r g b

blitImage dc file xy = do 
					State.liftIO (C.catch (withBitmapFromFile file (displayImage dc xy)) imageFail)

displayImage dc xy bm = do drawBitmap dc bm (tupleToPoint xy) True []

imageFail e = do
				putStr (show (e :: C.SomeException))
				exitWith (ExitFailure (-1))
