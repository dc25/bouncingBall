{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map as DM (Map, fromList, elems)
import Data.Text as DT (Text, pack, append)
import GHCJS.DOM.EventM (mouseOffsetXY) 
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Control.Monad.Trans (liftIO)
import System.Random
import Control.Monad.Random

type Point = (Double,Double)
type Vector = (Double,Double)

plus :: (Double, Double) -> (Double, Double) -> (Double, Double)
plus (x0,y0) (x1, y1) = (x0+x1, y0+y1)

dot :: (Double, Double) -> (Double, Double) -> (Double, Double)
dot (x0,y0) (x1, y1) = (x0*x1, y0*y1)

height = 400
width = 600

vflip :: Double -> Double
vflip y = fromIntegral height - y

data Color = Red | Green | Blue | Orange | Purple deriving (Show, Bounded, Enum)

data Cmd = Tick | Pick (Int, Int) | Pop Int

data Ball  = Ball { position :: Point
                  , velocity :: Vector
                  , radius   :: Double
                  , color    :: Text
                  } 

data Model = Model { gen   ::  StdGen 
                   , balls ::  [Ball] 
                   }

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

updateFrequency :: NominalDiffTime
updateFrequency = 0.1

acceleration :: Vector
acceleration = (0.0, -3.0)

dampen = 1.0

evaluateMove :: (Point -> Double) -> Ball -> Int -> (Double, Double)
evaluateMove getComponent b limit = 
    let rad = radius b
        pos = getComponent $ position b 
        vel = getComponent $ velocity b
        acc = getComponent acceleration

        posForward = pos + vel
        velForward = vel + acc

        -- back the acceleration out then negate and dampen
        velUndo = -((vel - acc)*dampen)  
        posReverse = pos + velUndo
        velReverse = velUndo + acc

        ok =    posForward >= rad 
             && posForward <= fromIntegral limit - rad

        posNew = if ok then posForward else posReverse
        velNew = if ok then velForward else velReverse
    in (posNew, velNew)

fall :: Ball -> Ball
fall b = 
    let (hPosNew, hVelNew) = evaluateMove fst b width
        (vPosNew, vVelNew) = evaluateMove snd b height
    in b { position = (hPosNew, vPosNew)
         , velocity = (hVelNew, vVelNew) 
         }

newBall :: (RandomGen g) => (Int,Int) -> (Rand g Ball)
newBall (x,y) = do
    hVelocity <- getRandomR (-25.0, 25.0) 
    vVelocity <- getRandomR (0.0, 15.0) 
    radius <- getRandomR (10.0, 30.0) 
    let minColor = fromEnum (minBound :: Color)
        maxColor = fromEnum (maxBound :: Color)
    colorIndex <- getRandomR (minColor, maxColor) 
    let position = (fromIntegral x, vflip $ fromIntegral y)
        velocity = (hVelocity, vVelocity)
        colorText = (pack.show) (toEnum colorIndex :: Color)
        ball = Ball position velocity radius colorText
    return ball

update :: Cmd -> Model -> Model
update (Pick location) (Model gen cs)  = 
    let (ball, newGen) = runRand (newBall location) gen 
    in Model newGen (ball : cs)

update Tick model@(Model _ cs) = model {balls =(fmap fall cs)}

update (Pop index) model@(Model _ cs) = 
    let (cs0, cs1) = splitAt index cs 
    in model {balls = cs0 ++ tail cs1}

ballToAttrs :: Ball -> Map Text Text
ballToAttrs (Ball (x,y) _ radius color) =
    DM.fromList [ ( "cx",     pack $ show x)
                , ( "cy",     pack $ show $ vflip y)
                , ( "r",      pack $ show radius)
                , ( "style",  "fill:" `DT.append` color)
                ] 

showBall :: MonadWidget t m => Int -> Dynamic t Ball -> m (Event t Cmd)
showBall index dBall  = do
    let dCircleAttrs = fmap ballToAttrs dBall

    (el,_) <- elStopPropagationNS svgns "g" Mousedown $ 
                 elDynAttrNS' svgns "circle" dCircleAttrs $ return ()

    return $ fmap (const $ Pop index) $ domEvent Mousedown el

view :: MonadWidget t m => Dynamic t Model -> m (Event t Cmd)
view model = do
    tickEvent <- tickLossy  updateFrequency =<< liftIO getCurrentTime

    let attrs = constDyn $ 
                    DM.fromList 
                        [ ("width" , pack $ show width)
                        , ("height", pack $ show height)
                        , ("style" , "border:solid; margin:8em")
                        ]

        ballMap = fmap (DM.fromList.(\b -> (zip [0..] b) ).balls) model

    (elm, dPopEventMap) <- elDynAttrNS' svgns "svg" attrs $ listWithKey ballMap showBall

    pickEvent <- wrapDomEvent 
                      (_element_raw elm) 
                      (onEventName Mousedown) 
                      mouseOffsetXY

    return $ leftmost [ fmap (const Tick) tickEvent 
                      , fmap Pick pickEvent 
                      , switch $ (leftmost . elems) <$> current dPopEventMap
                      ]

main = mainWidget $ do
    gen <- liftIO getStdGen
    rec 
        model <- foldDyn update (Model gen []) =<< view model
    return ()
