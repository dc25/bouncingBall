{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map (fromList, elems)
import Data.Text as DT (Text, pack, append)
import GHCJS.DOM.EventM (mouseOffsetXY) 
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Control.Monad.Trans (liftIO)
import System.Random

type Point = (Double,Double)
type Vector = (Double,Double)

plus :: (Double, Double) -> (Double, Double) -> (Double, Double)
plus (x0,y0) (x1, y1) = (x0+x1, y0+y1)

vbounce :: (Double, Double) -> (Double, Double) 
vbounce (x0,y0) = (x0, -y0)

hbounce :: (Double, Double) -> (Double, Double) 
hbounce (x0,y0) = (-x0, y0)

bounce = vbounce.hbounce

height = 400
width = 600

vflip :: Double -> Double
vflip y = fromIntegral height - y

data Color = Red | Green | Blue | Orange | Yellow | Purple deriving (Show, Bounded, Enum)

data Cmd = Tick | Pick (Int, Int) | Poke Int

data Ball  = Ball { position :: Point,
                    velocity :: Vector,
                    radius   :: Double,
                    color    :: Text
                  } deriving (Eq, Ord)

svgns :: Maybe Text
svgns = (Just "http://www.w3.org/2000/svg")

updateFrequency :: NominalDiffTime
updateFrequency = 0.1

acceleration :: Vector
acceleration = (0.0, -3.0)

fall :: Ball -> Ball
fall b = 
    let b' = b { position = position b `plus` velocity b,
                 velocity = velocity b `plus` acceleration }

        above = (snd.position) b' > 0.0 
        between = (fst.position) b' > 0.0 && (fst.position) b' < fromIntegral width 

        b'' = if above
              then if between
                   then b'
                   else b' { velocity = hbounce (velocity b) }  
              else if between
                   then b' { velocity = vbounce (velocity b) } 
                   else b' { velocity = bounce  (velocity b) }  
    in b''

update :: Cmd -> (StdGen ,[Ball]) -> (StdGen ,[Ball])
update (Pick (x,y)) (gen,cs) = 
    let position = (fromIntegral x, vflip $ fromIntegral y)
        (horizontalVelocity , gen') = randomR ((-25.0), 25.0) gen 
        velocity = (horizontalVelocity,0.0)
        (radius, gen'') = randomR (10.0, 20.0) gen'
        (colorIndex, gen''') = 
            randomR (fromEnum (minBound :: Color), 
                     fromEnum (maxBound :: Color) ) gen''

        color = toEnum colorIndex :: Color
        ball = Ball position velocity radius $ (pack.show) color
    in (gen''', ball : cs)

update Tick (gen,cs) = (gen, fmap fall cs)

update (Poke index) (gen,cs) = 
    let (cs0, cs1) = splitAt index cs 
    in (gen, cs0 ++ tail cs1)

showBall :: MonadWidget t m => (Int,Ball) -> Dynamic t () -> m (Event t Cmd)
showBall (index, Ball (x,y) _ radius color ) _  = do
    let circleAttrs = fromList [ ( "cx",     pack $ show x)
                               , ( "cy",     pack $ show $ vflip y)
                               , ( "r",      pack $ show radius)
                               , ( "style",  "fill:" `DT.append` color)
                               ] 

    (el,_) <- elStopPropagationNS svgns "g" Mousedown $ 
                 elDynAttrNS' svgns "circle" (constDyn circleAttrs) $ return ()

    return $ fmap (const $ Poke index) $ domEvent Mousedown el

draw :: MonadWidget t m => Dynamic t [Ball] -> m (Event t Cmd)
draw balls = do
    let ballMap = fmap (fromList.(\b -> zip (zip [0..] b) $ repeat ())) balls
    pokeEventWithKeys <- listWithKey ballMap showBall
    return $ switch $ (leftmost . elems) <$> current pokeEventWithKeys

view :: MonadWidget t m => Dynamic t [Ball]  -> m (Event t Cmd)
view balls = do
    tick <- tickLossy  updateFrequency =<< liftIO getCurrentTime

    let attrs = constDyn $ 
                    fromList 
                        [ ("width" , pack $ show width)
                        , ("height", pack $ show height)
                        , ("style" , "border:solid; margin:8em")
                        ]

    (elm, ev) <- elDynAttrNS' svgns "svg" attrs $ draw balls

    mouseEvent <- wrapDomEvent 
                      (_element_raw elm) 
                      (onEventName Mousedown) 
                      mouseOffsetXY

    return $ leftmost [ ev 
                      , fmap Pick mouseEvent 
                      , fmap (const Tick) tick 
                      ]

main = mainWidget $ do
    gen <- liftIO getStdGen
    rec 
        balls <- fmap snd <$> foldDyn update (gen, []) event
        event <- view balls
    return ()


