{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map (Map, fromList, toList, insert, empty, elems)
import Data.Text as DT (Text, pack, append)
import GHCJS.DOM.EventM (mouseOffsetXY) 
import Data.Time.Clock (NominalDiffTime, getCurrentTime)
import Control.Monad.Trans (liftIO)

type Point = (Double,Double)
type Vector = (Double,Double)

plus :: (Double, Double) -> (Double, Double) -> (Double, Double)
plus (x0,y0) (x1, y1) = (x0+x1, y0+y1)

minus :: (Double, Double) -> (Double, Double) -> (Double, Double)
minus (x0,y0) (x1, y1) = (x0-x1, y0-y1)

bounce :: (Double, Double) -> (Double, Double) 
bounce (x0,y0) = (x0, -y0)

height = 400
width = 600

vflip :: Double -> Double
vflip y = fromIntegral height - y

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
acceleration = (0.0, 3.0)

fall :: Ball -> Ball
fall b = 
    let b' = b { position = position b `minus` velocity b,
                 velocity = velocity b `plus` acceleration }
        b'' = if (snd.position) b' > 0.0 
              then b'
              else b' { velocity = bounce (velocity b) }
    in b''

update :: Cmd -> [Ball] -> [Ball] 
update (Pick (x,y)) cs = 
    let position = (fromIntegral x, vflip $ fromIntegral y)
        velocity = (0.0,0.0)
        radius = 50.0
        color = "Green"
        ball = Ball position velocity radius color
    in ball : cs

update Tick cs = fmap fall cs

update (Poke index) cs = 
    let (cs0, cs1) = splitAt index cs 
    in cs0 ++ tail cs1

showBall :: MonadWidget t m => (Int,Ball) -> Dynamic t () -> m (Event t Cmd)
showBall (index, Ball (x,y) _ radius color ) _  = do
    let circleAttrs = fromList [ ( "cx",     pack $ show x)
                               , ( "cy",     pack $ show $ vflip y)
                               , ( "r",      pack $ show radius)
                               , ( "style",  "fill:" `DT.append` color) ] 

    (el,_) <- elStopPropagationNS svgns "g" Mousedown $ elDynAttrNS' svgns "circle" (constDyn circleAttrs) $ return ()
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

main = mainWidget $ mdo 
    event <- view =<< foldDyn update [] event
    return ()


