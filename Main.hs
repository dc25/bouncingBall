{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map (Map, fromList, toList, insert, empty)
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

data Cmd = Tick | Pick (Int, Int)

data Ball  = Ball { position :: Point,
                    velocity :: Vector,
                    radius   :: Double,
                    color    :: Text
                  } deriving (Eq, Ord)

svgNamespace :: Maybe Text
svgNamespace = (Just "http://www.w3.org/2000/svg")

updateFrequency :: NominalDiffTime
updateFrequency = 0.1

acceleration :: Vector
acceleration = (0.0, 3.0)

fall :: ((Int, Ball),()) -> ((Int, Ball),())
fall ((i, b),()) = 
    let b' = b { position = position b `minus` velocity b,
                 velocity = velocity b `plus` acceleration }
        b'' = if (snd.position) b' > 0.0 
              then b'
              else b' { velocity = bounce (velocity b) }
    in ((i, b''),())

update :: Cmd -> Map (Int, Ball) () -> Map (Int,Ball) ()
update (Pick (x,y)) cs = 
    let position = (fromIntegral x, fromIntegral y)
        velocity = (0.0,0.0)
        radius = 10.0
        color = "Green"
        ball = Ball position velocity radius color
    in insert (length cs, ball) () cs

update Tick cs = fromList $ fmap fall $ toList cs

showBall :: MonadWidget t m => (Int,Ball) -> Dynamic t () -> m () 
showBall (index, Ball (x,y) _ radius color ) _  = do
    let circleAttrs = fromList [ ( "cx",     pack $ show x)
                               , ( "cy",     pack $ show y)
                               , ( "r",      pack $ show radius)
                               , ( "style",  "fill:" `DT.append` color) ] 

    elDynAttrNS' svgNamespace "circle" (constDyn circleAttrs) $ return ()
    return ()


main = mainWidget $ do 
    tick <- tickLossy  updateFrequency =<< liftIO getCurrentTime
    let attrs =   constDyn $ 
                      fromList 
                          [ ("width" , "500")
                          , ("height", "400")
                          , ("style" , "border:solid; margin:8em")
                          ]
    rec 
        (elm, _) <- elDynAttrNS' svgNamespace "svg" 
                        attrs $ listWithKey balls showBall 

        mouseEvent <- wrapDomEvent 
                          (_element_raw elm) 
                          (onEventName Mousedown) 
                          mouseOffsetXY

        let events = leftmost [fmap Pick mouseEvent, 
                               fmap (const Tick) tick]

        balls <- foldDyn update empty events

    return ()


