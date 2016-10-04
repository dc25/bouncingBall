{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map (Map, fromList, insert, empty)
import Data.Text as DT (Text, pack, append)
import GHCJS.DOM.EventM (mouseOffsetXY) 

type Point = (Double,Double)

data Ball  = Ball { position :: Point,
                    velocity :: Point,
                    radius   :: Double,
                    color    :: Text
                  } deriving (Eq, Ord)

svgNamespace :: Maybe Text
svgNamespace = (Just "http://www.w3.org/2000/svg")

addBall :: (Int,Int) -> Map (Int, Ball) () -> Map (Int,Ball) ()
addBall (x,y) cs = 
    let position = (fromIntegral x, fromIntegral y)
        velocity = (0.0,0.0)
        radius = 10.0
        color = "Green"
        ball = Ball position velocity radius color
    in insert (length cs, ball) () cs

showBall :: MonadWidget t m => (Int,Ball) -> Dynamic t () -> m () 
showBall (index, Ball (x,y) _ radius color ) _  = do
    let circleAttrs = fromList [ ( "cx",     pack $ show x)
                               , ( "cy",     pack $ show y)
                               , ( "r",      pack $ show radius)
                               , ( "style",  "fill:" `DT.append` color) ] 

    elDynAttrNS' svgNamespace "circle" (constDyn circleAttrs) $ return ()
    return ()


main = mainWidget $ do 
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

        balls <- foldDyn addBall empty mouseEvent

    return ()


