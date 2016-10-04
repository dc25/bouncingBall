{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map (Map, fromList, insert, empty)
import Data.Text as DT (Text, pack, append)
import GHCJS.DOM.EventM (mouseOffsetXY) 

type Point = (Double,Double)

data Ball  = Ball {
                     position :: Point,
                     velocity :: Point,
                     radius   :: Double,
                     color    :: Text
                  } deriving (Eq, Ord)

svgNamespace :: Maybe Text
svgNamespace = (Just "http://www.w3.org/2000/svg")

addPick :: (Int,Int) -> Map (Int, Ball) () -> Map (Int,Ball) ()
addPick (x,y) cs = 
    let position = (fromIntegral x, fromIntegral y)
        velocity = (0.0,0.0)
        radius = 10.0
        color = "Green"
        ball = Ball position velocity radius color
    in insert (length cs, ball) () cs

showCircle :: MonadWidget t m => (Int,Ball) -> Dynamic t () -> m () 
showCircle (index, Ball (x,y) _ radius color ) _  = do
    let 
        t_x =      pack $ show x
        t_y =      pack $ show y
        t_radius = pack $ show radius
        t_style  = "fill:" `DT.append` color

        circleAttrs = fromList [ ( "cx",     t_x)
                               , ( "cy",     t_y)
                               , ( "r",      t_radius)
                               , ( "style",  t_style) ] 

    elDynAttrNS' svgNamespace "circle" (constDyn circleAttrs) $ return ()
    return ()


main = mainWidget $ do 
    let attrs =   constDyn $ 
                      fromList 
                          [ ("width" , "500")
                          , ("height" , "250")
                          , ("style" , "border:solid; margin:8em")
                          ]
    rec 
        let circles = listWithKey picks showCircle

        (elm, _) <-   elDynAttrNS' svgNamespace "svg" attrs circles

        mouseEvent <- wrapDomEvent 
                          (_element_raw elm) 
                          (onEventName Mousedown) 
                          mouseOffsetXY

        picks <- foldDyn addPick empty mouseEvent

    return ()


