{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map (Map, fromList, insert, empty)
import Data.Text as DT (Text, pack, append)
import GHCJS.DOM.EventM (mouseOffsetXY) 

svgNamespace :: Maybe Text
svgNamespace = (Just "http://www.w3.org/2000/svg")

drawCircle :: MonadWidget t m => Text -> Int -> Int -> Int -> m ()
drawCircle color radius x y = do
    let 
        t_x =      pack $ show (fromIntegral x :: Double)
        t_y =      pack $ show (fromIntegral y :: Double)
        t_radius = pack $ show (fromIntegral radius :: Double)
        t_style  = "fill:" `DT.append` color

        circleAttrs = fromList [ ( "cx",     t_x)
                               , ( "cy",     t_y)
                               , ( "r",      t_radius)
                               , ( "style",  t_style) ] 

    elDynAttrNS' svgNamespace "circle" (constDyn circleAttrs) $ return ()
    return ()

type Point = (Int,Int)

addPick :: Point -> Map (Int, Point) () -> Map (Int,Point) ()
addPick point cs  = insert (length cs, point) () cs

showCircle :: MonadWidget t m => (Int,Point) -> Dynamic t () -> m () 
showCircle (index, (x,y)) _  = drawCircle "Red" 10 x y

circleBox :: MonadWidget t m => m () 
circleBox = do
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


main = mainWidget circleBox

