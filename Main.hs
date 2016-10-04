{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex
import Reflex.Dom 
import Data.Map (Map, fromList, insert, empty)
import Data.Text as DT (Text, pack, append)
import GHCJS.DOM.EventM (mouseOffsetXY) 

type Point = (Int,Int)

data Ball  = Ball {
                     position :: Point,
                     velocity :: Point,
                     radius   :: Int,
                     color    :: Text
                  } deriving (Eq, Ord)

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

addPick :: Point -> Map (Int, Ball) () -> Map (Int,Ball) ()
addPick point cs  = let ball = Ball point (0,0) 10 "Red" in insert (length cs, ball) () cs

showCircle :: MonadWidget t m => (Int,Ball) -> Dynamic t () -> m () 
showCircle (index, Ball (x,y) _ radius color ) _  = drawCircle color radius x y

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


