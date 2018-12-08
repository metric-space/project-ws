module Main where

import Prelude
import Data.Array ((!!), length, mapWithIndex)
import Data.Maybe (fromJust, maybe)
import Data.Traversable
import Graphics.Canvas (getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth, Context2D, Rectangle(..), setFillStyle, fillRect, fillText, strokeRect)
import Partial.Unsafe (unsafePartial)
import Data.Int (toNumber)


import Effect (Effect)
import Effect.Console (log)


play_map :: Array (Array Int)
play_map = [
  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,2,2,2,2,2,0,0,0,0,3,0,3,0,3,0,0,0,1],
  [1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,3,0,0,0,3,0,0,0,1],
  [1,0,0,0,0,0,2,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,2,2,0,2,2,0,0,0,0,3,0,3,0,3,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,0,4,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,0,0,0,0,5,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,0,4,0,0,0,0,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,0,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,4,4,4,4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
  [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
]


block_width :: Int
block_width = 30


block_height :: Int
block_height = 30


-- make new type to hold information, plain Rectangle isn't cutting it
type NuRectangle = { r :: Rectangle , colour  :: String }


to_nurect :: Int -> Int -> Int -> Int -> Int ->  NuRectangle
to_nurect i j x bw bh = { r: rectangle, colour: (show x)} 
                         where rectangle :: Rectangle 
                               rectangle = {x: (toNumber (i*bw)), y: (toNumber (j*bh)), width: (toNumber bw), height: (toNumber bh)}


-- big fat code smell here
play_map_to_nurect_array :: Int -> Int  -> Array (Array Int) -> Array (Array NuRectangle)
play_map_to_nurect_array block_w block_h play_map = 
  mapWithIndex inner_func  play_map

  where inner_func :: Int -> Array Int -> Array NuRectangle
        inner_func index array_ = mapWithIndex (\j x -> to_nurect index j x block_w block_h) array_ 
                                                        
        
string_to_color :: String -> String
string_to_color "4" = "red"
string_to_color "1" = "blue"
string_to_color "2" = "yellow"
string_to_color "3" = "green"
string_to_color _   = "white"


render_nu_rect :: Context2D -> NuRectangle -> Effect Unit
render_nu_rect ctx r = do 
                         setFillStyle ctx (string_to_color r.colour)
                         fillRect ctx r.r
                         setFillStyle ctx "black"
                         strokeRect ctx r.r 
                         fillText ctx r.colour (r.r.x + 10.0) (r.r.y + 10.0) 


render_play_map :: Context2D -> Array (Array NuRectangle) -> Effect Unit
render_play_map ctx x = traverse_ (traverse_ (render_nu_rect ctx)) x  
 

main :: Effect Unit
main = do
  let w = (length $ play_map) * block_width
      h =  (length (unsafePartial (fromJust $ (play_map !! 0)))) * block_height
  mcanvas <- getCanvasElementById "canvas"
  -- fuck the unsafePartial stuff here, it's as bad as clojurescript's nil bs
  -- cleanup after
  let canvas = unsafePartial (fromJust mcanvas)
      play_map_ = play_map_to_nurect_array block_width block_height play_map
  ctx <- getContext2D canvas
  _ <- setCanvasWidth canvas (toNumber w)
  _ <- setCanvasHeight canvas (toNumber h)

  render_play_map ctx play_map_
