module Main where

import Prelude
import Control.Alt           (alt)
import Control.MonadZero     (guard)
import Data.Array            ((!!), length, mapWithIndex)
import Data.Bifunctor        (lmap, rmap)
import Data.Int              (toNumber)
import Data.Maybe            (fromMaybe, maybe, Maybe(..))
import Data.Traversable      (traverse_)
import Data.Tuple            (Tuple(..))
import Effect                (Effect)
import Effect.Console        (log)
import FRP.Event             (subscribe, fold, Event)
import FRP.Event.Keyboard    (down)
import Graphics.Canvas       (getCanvasElementById, getContext2D, setCanvasHeight, 
                              setCanvasWidth, setStrokeStyle, Context2D, Rectangle(..), 
                              setFillStyle, fillRect, fillText, strokeRect, CanvasElement)


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


map_width :: Int
map_width = length play_map


map_height :: Int
map_height = length <<< fromMaybe [] $ (play_map !! 0)


block_width :: Int
block_width = 30


block_height :: Int
block_height = 30


string_to_color :: String -> String
string_to_color "4" = "red"
string_to_color "1" = "blue"
string_to_color "2" = "yellow"
string_to_color "3" = "green"
string_to_color _   = "white"


type NuRectangle = { r :: Rectangle , colour  :: String }


to_nurect :: Int -> Int -> Int -> Int -> Int ->  NuRectangle
to_nurect i j x bw bh = { r: rectangle, colour: (show x)} 
                         where x_coord :: Number
                               x_coord = toNumber (i*bw)

                               y_coord :: Number
                               y_coord =  toNumber (j*bh)

                               rectangle :: Rectangle 
                               rectangle = {x: x_coord, y: y_coord, 
                                            width: (toNumber bw), 
                                            height: (toNumber bh)}


play_map_ :: Array (Array NuRectangle)
play_map_ = play_map_to_nurect_array block_width block_height play_map


-- big fat code smell here
play_map_to_nurect_array :: Int -> Int  -> Array (Array Int) -> Array (Array NuRectangle)
play_map_to_nurect_array block_w block_h play_map = 
  mapWithIndex inner_func  play_map

  where inner_func :: Int -> Array Int -> Array NuRectangle
        inner_func index array_ = mapWithIndex (\j x -> to_nurect index j x block_w block_h) array_ 
                                                        
        
render_nu_rect :: Context2D -> NuRectangle -> Effect Unit
render_nu_rect ctx r = do 
                         setFillStyle ctx (string_to_color r.colour)
                         fillRect ctx r.r
                         setFillStyle ctx "black"
                         strokeRect ctx r.r 
                         fillText ctx r.colour (r.r.x + 10.0) (r.r.y + 10.0) 


render_play_map :: Context2D -> Array (Array NuRectangle) -> Effect Unit
render_play_map ctx x = traverse_ (traverse_ (render_nu_rect ctx)) x  
 

-- ============================================================================
--
--                  Animation stuff  
--
-- ============================================================================




guard_against :: Tuple Int Int -> Maybe (Tuple Int Int)
guard_against (Tuple x y) = do
                              a <- play_map !! x
                              b <- a !! y 
                              guard $ b == 0
                              pure (Tuple x y)


movement :: String -> Tuple Int Int -> Tuple Int Int
movement svalue  a@(Tuple x y) = case svalue of "ArrowUp"  -> fromMaybe a (guard_against (rmap (_ - 1) a))
                                                "ArrowDown"  -> fromMaybe a (guard_against (rmap (_ + 1) a))
                                                "ArrowLeft"  -> fromMaybe a (guard_against (lmap (_ - 1) a))
                                                "ArrowRight" -> fromMaybe a (guard_against (lmap (_ + 1) a))
                                                _ -> Tuple x y


animation_fn :: Context2D -> Tuple Int Int -> Effect Unit
animation_fn ctx (Tuple x y) = do
                                  -- log "rendered"
                                  render_play_map ctx play_map_
                                  setFillStyle ctx "rgba(187, 143, 206, 0.5)"
                                  fillRect ctx {x: toNumber (x*block_width), 
                                                  y: toNumber (y*block_height), 
                                                  width: toNumber block_width, 
                                                  height: toNumber block_height}


-- =====================================================================================
--
--                   FRP events
--
-- =====================================================================================


down_with_init_point :: Event String
down_with_init_point = pure "e" `alt` down


position_stream ::(Tuple Int Int) ->  Event (Tuple Int Int)
position_stream i = fold movement down_with_init_point i 

-- ===================================================================================


get_crackin :: CanvasElement -> Int -> Int -> Effect Unit
get_crackin canvas w h  = do
    ctx <- getContext2D canvas
    _ <- setCanvasWidth canvas (toNumber w)
    _ <- setCanvasHeight canvas (toNumber h)

    render_play_map ctx play_map_
    _ <- subscribe (position_stream (Tuple 1 1)) (animation_fn ctx) 
    pure unit
    
    
    


main :: Effect Unit
main = do
  let w = map_width * block_width
      h = map_height * block_height
  canvas <- getCanvasElementById "canvas" 
  case canvas of Nothing -> log "Canvas element not found!! check ID!!"
                 Just c -> get_crackin c w h 

  
