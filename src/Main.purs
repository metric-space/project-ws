module Main where

import Prelude
import Control.Alt           (alt)
import Control.MonadZero     (guard)
import Data.Array            ((!!), length, mapWithIndex, range, concat, nub)
import Data.Bifunctor        (bimap, lmap, rmap)
import Data.Int              (toNumber, floor)
import Data.Maybe            (fromMaybe, maybe, Maybe(..))
import Data.Traversable      (traverse_)
import Data.Tuple            (Tuple(..), fst, snd)
import Effect                (Effect)
import Effect.Console        (log)
import FRP.Event             (subscribe, fold, Event)
import FRP.Event.Keyboard    (down)
import Graphics.Canvas       (getCanvasElementById, getContext2D, setCanvasHeight, 
                              setCanvasWidth, setStrokeStyle, Context2D, Rectangle(..), 
                              setFillStyle, fillRect, fillText, strokeRect, CanvasElement,
                              lineTo, stroke, moveTo, clearRect, beginPath)
import Math                  (cos, sin, abs)


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


block_width :: Number
block_width = 30.0


block_height :: Number
block_height = 30.0


vel :: Number
vel = 0.15


rvel :: Number
rvel = 0.09


type State = { pos :: Tuple Number Number,
               dir :: Tuple Number Number,
               cam :: Tuple Number Number }


string_to_color :: String -> String
string_to_color "4" = "red"
string_to_color "1" = "blue"
string_to_color "2" = "yellow"
string_to_color "3" = "green"
string_to_color _   = "white"


-- =====================================================================================
--
--                           Drawing Utils
--
-- ====================================================================================


type NuRectangle = { r :: Rectangle , colour  :: String }


to_nurect :: Int -> Int -> Int -> Number -> Number ->  NuRectangle
to_nurect i j x bw bh = { r: rectangle, colour: (show x)} 
                         where x_coord :: Number
                               x_coord = (toNumber i)*bw

                               y_coord :: Number
                               y_coord =  (toNumber j)*bh

                               rectangle :: Rectangle 
                               rectangle = {x: x_coord, y: y_coord, 
                                            width:  bw, 
                                            height: bh}


play_map_ :: Array (Array NuRectangle)
play_map_ = play_map_to_nurect_array block_width block_height play_map




-- big fat code smell here
play_map_to_nurect_array :: Number -> Number  -> Array (Array Int) -> Array (Array NuRectangle)
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
 

draw_line :: Context2D -> Tuple Number Number -> Tuple  Number Number -> Effect Unit
draw_line ctx (Tuple x1 y1) (Tuple x2 y2) = do
                                             moveTo ctx (x1*block_width) (y1*block_height)
                                             lineTo ctx (x2*block_width) (y2*block_height)
                                             stroke ctx


draw_player :: Context2D -> State -> Effect Unit
draw_player ctx a = do
                     let p@(Tuple x y) = a.pos
                     -- render arrow
                     setStrokeStyle ctx "green"
                     beginPath ctx
                     draw_line ctx p (add p a.dir)
                     setStrokeStyle ctx "black"
                     -- render dot
                     setFillStyle ctx "red"
                     fillRect ctx {x: x*block_width, 
                                   y: y*block_height, 
                                   width:3.0, height: 3.0 }


-- ============================================================================
--
--                      Player movement
--
-- ============================================================================


move_up :: State -> State
move_up s =  s { pos = add s.pos (bimap (_ * vel) (_ * vel) s.dir)}


move_down :: State -> State
move_down s = s { pos = sub s.pos (bimap (_ * vel)  (_ * vel) s.dir)}


rotate_vector :: Number -> (Tuple Number Number) -> (Tuple Number Number)
rotate_vector angle (Tuple x y) = let x_ = x*(cos angle)-y*(sin angle)
                                      y_ = x*(sin  angle) + y*(cos angle)
                                  in (Tuple x_ y_)



rotate_cw :: State -> State
rotate_cw a =  a {dir = rotate_vector rvel a.dir , 
                  cam = rotate_vector rvel a.cam}


rotate_ccw :: State -> State
rotate_ccw a =  a {dir = rotate_vector (-rvel) a.dir 
                 , cam = rotate_vector (-rvel) a.cam}


guard_against :: State -> Maybe State
guard_against z@{pos: (Tuple x y)} = do 
                                      let x_ = floor x 
                                          y_ = floor y 
                                      a <- play_map !! x_ 
                                      b <- a !! y_ 
                                      guard $ b == 0 
                                      pure z


movement :: String -> State  -> State
movement svalue  s = case svalue of "ArrowUp" -> fromMaybe s (guard_against <<< move_up $ s)
                                    "ArrowDown" -> fromMaybe s (guard_against <<< move_down $ s)
                                    "ArrowLeft" -> rotate_ccw s
                                    "ArrowRight" -> rotate_cw s
                                    _ -> s


-- ============================================================================
--
--                  Animation stuff  
--
-- ============================================================================


animation_fn :: Context2D -> State -> Effect Unit
animation_fn ctx a@{pos: (Tuple x_ y_)} = do
                                          let x = floor x_
                                              y = floor y_

                                              e :: Array (Tuple Int Int)
                                              e = (nub <<< concat <<< (map (\x -> x.explored_blocks))) $ (dda 1 a)
                                          log "rendered"
                                          log <<< show $ e
                                          render_play_map ctx play_map_
                                          -- render block
                                          setFillStyle ctx "rgba(187, 143, 206, 0.5)"
                                          fillRect ctx {x: (toNumber x)*block_width, 
                                                        y: (toNumber y)*block_height, 
                                                        width: block_width, 
                                                        height: block_height}
                                          draw_player ctx a


-- =====================================================================================
--
--                   FRP events
--
-- =====================================================================================


down_with_init_point :: Event String
down_with_init_point = pure "e" `alt` down


position_stream :: State ->  Event State
position_stream i = fold movement down_with_init_point i 


-- =====================================================================================
--
--                   DDA algorithm
--
-- =====================================================================================


type DDAR = {wall_block      :: Tuple Int Int, 
             explored_blocks :: Array (Tuple Int Int),
             x               :: Boolean}


type RSEARCH = { continue :: Boolean ,
                 searched :: Array (Tuple Int Int),
                 accum    :: (Tuple Number Number), 
                 x_side   :: Boolean}


type STEP = {step_x  :: Number,
             step_y  :: Number,
             sdist_x :: Number,
             sdist_y :: Number}


camera_position :: Number -> Number -> Number
camera_position x w = (2.0*x/w) - 1.0


calculate_side_dist :: Number -> Number -> Number -> Number -> Tuple Number Number
calculate_side_dist ray_dir d_  map_ pos_ = if ray_dir < 0.0
                                                then (Tuple (d_*(1.0 - (pos_ - map_))) (-1.0))
                                                else (Tuple (d_*(pos_ - map_)) 1.0)


side_dists :: (Tuple Number Number) -> (Tuple Number Number) -> (Tuple Number Number) -> (Tuple Number Number) -> STEP
side_dists (Tuple ray_dir_x ray_dir_y) (Tuple dx dy) (Tuple map_x map_y) (Tuple pos_x pos_y) 
     = let (Tuple s_dx step_x) = calculate_side_dist ray_dir_x dx map_x pos_x 
           (Tuple s_dy step_y) = calculate_side_dist ray_dir_y dy map_y pos_y
       in {step_x:  step_x , 
           step_y:  step_y, 
           sdist_x: s_dx,
           sdist_y: s_dy}


any_hit :: (Tuple Int Int) -> Boolean
any_hit (Tuple x y) = case ((play_map !! x) >>= (_ !! y)) of Nothing  -> true
                                                             Just 0 -> false
                                                             _ -> true


hit_search :: RSEARCH -> (Tuple Number Number) -> (Tuple Number Number) -> STEP -> DDAR
hit_search {continue,searched,accum,x_side} map_ d@(Tuple dx dy) s@{step_x,step_y} = 
      if continue == true
          then let x_bias = (fst accum) < (snd accum)
                   map__  = if x_bias
                              then lmap (_ + step_x) map_
                              else rmap (_ + step_y) map_
                   accum_ = if x_bias
                              then lmap (_ + dx) accum
                              else rmap (_ + dy) accum
                   nmap   = bimap floor floor map_
                   hit    = any_hit nmap
                in if hit == true
                         then hit_search {continue: false, 
                                          searched: searched, 
                                          accum: accum_, 
                                          x_side: x_bias } map_ d s
                         else hit_search {continue: true , 
                                          searched:  [nmap] <> searched ,
                                          accum: accum_ , x_side:x_bias} map_ d s 
                   
          else {wall_block: (bimap floor floor map_), explored_blocks: searched, x: x_side} 
             
            
explore :: (Tuple Number Number) -> (Tuple Number Number) -> STEP -> DDAR
explore map_ deltas s@{sdist_x,sdist_y} = let start :: RSEARCH
                                              start = {continue: true, searched: [], accum: (Tuple sdist_x sdist_y), x_side: false}
                                          in  hit_search start map_ deltas s


dda_mini :: Number -> Number -> State ->  DDAR
dda_mini x w s = let camera_x                    = camera_position x w
                     f                           = (_ * camera_x)
                     g                           = \x -> abs (1.0 / x)
                     ray_dir                     = add s.dir (bimap f f s.cam)
                     map_                        = bimap (toNumber <<< floor) (toNumber <<< floor) s.pos
                     deltas                      = bimap g g ray_dir
                     step                        = side_dists ray_dir deltas map_ s.pos
                 in explore map_ deltas step

              
dda :: Int -> State -> Array DDAR
dda w state = map (\x -> dda_mini (toNumber x) (toNumber w) state)  (range 0 (w - 1))           


-- =====================================================================================
--
--                   Main
--
-- =====================================================================================


init_state :: State
init_state = {pos: Tuple 1.0 1.0, 
              dir: Tuple (-1.0) 0.0, 
              cam: Tuple 0.0 0.66}


get_crackin :: CanvasElement -> Number -> Number -> Effect Unit
get_crackin canvas w h  = do
    ctx <- getContext2D canvas
    _ <- setCanvasWidth canvas w
    _ <- setCanvasHeight canvas h

    render_play_map ctx play_map_
    _ <- subscribe (position_stream init_state) (animation_fn ctx) 
    pure unit
    
    
main :: Effect Unit
main = do
  let w = (toNumber map_width) * block_width
      h = (toNumber map_height) * block_height
  canvas <- getCanvasElementById "canvas" 
  case canvas of Nothing -> log "Canvas element not found!! check ID!!"
                 Just c -> get_crackin c w h 
