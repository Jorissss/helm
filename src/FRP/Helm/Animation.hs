{-| Contains all data structures and functions for creating and stepping animations. -}
module FRP.Helm.Animation (
  -- * Types
  Frame,
  Animation,
  -- * Creating
  absolute,
  relative,
  -- * Animating
  animate,
  formAt,
  length
) where

import Prelude hiding (length)

import FRP.Elerea.Simple
import Control.Applicative
import FRP.Helm.Graphics (Form,blank)
import FRP.Helm.Time (Time)
import Data.Maybe (fromJust)
import Data.List (find)

{-| A type describing a single frame in an animation. A frame consists of a time at
    which the frame takes place in an animation and the form which is how the frame
    actually looks when rendered. -}
type Frame = (Time, Form)


{-| A type describing an animation consisting of a list of frames. -}
type Animation = [Frame]

{-| Creates an animation from a list of frames. The time value in each frame
    is absolute to the entire animation, i.e. each time value is the time
    at which the frame takes place relative to the starting time of the animation.
 -}
absolute :: [Frame] -> Animation
absolute = id

{-| Creates an animation from a list of frames. The time value in each frame
    is relative to other frames, i.e. each time value is the difference
    in time from the last frame.

    > relative [(100, picture1), (100, picture2), (300, picture3)] == absolute [(100, picture1), (200, picture2), (500, picture3)]
 -}
relative :: [Frame] -> Animation
relative = scanl1 (\acc x -> (fst acc + fst x, snd x))

{-| Creates a signal contained in a generator that returns the current form in the animation when sampled from
    a specific animation. The second argument is a signal generator containing a signal that
    returns the time to setup the animation forward when sampled. The third argument is a
    signal generator containing a signal that returns true to continue animating
    or false to stop animating when sampled. -}
animate :: Animation -> SignalGen (Signal Time) -> SignalGen (Signal Bool) -> SignalGen (Signal Form)
animate [] _ _ = return $ return blank
animate anim dt cont = do
  dt1 <- dt
  cont1 <- cont
  progress <- transfer2 0 (\t r animT -> if r then 0 else cycleThisAnim (animT + t)) dt1 cont1

  return $ fromJust <$> formAt anim <$> progress
    where
      cycleThisAnim = resetOnEnd anim

{-| The form that will be rendered for a specific time in an animation. -}
formAt :: Animation -> Time -> Maybe Form
formAt anim t = snd <$> find (\frame -> t < fst frame) anim

{-| The amount of time one cycle of the animation takes. -}
length :: Animation -> Time
length [] = 0
length anim = maximum $ times anim

{-| A list of all the time values of each frame in the animation. -}
times :: Animation -> [Time]
times = map fst

{-| Given an animation, a function is created which resets the time of the animation
    if the animation was finished. -}
resetOnEnd :: Animation -> Time -> Time
resetOnEnd anim = resetOnEnd' (length anim)

{-| Helper function which resets a timer if it reached a given number. -}
resetOnEnd' :: Time -> Time -> Time
resetOnEnd' l t
  | t >= l = 0
  | otherwise = t
