{-# LANGUAGE RecordWildCards #-}

import Data.List((\\))
import Linear.V2 (V2(V2))

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D
import qualified Helm.Graphics2D.Text as Text
import qualified Helm.Cmd as Cmd
import qualified Helm.Mouse as Mouse
import qualified Helm.Engine.SDL as SDL
import qualified Helm.Keyboard as Keyboard

import qualified Helm.Sub as Sub
import qualified Helm.Time as Time
import           Helm.Time (Time)


-- | Represents the game actions for our game.
data Action
  = DoNothing
  | Animate Double              -- ^ Animate the game with a dt.
  | ChangeVel (V2 Double)       -- ^ Change the velocity of the player.
  | Shoot                       -- ^ Shoot.


class Object a where
  position :: a -> V2 Double
  dimensions :: a -> V2 Double


collision :: (Object a, Object b) => a -> b -> Bool
collision a b = (lt ay ah <= rb by bh)
            &&  (rb ax aw >= lt bx bw)
            &&  (lt ax aw <= rb bx bw)
            &&  (rb ay ah >= lt by bh)
  where V2 ax ay = position a
        V2 aw ah = dimensions a
        V2 bx by = position b
        V2 bw bh = dimensions b
        lt xy wh = xy - (wh / 2) -- calculate left/top given x/y and width/height
        rb xy wh = xy + (wh / 2) -- calculate right/bottom given x/y and with/height

-- returns Nothing if nothing is filtered
flagFilter :: (a -> Bool) -> [a] -> Maybe [a]
flagFilter pred xs = ff False pred xs where
  ff flag pred (x:xs) = if pred x
    then fmap (x :) (ff flag pred xs)
    else ff True pred xs
  ff flag _ [] = if flag then Just [] else Nothing


-- relates can be collide
doubleFilter :: (a -> b -> Bool) -> [a] -> [b] -> ([a], [b])
doubleFilter relates (a:as) bs1 = case flagFilter (relates a) bs1 of
   Just bs2 -> doubleFilter relates as bs2
   Nothing -> (a:resa, resb)
    where (resa, resb) = doubleFilter relates as bs1

doubleFilter _ [] bs = ([], bs)


-- | Represents a block the player can hit with a bullet.
data Block = Block
  {
    blockPos   :: V2 Double
  --, blockVel   :: V2 Double
  --, blockMove  :: V2 Double -> V2 Double -> V2 Double -- ^ can take position and change velocity
  } deriving Eq

instance Object (Block) where
  position = blockPos
  dimensions b = blockDims

blockDims :: V2 Double
blockDims = V2 20 10

-- | Represents a bullet a player can shoot
data Bullet = Bullet
  {
    bulletPos   :: V2 Double
  } deriving Eq

instance Object (Bullet) where
  position = bulletPos
  dimensions b = bulletDims

bulletDims :: V2 Double
bulletDims = V2 5 5

bulletVel :: V2 Double
bulletVel = V2 0 (-0.8)

type Level = [Block]

-- | Represents the game state.
data Model = Model
  { playerPos   :: V2 Double
  , playerVel   :: V2 Double
  , blocks      :: [Block]
  , bullets     :: [Bullet]
  , currentLev  :: Int
  }

playerDims :: V2 Double
playerDims = V2 10 10


initial :: (Model, Cmd SDLEngine Action)
initial =
  ( Model
      { playerPos = V2 400 550
      , playerVel = V2 0 0
      , blocks = [Block { blockPos = fmap fromIntegral (V2 (100 * i) (100 * j)) } | i <- [1..7], j <- [1..3]]
      , bullets = []
      , currentLev = 1
      }
  , Cmd.none
  )
  where V2 w h = windowDims


windowDims :: V2 Int
windowDims = V2 800 600

onscreen :: Object a => a -> Bool
onscreen a = x >= 0 && x <= w && y >= 0 && y <= h
  where V2 x y = position a
        V2 w h = fmap fromIntegral windowDims

update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model@Model { .. } (Animate dt) =
  ( model
    { playerPos   = if validPos newPos then newPos else playerPos
    , bullets     = map moveBullet (filter onscreen [ b | b <- bullets, not $ collideswithBlocks b blocks ])
    , blocks      = [ b | b <- blocks, not $ collideswithBullets b bullets]
    }
    , Cmd.none
  )
  where
    newPos = playerPos + (fmap ((*) dt) playerVel)
    validPos (V2 x y) = x - pw / 2 >= 0 && x + pw / 2 <= sw && y - ph / 2 >= 0 && y + ph / 2 <= sh
      where V2 sw sh = fmap fromIntegral windowDims
            V2 pw ph = playerDims
    moveBullet bullet@Bullet { .. } = bullet { bulletPos = bulletPos + (fmap ((*) dt) bulletVel) }
    collideswithBlocks bullet [] = False
    collideswithBlocks bullet (b:blocks) = (collision bullet b) || (collideswithBlocks bullet blocks)

    collideswithBullets block [] = False
    collideswithBullets block (b:bullets) = (collision b block) || (collideswithBullets block bullets)

update (model@Model { .. }) (ChangeVel dvel) =
  ( model
    {
      playerVel = (dvel)
    }
  , Cmd.none
  )
update (model@Model { .. }) (Shoot) =
  ( model
    {
      bullets = bullets ++ [Bullet { bulletPos = playerPos }]
    }
  , Cmd.none
  )
update model _ = (model, Cmd.none)


subscriptions :: Sub SDLEngine Action
-- subscriptions = Mouse.moves (\(V2 x y) -> ChangePosition $ V2 (fromIntegral x) (fromIntegral y))
subscriptions = Sub.batch
  [ Keyboard.downs $ \key -> (case key of
      Keyboard.RightKey -> ChangeVel (V2 0.3 0)
      Keyboard.LeftKey  -> ChangeVel (V2 (-0.3) 0)
      _                 -> DoNothing)
  , Keyboard.presses $ \key -> (case key of
      Keyboard.SpaceKey -> Shoot
      _                 -> DoNothing)
  , Keyboard.ups $ \key -> case key of
      Keyboard.RightKey -> ChangeVel (V2 0 0)
      Keyboard.LeftKey  -> ChangeVel (V2 0 0)
      _                 -> DoNothing
  , Time.fps 60 Animate
  ]

playingOverlay :: Model -> Color -> Form SDLEngine
playingOverlay Model { .. } color =
  group
    [
      move (V2 (w/2) (h/16)) $ text $ Text.height 12 $
                                         Text.color color $
                                         Text.toText status
    ]

  where
    status = "score: " ++ (show $ length blocks) -- secondsText timeScore ++ " | " ++ printf "%.2fx speed" timeSpeed
    V2 w h = fromIntegral <$> windowDims

view :: Model -> Graphics SDLEngine
view (model@Model { .. }) = Graphics2D $
  collage
    [
      move playerPos $ filled (rgb 1 0 0) $ rect playerDims
    , group $ map viewBullet $ bullets
    , group $ map viewBlock  $ blocks
    , playingOverlay model (rgb 0 1 1)
    ]
    where viewBullet Bullet { .. } = move bulletPos $ filled (rgb 0 1 0) $ rect (bulletDims)
          viewBlock   Block  { .. } = move blockPos  $ filled (rgb 0 0 1) $ rect (blockDims)
          V2 w h = fmap fromIntegral windowDims
          V2 x y = playerPos


main :: IO ()
main = do
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowIsResizable = False
    , SDL.windowDimensions = windowDims
    }

  run engine GameConfig
    { fpsLimit = Limited 60
    , updateLimit = 10
    } GameLifecycle
    {
      initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }
