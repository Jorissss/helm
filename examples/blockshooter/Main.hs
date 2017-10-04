{-# LANGUAGE RecordWildCards #-}

import Data.List((\\))
import Linear.V2 (V2(V2))

import Helm
import Helm.Color
import Helm.Engine.SDL (SDLEngine)
import Helm.Graphics2D

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
bulletVel = V2 0 (-9)

-- | Represents the game state.
data Model = Model
  { playerPos   :: V2 Double
  , playerVel   :: V2 Double
  , blocks      :: [Block]
  , bullets     :: [Bullet]
  }


initial :: (Model, Cmd SDLEngine Action)
initial =
  ( Model
      { playerPos = V2 400 550
      , playerVel = V2 0 0
      , blocks = [Block { blockPos = fmap fromIntegral (V2 (100 * i) 100) } | i <- [1..7]]
      , bullets = []
      }
  , Cmd.none
  )
  where V2 w h = windowDims


windowDims :: V2 Int
windowDims = V2 800 600


update :: Model -> Action -> (Model, Cmd SDLEngine Action)
update model@Model { .. } (Animate dt) =
  ( model
    { playerPos   = playerPos + playerVel
    , bullets     = map moveBullet [ b | b <- bullets, not $ collideswithBlocks b blocks ]
    , blocks      = [ b | b <- blocks, not $ collideswithBullets b bullets]
    }
    , Cmd.none
  )
  where
    moveBullet bullet@Bullet { .. } = bullet { bulletPos = bulletPos + bulletVel }
    (newBullets, newBlocks) = doubleFilter collision bullets blocks
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
      Keyboard.RightKey -> ChangeVel (V2 5 0)
      Keyboard.LeftKey  -> ChangeVel (V2 (-5) 0)
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

view :: Model -> Graphics SDLEngine
view (model@Model { .. }) = Graphics2D $ collage
  [ move playerPos $ filled (rgb 1 0 0) $ square 10
  , group $ map viewBullet $ bullets
  , group $ map viewBlock  $ blocks
  ]
  where viewBullet Bullet { .. } = move bulletPos $ filled (rgb 0 1 0) $ rect (bulletDims)
        viewBlock   Block  { .. } = move blockPos  $ filled (rgb 0 0 1) $ rect (blockDims)


main :: IO ()
main = do
  engine <- SDL.startupWith $ SDL.defaultConfig
    { SDL.windowIsResizable = False
    , SDL.windowDimensions = windowDims
    }

  run engine GameConfig
    { fpsLimit        = defaultFPSLimit
    , initialFn       = initial
    , updateFn        = update
    , subscriptionsFn = subscriptions
    , viewFn          = view
    }
