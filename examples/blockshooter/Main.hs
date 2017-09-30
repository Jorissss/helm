{-# LANGUAGE RecordWildCards #-}

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


-- | Represents a block the player can hit with a bullet.
data Block = Block
  { blockPos    :: V2 Double
  }

-- | Represents a bullet a player can shoot
data Bullet = Bullet
  { bulletPos   :: V2 Double
  }

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
      , blocks = [Block { blockPos = fmap fromIntegral (V2 (100 * i) 20) } | i <- [1..7]]
      , bullets = []
      }
  , Cmd.none
  )
  where V2 w h = windowDims

blockDims :: V2 Int
blockDims = V2 20 10

bulletDims :: V2 Int
bulletDims = V2 5 5

bulletVel :: V2 Double
bulletVel = V2 0 (-20)

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

    collideswithBlocks bullet [] = False
    collideswithBlocks bullet (b:blocks) = (collideswith bullet b) || (collideswithBlocks bullet blocks)

    collideswithBullets block [] = False
    collideswithBullets block (b:bullets) = (collideswith b block) || (collideswithBullets block bullets)

    collideswith (bullet@Bullet { .. }) (block@Block { .. }) =
      bully <= blocky && bullx >= blockx - ( ((fromIntegral blockw) / 2)) && bullx <= blockx + ((fromIntegral blockw) / 2)
      where V2 bullx bully    = bulletPos
            V2 blockx blocky  = blockPos
            V2 blockw blockh  = blockDims

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
  where viewBullet Bullet { .. } = move bulletPos $ filled (rgb 0 1 0) $ rect (fmap fromIntegral bulletDims)
        viewBlock   Block  { .. } = move blockPos  $ filled (rgb 0 0 1) $ rect (fmap fromIntegral blockDims)
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
