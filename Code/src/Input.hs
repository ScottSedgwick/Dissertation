-- |Module: Frogger.Input
module Input where

import Graphics.UI.GLUT
import Data.IORef
import Type

-- |Essentially a routing function, which calls the requisite function depending on what the current 'GameState' is
input :: IORef Env -> KeyboardMouseCallback
input m k ks mo p = get m >>= \m' -> case gameState m' of PreStart      -> inputPreStart m k ks mo p
                                                          Playing       -> inputPlaying m k ks mo p
                                                          Paused        -> inputPaused m k ks mo p
                                                          PlayerDead _  -> inputDead m k ks mo p
                                                          LevelComplete -> inputComplete m k ks mo p

-- |The input handler for when the level is complete.
--  Pressing space will advance to the next level, any other input is ignored.
inputComplete :: IORef Env -> KeyboardMouseCallback
inputComplete m c Down _ _
  | c == (Char ' ') = m $~! \e -> let nextLevel = level e + 1
                                   in (startEnv nextLevel) { frames = frames e
                                                           , time = time e
                                                           , gameState = Playing
                                                           , gameScore = gameScore e
                                                           , level = nextLevel
                                                           }
  | otherwise       = return ()
inputComplete _ _ _ _ _ = return ()

-- |The input handler for when the player is dead.
--  Pressing space will restart the game, any other input is ignored.
inputDead :: IORef Env -> KeyboardMouseCallback
inputDead m c Down _ _
  | c == (Char ' ') = m $~! \_ -> startEnv 1
  | otherwise       = return ()
inputDead _ _ _ _ _ = return ()

-- |The input handler for when the game has yet to start.
--  Any input starts the game.
inputPreStart :: IORef Env -> KeyboardMouseCallback
inputPreStart m _ _ _ _ = m $~! \e -> e {gameState = Playing}

-- |The input handler for when the level is paused.
--  Pressing space will unpause, any other input is ignored.
inputPaused :: IORef Env -> KeyboardMouseCallback
inputPaused m c Down _ _
  | c == (Char ' ') = m $~! \e -> e {gameState = Playing}
  | otherwise       = return ()
inputPaused _ _ _ _ _ = return ()

-- |The input handler for when the game is in progress.
--  'w', 'a', 's', and 'd' move the player up, left, down, and right respectively.
--  Pressing space will pause the level, and pressing Esc will quit the game.
inputPlaying :: IORef Env -> KeyboardMouseCallback
inputPlaying m c Down _ _
  | c == (Char 'w') || c == (Char 'W') = m $~! \e -> let p = player e
                                                     in e {player = setdY speed p {is_JumpingY = True
                                                                                  ,targetY = getY p + step
                                                                                  ,prev_dX = getdX p
                                                                                  ,prev_dY = getdY p
                                                                                  }
                                                          }
  | c == (Char 'a') || c == (Char 'A') = m $~! \e -> let p = player e
                                                     in e {player = setdX (-speed) p {is_JumpingX = True
                                                                                     ,targetX = getX p - step
                                                                                     ,prev_dX = getdX p
                                                                                     ,prev_dY = getdY p
                                                                                     }
                                                          }
  | c == (Char 's') || c == (Char 'S') = m $~! \e -> let p = player e
                                                     in e {player = setdY (-speed) p {is_JumpingY = True
                                                                                     ,targetY = getY p - step
                                                                                     ,prev_dX = getdX p
                                                                                     ,prev_dY = getdY p
                                                                                     }
                                                          }
  | c == (Char 'd') || c == (Char 'D') = m $~! \e -> let p = player e
                                                     in e {player = setdX speed p {is_JumpingX = True
                                                                                  ,targetX = getX p + step
                                                                                  ,prev_dX = getdX p
                                                                                  ,prev_dY = getdY p
                                                                                  }
                                                          }
  | c == (Char ' ')                    = m $~! \e -> e {gameState = Paused}
  | c == (Char '\27')                  = m $~! \e -> e {gameState = PlayerDead "You quit"}
  | otherwise                          = return ()
  where step = 32
        speed = 1.0
inputPlaying _ _ _ _ _ = return ()
