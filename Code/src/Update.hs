-- |Module: Frogger.Update
module Update where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Bitmap
import Data.Maybe
import Type

-- | The function which updates the game on every 'tick'
gameUpdate :: Float   -- ^ Delta-t in milliseconds
              -> Env  -- ^ The current Env
              -> Env  -- ^ The updated Env
gameUpdate n e@E{gameState = gs, time = t} = if gs == Playing then updateEnv $ e {time = t + n}
                                                              else e

-- | updateEnv is a composition of 3 functions which update the positions of moving objects, detect collision between the player and those objects, detect a collision between the player and a goal, and update the score respectively.
updateEnv :: Env -> Env
updateEnv e = let p = player e
                  coll = if is_Jumping p
                         then id
                         else hitCheck
               in scoreUpdate . coll . updateMovers $ e


-- | scoreUpdate, if the level is complete, increases the score by an amount proportional to the current level
scoreUpdate :: Env -> Env
scoreUpdate e = if gameState e == LevelComplete then e {gameScore = gameScore e + (1000 * level e)}
                                                else e

-- | updateMovers applies the update function required of all Drawables to all moving objects
updateMovers :: Env -> Env
updateMovers e = e {player = update . player $ e
                   ,roadEnemies = map update . roadEnemies $ e
                   ,riverEnemies = map update . riverEnemies $ e
                   }

-- | hitCheck acts as something of a routing function; provided the player is within bounds it calls the relevant function depending upon where on the map they are.
--   If they are it calls 'inBoundsHitCheck' to determine what to do next.
hitCheck :: Env -> Env
hitCheck e@E {player = p} = if inRange (0 - getL p, 4000) (getX p) && inRange (0, 3000) (getY p)
                            then inBoundsHitCheck e
                            else e {gameState = PlayerDead "You went out of bounds!"}

-- | inBoundsHitCheck makes use of Haskell's guarded expressions instead of a pair of if...else expressions to call the correct checking function depending upon the position of the player.
inBoundsHitCheck :: Env -> Env
inBoundsHitCheck e@E {player = p}
  | inRange (head lanes, lanes!! 6) py    = roadCheck e
  | inRange (lanes !! 6, lanes !! 12) py  = riverCheck e
  | otherwise                             = e
  where py = getY p

-- | inRange checks that a value is within a given range.
inRange :: Ord a
        => (a,a)  -- ^ The lower and upper bounds
        -> a      -- ^ The value to check
        -> Bool
inRange (l,u) n = u >= n && n >= l

-- | roadCheck checks to see if the player has been run over yet.
--   If the player is hit by a car, they die.
roadCheck :: Env -> Env
roadCheck e = let p = player e
                  coll = lookup True . map (\m -> (hasCollided p m, m)) $ roadEnemies e
               in case coll of Just m   -> e {gameState = PlayerDead "You got run over!"}
                               Nothing  -> e {player = setdX 0 p}

-- | riverCheck performs the equal yet opposite function to roadCheck in that it checks to see if the player has saved themself by jumping onto something.
--   If the player has collided with either a RiverMover or a Goal they remain alive (well, may remain alive in the Goal case).
--   Otherwise they drown.
--   If the player has collided with a Goal the function 'hitGoal' is called to deal with that.
riverCheck :: Env -> Env
riverCheck e = let p = player e
                   collRi = lookup True . map (\m -> (hasCollided p m, m)) $ riverEnemies e
                   collGo = lookup True . map (\g -> (hasCollided p g, g)) $ goals e
                in case collRi of Just m  -> riverCollision m e
                                  Nothing -> case collGo of Just g  -> hitGoal g e
                                                            Nothing -> e {gameState = PlayerDead "You drowned!"}

-- | riverCollision takes a RiverMover with which the player has collided and updates the Env accordingly.
--   If the RiverMover is a Croc, stepping on only the head will kill the player, else the player will ride it.
--   If the RiverMover is some Turtles, the player will survive if they are above water, else they might as well not be there.
--   Otherwise the player will ride whatever object it is.
riverCollision :: RiverMover -> Env -> Env
riverCollision c@Croc {} e@E {player = p} = let (cHead, cBody) = splitCroc c
                                             in if hasCollided p cHead && not (hasCollided p cBody)
                                                then e {gameState = PlayerDead "You got eaten by a crocodile!"}
                                                else e {player = setdX (getdX c) p}
riverCollision t@Turtles {aboveWater = aw} e@E {player = p} 
  = if aw then e {player = setdX (getdX t) p}
          else e {gameState = PlayerDead "Those turtles were underwater!"}
riverCollision rm e@E {player = p} = e {player = setdX (getdX rm) p}

-- | hitGoal deals with when a Goal has been collided with.
--   If that Goal wa occupied, the player dies.
--   Otherwise, that Goal is made occupied and the players position resets.
--   If all Goals are occupied, the level is complete.
hitGoal :: Goal -- ^ The Goal that has been collided with
        -> Env  -- ^ The current Env
        -> Env  -- ^ The resultant Env
hitGoal g e = let p = player e
                  gs = goals e
               in if is_Occupied g
                  then e {gameState = PlayerDead "That goal had someone on it!"}
                  else let gs' = g {is_Occupied = True} : filter (/=g) gs
                        in if all is_Occupied gs' then e {gameState = LevelComplete}
                                                  else e {player = newPlayer
                                                         ,goals = gs'
                                                         }

-- | Detecting whether or not the Frogger has collided with another Drawable
hasCollided :: Drawable a => Frogger -> a -> Bool
hasCollided f d = let xf = getX f
                      yf = getY f
                      lf = getL f
                      wf = getW f
                      xd = getX d
                      yd = getY d
                      ld = getL d
                      wd = getW d
                   in case signum ld of 1         -> xd + ld > xf &&
                                                     yd + wd > yf &&
                                                     xf + lf > xd &&
                                                     yf + wf > yd
                                        -1        -> xd > xf &&
                                                     yd + wd > yf &&
                                                     xf + lf > xd + ld &&
                                                     yf + wf > yd
                                        otherwise -> False