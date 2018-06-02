module Main where

import Control.Monad.IO.Class
import Data.Map.Strict (Map)
import Data.Monoid
import Game.GoreAndAsh
import Game.GoreAndAsh.Time
import Logger.API
import System.Random

import qualified Data.Foldable as F
import qualified Data.Map.Strict as M

-- | Application monad that is used for implementation of game API
type AppMonad = LoggerT Spider GMSpider

-- | We index components by ints
type ComponentId = Int

-- | Amount of time
type Seconds = Double

-- | Single component that waits a specified time and emits an event about disire
-- to die.
component :: (MonadGame t m, LoggerMonad t m) => ComponentId -> Seconds -> m (Event t ComponentId)
component n seconds = do
  startE <- getPostBuild
  outputMessage $ ffor startE $ const $ "Component " ++ show n ++ " is born!"
  dt <- liftIO $ randomRIO (0.1, seconds)
  deathEvent <- tickEvery $ realToFrac dt
  outputMessage $ ffor deathEvent $ const $ "Component " ++ show n ++ " is tired!"
  return $ const n <$> deathEvent

-- The application should be generic in the host monad that is used
app :: forall t m . (MonadGame t m, LoggerMonad t m) => Seconds -> Seconds -> m ()
app bornTime tireTime = do
  -- generate new child every bornTime seconds
  bornTick <- tickEvery $ realToFrac bornTime
  rec
    -- recursive loop as components sends signal when the want to die
    cmps :: Dynamic t (Map ComponentId (Event t ComponentId)) <- listHoldWithKey mempty updE component
    let countDyn = length <$> cmps
        -- build map with those that wanted to die
        delEvent = switchPromptlyDyn $ ffor cmps $ collectDelEvents . M.elems
        -- read current number of components to assign an id to new component
        bornEvent = flip pushAlways bornTick $ const $ do
          n <- sample (current countDyn)
          return $ M.singleton n (Just tireTime)
        -- if deletion and born coincedence, merge their internal maps
        updE = delEvent <> bornEvent

  outputMessage $ ffor (updated countDyn) $ \n -> "Now have " ++ show n ++ " components!"

-- | Construct a event for deletion a component from dynamic collection
collectDelEvents :: Reflex t => [Event t ComponentId] -> Event t (Map ComponentId (Maybe Seconds))
collectDelEvents es = ffor (mergeList es) $ \is -> M.fromList [(i, Nothing) | i <- F.toList is]

main :: IO ()
main = runGM $ runLoggerT (app 2 5 :: AppMonad ())
