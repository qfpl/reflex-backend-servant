{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Example (
    runMyAPI
  ) where

import Data.Proxy (Proxy(..))

import Control.Monad.Trans (liftIO)

import Data.Hashable (Hashable(..))

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Concurrent.STM

import Servant.API

import Network.Wai.Handler.Warp (run)

import Reflex
import Reflex.Dom.Core (debounce)
import Reflex.Basic.Host

import Util.List

import ServantReflexServer

(=:) :: Ord k => k -> v -> Map k v
(=:) = Map.singleton

newtype Ticket = Ticket Int
  deriving (Eq, Ord, Show, Hashable)

newTicketDispenser :: IO (TVar Ticket)
newTicketDispenser =
  atomically . newTVar . Ticket $ 0

getNextTicket :: TVar Ticket -> STM Ticket
getNextTicket tv = do
  Ticket t <- readTVar tv
  writeTVar tv $ Ticket (succ t)
  return $ Ticket t

type MyAPI =
  "add" :> Capture "addend" Int :> Get '[JSON] NoContent :<|>
  "total" :> Get '[JSON] Int :<|>
  "total" :> "after" :> Capture "count" Int :> Get '[JSON] Int :<|>
  "total" :> "delay" :> Capture "seconds" Int :> Get '[JSON] Int

runMyAPI :: IO ()
runMyAPI = do
  td <- newTicketDispenser
  let mkT = atomically $ getNextTicket td
  app <- basicHost $ serverGuest (Proxy :: Proxy MyAPI) mkT myAPINetwork
  run 8080 app

myAPINetwork ::
  (Ord tag, Show tag, Reflex t) =>
  EventsIn' t tag () MyAPI ->
  BasicGuest t m (EventsOut t tag MyAPI)
myAPINetwork (eAddIn :<|> eTotalIn :<|> eTotalAfterIn :<|> eTotalDelayIn) = mdo

  -- We acknowledge the add requests as they come in

  let
    mkAddOut (t, _) =
      t =: Right NoContent
    eAddOut =
      mkAddOut <$> eAddIn

  performEvent_ $ (\x -> liftIO . putStrLn $ "FRP: add in " ++ show x) <$> eAddIn
  performEvent_ $ (\x -> liftIO . putStrLn $ "FRP: add out " ++ show x) <$> eAddOut

  -- The add requests are used to build up a total

  dTotal <- foldDyn ($) 0 $
    ((+) . snd) <$> eAddIn

  -- and we use that when requests for the total come in

  let
    mkTotalOut r (t, _) =
      t =: Right r
    eTotalOut =
      mkTotalOut <$> current dTotal <@> eTotalIn

  -- When a request for the total-after-n-adds comes in, we put it in a map

  dCountdown <- foldDyn ($) Map.empty . mergeWith (.) $ [
      uncurry Map.insert <$> eTotalAfterIn
      -- and decrease n when adds occur
    , Map.filter (> 0) . fmap pred <$ eAddIn
    ]

  let
    -- keeping an eye out for when the count hits zero
    mkTotalAfterOut total =
      fmap (const $ Right total) . Map.filter (== 0) . fmap pred
    eTotalAfterOut =
      ffilter (not . Map.null) $ mkTotalAfterOut <$> current dTotal <*> current dCountdown <@ eAddIn

  -- When a request for the total-after-n-seconds-of-inactivity comes in, we put it in a map
  dDelays <- foldDyn ($) Map.empty . mergeWith (.) $ [
      uncurry Map.insert <$> eTotalDelayIn
    , (\d -> Map.filterWithKey (\k _ -> Map.notMember k d)) <$> emDone
    ]

  (_, emDone) <- runEventWriterT . listWithKey dDelays $ \t dv -> do
    -- and debounce the add event with the given delay
    d <- sample . current $ dv
    e <- debounce (fromIntegral d) (t =: () <$ eAddIn)
    tellEvent e

  let
    mkTotalDelayOut r =
      fmap (const $ Right r)
    eTotalDelayOut =
      mkTotalDelayOut <$> current dTotal <@> emDone

  pure $ eAddOut :<|> eTotalOut :<|> eTotalAfterOut :<|> eTotalDelayOut