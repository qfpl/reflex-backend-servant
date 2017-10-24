{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Util.Ticket (
    Ticket
  , newTicketDispenser
  , getNextTicket
  ) where

import Data.Hashable (Hashable(..))

import Control.Monad.STM
import Control.Concurrent.STM

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


