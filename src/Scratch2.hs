{-|
Copyright   : (c) Dave Laing, 2017
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scratch2 where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, KnownNat)

import Control.Monad.Trans (MonadIO, liftIO)

import Control.Concurrent.STM
import qualified Control.Concurrent.STM.Map as SM

import Data.Hashable (Hashable(..))

import Network.Socket (SockAddr)

import Servant.API
import Servant.API.ContentTypes (AllCTRender(..), AllCTUnrender(..))
import Servant.Server (ServantErr, Server)

import Reflex

import GHC.Generics
import Data.Aeson

class Embed tag api where
  type PayloadIn api
  type PayloadOut api -- do we need this?
  type Queues tag api

  mkQueue :: MonadIO m
          => Proxy tag
          -> Proxy api
          -> m (Queues tag api)

  {-
  serve :: Proxy api
        -> tag
        -> Queues tag api
        -> ((tag, PayloadIn api) -> IO ())
        -> Server api
  serve :: takes a tag and a Queues and a firing function
           and produces a ServerT m api?
  -- builds up a value of type PayloadIn
  -- fire of the event
  -- wait for a result based on tag
  -- return the result
  -}

class Embed tag api => EmbedEvents t tag api where
  type EventsInTuple t tag api
  type EventsInTuple t tag api = EventsInTuple' t tag () api
  type EventsIn t tag api
  type EventsIn t tag api = EventsIn' t tag () api

  type PairIn' t tag h api
  type FireIn' t tag h api

  type EventsInTuple' t tag h api
  type EventsIn' t tag h api
  type EventsOut t tag api

  mkPair :: ( Reflex t
            , Monad m
            , TriggerEvent t m
            )
         => Proxy tag
         -> Proxy h
         -> Proxy api
         -> m (PairIn' t tag h api)

  splitPair :: Proxy t
            -> Proxy tag
            -> Proxy h
            -> Proxy api
            -> PairIn' t tag h api
            -> (FireIn' t tag h api, EventsIn' t tag h api)

  mkOut :: Reflex t
        => Proxy api
        -> Event t (tag, PayloadOut api)
        -> EventsOut t tag api

  addToQueue :: ( Reflex t
                , Monad m
                , PerformEvent t m
                , MonadIO (Performable m)
                , Eq tag
                , Hashable tag
                )
             => Proxy tag
             -> Proxy api
             -> EventsOut t tag api
             -> Queues tag api
             -> m ()


instance (Embed tag a, Embed tag b) =>
  Embed tag (a :<|> b) where

  type PayloadIn (a :<|> b) =
    Either (PayloadIn a) (PayloadIn b)

  type PayloadOut (a :<|> b) =
    Either (PayloadOut a) (PayloadOut b)

  type Queues tag (a :<|> b) =
    Queues tag a :<|> Queues tag b

  mkQueue pt _ =
    (:<|>) <$>
      mkQueue pt (Proxy :: Proxy a) <*>
      mkQueue pt (Proxy :: Proxy b)

instance (EmbedEvents t tag a, EmbedEvents t tag b) =>
  EmbedEvents t tag (a :<|> b) where

  type PairIn' t tag h (a :<|> b) =
    PairIn' t tag h a :<|>
    PairIn' t tag h b

  type FireIn' t tag h (a :<|> b) =
    FireIn' t tag h a :<|>
    FireIn' t tag h b

  type EventsInTuple' t tag h (a :<|> b) =
    EventsInTuple' t tag h a :<|>
    EventsInTuple' t tag h b

  type EventsIn' t tag h (a :<|> b) =
    EventsIn' t tag h a :<|>
    EventsIn' t tag h b

  type EventsOut t tag (a :<|> b) =
    EventsOut t tag a :<|> EventsOut t tag b

  splitPair pT pTag pH _ (pA :<|> pB) =
    let
      (fA, eA) = splitPair pT pTag pH (Proxy :: Proxy a) pA
      (fB, eB) = splitPair pT pTag pH (Proxy :: Proxy b) pB
    in
      (fA :<|> fB, eA :<|> eB)

  mkOut _ ee =
    let
      l (Left e)  = Just e
      l (Right _) = Nothing

      r (Right e) = Just e
      r (Left _)  = Nothing

      wrap (t, Just x)  = Just (t, x)
      wrap (_, Nothing) = Nothing
    in
      mkOut (Proxy :: Proxy a) (fmapMaybe (wrap . fmap l) ee) :<|>
      mkOut (Proxy :: Proxy b) (fmapMaybe (wrap . fmap r) ee)

  addToQueue pt _ (eoA :<|> eoB) (qA :<|> qB) = do
    addToQueue pt (Proxy :: Proxy a) eoA qA
    addToQueue pt (Proxy :: Proxy b) eoB qB

instance (KnownSymbol capture, FromHttpApiData a, Embed tag api) =>
  Embed tag (Capture capture a :> api)  where

  type PayloadIn (Capture capture a :> api) =
    (a, PayloadIn api)

  type PayloadOut (Capture capture a :> api) =
    PayloadOut api

  type Queues tag (Capture capture a :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (KnownSymbol capture, FromHttpApiData a, EmbedEvents t tag api) =>
  EmbedEvents t tag (Capture capture a :> api)  where

  type EventsInTuple' t tag h (Capture capture a :> api) =
    EventsInTuple' t tag (a, h) api

  type PairIn' t tag h (Capture capture a :> api) =
    PairIn' t tag (a, h) api

  type FireIn' t tag h (Capture capture a :> api) =
    FireIn' t tag (a, h) api

  type EventsIn' t tag h (Capture capture a :> api) =
    EventsIn' t tag (a, h) api

  type EventsOut t tag (Capture capture a :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (a, h)) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol capture, FromHttpApiData a, Embed tag api) =>
  Embed tag (CaptureAll capture a :> api) where

  type PayloadIn (CaptureAll capture a :> api) =
    ([a], PayloadIn api)

  type PayloadOut (CaptureAll capture a :> api) =
    PayloadOut api

  type Queues tag (CaptureAll capture a :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (KnownSymbol capture, FromHttpApiData a, EmbedEvents t tag api) =>
  EmbedEvents t tag (CaptureAll capture a :> api) where

  type EventsInTuple' t tag h (CaptureAll capture a :> api) =
    EventsInTuple' t tag ([a], h) api

  type PairIn' t tag h (CaptureAll capture a :> api) =
    PairIn' t tag ([a], h) api

  type FireIn' t tag h (CaptureAll capture a :> api) =
    FireIn' t tag ([a], h) api

  type EventsIn' t tag h (CaptureAll capture a :> api) =
    EventsIn' t tag ([a], h) api

  type EventsOut t tag (CaptureAll capture a :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy ([a], h)) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol sym, FromHttpApiData a, Embed tag api) =>
  Embed tag (Header sym a :> api) where

  type PayloadIn (Header sym a :> api) =
    (Maybe a, PayloadIn api)

  type PayloadOut (Header sym a :> api) =
    PayloadOut api

  type Queues tag (Header sym a :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (KnownSymbol sym, FromHttpApiData a, EmbedEvents t tag api) =>
  EmbedEvents t tag (Header sym a :> api) where

  type EventsInTuple' t tag h (Header sym a :> api) =
    EventsInTuple' t tag ((Maybe a), h) api

  type PairIn' t tag h (Header sym a :> api) =
    PairIn' t tag ((Maybe a), h) api

  type FireIn' t tag h (Header sym a :> api) =
    FireIn' t tag ((Maybe a), h) api

  type EventsIn' t tag h (Header sym a :> api) =
    EventsIn' t tag ((Maybe a), h) api

  type EventsOut t tag (Header sym a :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (Maybe a, h)) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol sym, FromHttpApiData a, Embed tag api) =>
  Embed tag (QueryParam sym a :> api) where

  type PayloadIn (QueryParam sym a :> api) =
    (Maybe a, PayloadIn api)

  type PayloadOut (QueryParam sym a :> api) =
    PayloadOut api

  type Queues tag (QueryParam sym a :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (KnownSymbol sym, FromHttpApiData a, EmbedEvents t tag api) =>
  EmbedEvents t tag (QueryParam sym a :> api) where

  type EventsInTuple' t tag h (QueryParam sym a :> api) =
    EventsInTuple' t tag ((Maybe a), h) api

  type PairIn' t tag h (QueryParam sym a :> api) =
    PairIn' t tag ((Maybe a), h) api

  type FireIn' t tag h (QueryParam sym a :> api) =
    FireIn' t tag ((Maybe a), h) api

  type EventsIn' t tag h (QueryParam sym a :> api) =
    EventsIn' t tag ((Maybe a), h) api

  type EventsOut t tag (QueryParam sym a :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (Maybe a, h)) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol sym, FromHttpApiData a, Embed tag api) =>
  Embed tag (QueryParams sym a :> api) where

  type PayloadIn (QueryParams sym a :> api) =
    ([a], PayloadIn api)

  type PayloadOut (QueryParams sym a :> api) =
    PayloadOut api

  type Queues tag (QueryParams sym a :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (KnownSymbol sym, FromHttpApiData a, EmbedEvents t tag api) =>
  EmbedEvents t tag (QueryParams sym a :> api) where

  type EventsInTuple' t tag h (QueryParams sym a :> api) =
    EventsInTuple' t tag ([a], h) api

  type PairIn' t tag h (QueryParams sym a :> api) =
    PairIn' t tag ([a], h) api

  type FireIn' t tag h (QueryParams sym a :> api) =
    FireIn' t tag ([a], h) api

  type EventsIn' t tag h (QueryParams sym a :> api) =
    EventsIn' t tag ([a], h) api

  type EventsOut t tag (QueryParams sym a :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy ([a], h)) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol sym, Embed tag api) =>
  Embed tag (QueryFlag sym :> api) where

  type PayloadIn (QueryFlag sym :> api) =
    (Bool, PayloadIn api)

  type PayloadOut (QueryFlag sym :> api) =
    PayloadOut api

  type Queues tag (QueryFlag sym :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (KnownSymbol sym, EmbedEvents t tag api) =>
  EmbedEvents t tag (QueryFlag sym :> api) where

  type EventsInTuple' t tag h (QueryFlag sym :> api) =
    EventsInTuple' t tag (Bool, h) api

  type PairIn' t tag h (QueryFlag sym :> api) =
    PairIn' t tag (Bool, h) api

  type FireIn' t tag h (QueryFlag sym :> api) =
    FireIn' t tag (Bool, h) api

  type EventsIn' t tag h (QueryFlag sym :> api) =
    EventsIn' t tag (Bool, h) api

  type EventsOut t tag (QueryFlag sym :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (Bool, h)) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (AllCTUnrender list a, Embed tag api) =>
  Embed tag (ReqBody list a :> api) where

  type PayloadIn (ReqBody list a :> api) =
    (a, PayloadIn api)

  type PayloadOut (ReqBody list a :> api) =
    PayloadOut api

  type Queues tag (ReqBody list a :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (AllCTUnrender list a, EmbedEvents t tag api) =>
  EmbedEvents t tag (ReqBody list a :> api) where

  type EventsInTuple' t tag h (ReqBody list a :> api) =
    EventsInTuple' t tag (a, h) api

  type PairIn' t tag h (ReqBody list a :> api) =
    PairIn' t tag (a, h) api

  type FireIn' t tag h (ReqBody list a :> api) =
    FireIn' t tag (a, h) api

  type EventsIn' t tag h (ReqBody list a :> api) =
    EventsIn' t tag (a, h) api

  type EventsOut t tag (ReqBody list a :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (a, h)) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol path, Embed tag api) =>
  Embed tag (path :> api) where

  type PayloadIn (path :> api) =
    PayloadIn api

  type PayloadOut (path :> api) =
    PayloadOut api

  type Queues tag (path :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (KnownSymbol path, EmbedEvents t tag api) =>
  EmbedEvents t tag (path :> api) where

  type EventsInTuple' t tag h (path :> api) =
    EventsInTuple' t tag h api

  type PairIn' t tag h (path :> api) =
    PairIn' t tag h api

  type FireIn' t tag h (path :> api) =
    FireIn' t tag h api

  type EventsIn' t tag h (path :> api) =
    EventsIn' t tag h api

  type EventsOut t tag (path :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy h) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (Embed tag api) =>
  Embed tag (IsSecure :> api) where

  type PayloadIn (IsSecure :> api) =
    (IsSecure, PayloadIn api)

  type PayloadOut (IsSecure :> api) =
    PayloadOut api

  type Queues tag (IsSecure :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (EmbedEvents t tag api) =>
  EmbedEvents t tag (IsSecure :> api) where

  type EventsInTuple' t tag h (IsSecure :> api) =
    EventsInTuple' t tag (IsSecure, h) api

  type PairIn' t tag h (IsSecure :> api) =
    PairIn' t tag (IsSecure, h) api

  type FireIn' t tag h (IsSecure :> api) =
    FireIn' t tag (IsSecure, h) api

  type EventsIn' t tag h (IsSecure :> api) =
    EventsIn' t tag (IsSecure, h) api

  type EventsOut t tag (IsSecure :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (IsSecure, h)) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (Embed tag api) =>
  Embed tag (HttpVersion :> api) where

  type PayloadIn (HttpVersion :> api) =
    (HttpVersion, PayloadIn api)

  type PayloadOut (HttpVersion :> api) =
    PayloadOut api

  type Queues tag (HttpVersion :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (EmbedEvents t tag api) =>
  EmbedEvents t tag (HttpVersion :> api) where

  type EventsInTuple' t tag h (HttpVersion :> api) =
    EventsInTuple' t tag (HttpVersion, h) api

  type PairIn' t tag h (HttpVersion :> api) =
    PairIn' t tag (HttpVersion, h) api

  type FireIn' t tag h (HttpVersion :> api) =
    FireIn' t tag (HttpVersion, h) api

  type EventsIn' t tag h (HttpVersion :> api) =
    EventsIn' t tag (HttpVersion, h) api

  type EventsOut t tag (HttpVersion :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (HttpVersion, h)) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (Embed tag api) =>
  Embed tag (RemoteHost :> api) where

  type PayloadIn (RemoteHost :> api) =
    (SockAddr, PayloadIn api)

  type PayloadOut (RemoteHost :> api) =
    PayloadOut api

  type Queues tag (RemoteHost :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (EmbedEvents t tag api) =>
  EmbedEvents t tag (RemoteHost :> api) where

  type EventsInTuple' t tag h (RemoteHost :> api) =
    EventsInTuple' t tag (SockAddr, h) api

  type PairIn' t tag h (RemoteHost :> api) =
    PairIn' t tag (SockAddr, h) api

  type FireIn' t tag h (RemoteHost :> api) =
    FireIn' t tag (SockAddr, h) api

  type EventsIn' t tag h (RemoteHost :> api) =
    EventsIn' t tag (SockAddr, h) api

  type EventsOut t tag (RemoteHost :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (SockAddr, h)) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (Embed tag api) =>
  Embed tag (Vault :> api) where

  type PayloadIn (Vault :> api) =
    (Vault, PayloadIn api)

  type PayloadOut (Vault :> api) =
    PayloadOut api

  type Queues tag (Vault :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (EmbedEvents t tag api) =>
  EmbedEvents t tag (Vault :> api) where

  type EventsInTuple' t tag h (Vault :> api) =
    EventsInTuple' t tag (Vault, h) api

  type PairIn' t tag h (Vault :> api) =
    PairIn' t tag (Vault, h) api

  type FireIn' t tag h (Vault :> api) =
    FireIn' t tag (Vault, h) api

  type EventsIn' t tag h (Vault :> api) =
    EventsIn' t tag (Vault, h) api

  type EventsOut t tag (Vault :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (Vault, h)) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol realm, Embed tag api) =>
  Embed tag (BasicAuth realm usr :> api) where

  type PayloadIn (BasicAuth realm usr :> api) =
    (usr, PayloadIn api)

  type PayloadOut (BasicAuth realm usr :> api) =
    PayloadOut api

  type Queues tag (BasicAuth realm usr :> api) =
    Queues tag api

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

instance (KnownSymbol realm, EmbedEvents t tag api) =>
  EmbedEvents t tag (BasicAuth realm usr :> api) where

  type EventsInTuple' t tag h (BasicAuth realm usr :> api) =
    EventsInTuple' t tag (usr, h) api

  type PairIn' t tag h (BasicAuth realm usr :> api) =
    PairIn' t tag (usr, h) api

  type FireIn' t tag h (BasicAuth realm usr :> api) =
    FireIn' t tag (usr, h) api

  type EventsIn' t tag h (BasicAuth realm usr :> api) =
    EventsIn' t tag (usr, h) api

  type EventsOut t tag (BasicAuth realm usr :> api) =
    EventsOut t tag api

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (usr, h)) (Proxy :: Proxy api)

  mkOut _ =
    mkOut (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (AllCTRender ctypes a, ReflectMethod method, KnownNat status) =>
  Embed tag (Verb (method :: k1) status ctypes a) where

  type PayloadIn (Verb method status ctypes a) =
    ()

  type PayloadOut (Verb method status ctypes a) =
    Either ServantErr a

  type Queues tag (Verb method status ctypes a) =
    SM.Map tag (Either ServantErr a)

  mkQueue _ _ =
    liftIO . atomically $ SM.empty

instance (AllCTRender ctypes a, ReflectMethod method, KnownNat status) =>
  EmbedEvents t tag (Verb (method :: k1) status ctypes a) where

  type EventsInTuple' t tag h (Verb method status ctypes a) =
    Event t (tag, RevTupleList h)

  type PairIn' t tag h (Verb method status ctypes a) =
    ((tag, RevTupleListFlatten h) -> IO (), Event t (tag, RevTupleListFlatten h))

  type FireIn' t tag h (Verb method status ctypes a) =
    (tag, RevTupleListFlatten h) -> IO ()

  type EventsIn' t tag h (Verb method status ctypes a) =
    Event t (tag, RevTupleListFlatten h)

  type EventsOut t tag (Verb method status ctypes a) =
    Event t (tag, Either ServantErr a)

  splitPair _ _ _ _ =
    id

  mkOut _ =
    id

  addToQueue _ _ eo q =
    let
      f (k, v) = liftIO . atomically $ SM.insert k v q
    in
      performEvent_ $ f <$> eo

class TupleList xs where
  type TupleListFlatten xs
  type RevTupleListFlatten xs
  type RevTupleList xs

  tupleListFlatten :: xs -> TupleListFlatten xs
  revTupleListFlatten :: xs -> RevTupleListFlatten xs
  revTupleList :: xs -> RevTupleList xs

instance TupleList () where
  type TupleListFlatten () =
    ()
  type RevTupleListFlatten () =
    ()
  type RevTupleList () =
    ()

  tupleListFlatten _ =
    ()
  revTupleListFlatten _ =
    ()
  revTupleList _ =
    ()

instance TupleList (a, ()) where
  type TupleListFlatten (a, ()) =
    a
  type RevTupleListFlatten (a, ()) =
    a
  type RevTupleList (a, ()) =
    (a, ())

  tupleListFlatten (a, ()) =
    a
  revTupleListFlatten (a, ()) =
    a
  revTupleList (a, ()) =
    (a, ())

instance TupleList (a, (b, ())) where
  type TupleListFlatten (a, (b, ())) =
    (a, b)
  type RevTupleListFlatten (a, (b, ())) =
    (b, a)
  type RevTupleList (a, (b, ())) =
    (b, (a, ()))

  tupleListFlatten (a, (b, ())) =
    (a, b)
  revTupleListFlatten (a, (b, ())) =
    (b, a)
  revTupleList (a, (b, ())) =
    (b, (a, ()))

instance TupleList (a, (b, (c, ()))) where
  type TupleListFlatten (a, (b, (c, ()))) =
    (a, b, c)
  type RevTupleListFlatten (a, (b, (c, ()))) =
    (c, b, a)
  type RevTupleList (a, (b, (c, ()))) =
    (c, (b, (a, ())))

  tupleListFlatten (a, (b, (c, ()))) =
    (a, b, c)
  revTupleListFlatten (a, (b, (c, ()))) =
    (c, b, a)
  revTupleList (a, (b, (c, ()))) =
    (c, (b, (a, ())))

data Payload = Payload { value :: String }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Payload
instance ToJSON Payload

type MyAPI =
       "payloads" :> Get '[JSON] [Payload]
  :<|> "payloads" :> ReqBody '[JSON] Payload :> PostNoContent '[JSON] NoContent
  :<|> "payloads" :> Capture "index" Int :> DeleteNoContent '[JSON] NoContent

type TestAPI = "one" :> Capture "i1" Int :> TestAPI2
          :<|> "two" :> Capture "b1" Bool :> TestAPI2

type TestAPI2 = "three" :> Capture "i2" Int :> Get '[JSON] Int
           :<|> "four" :> Capture "b2" Bool :> Get '[JSON] Bool
