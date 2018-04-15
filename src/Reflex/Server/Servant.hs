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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Reflex.Server.Servant (
    ServantReflexServer(..)
  , ServantReflexServerEvents(..)
  , serverGuest
  ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, KnownNat)
import GHC.Exts (Constraint)

import Control.Monad (void)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Except (throwError)

import Control.Concurrent.STM
import qualified Control.Concurrent.STM.Map as SM

import Data.Hashable (Hashable(..))
import Data.Tagged (Tagged(..))

import Data.Map (Map)
import qualified Data.Map as Map

import Network.Socket (SockAddr)

import Servant.API
import Servant.API.ContentTypes (AllCTRender(..), AllCTUnrender(..))
import Servant.Server (ServantErr, Server, HasServer, serve)
import Network.Wai (Application, Request, Response)

import Reflex hiding (Request, Response)
import Reflex.Host.Basic

import Util.Tuples

serverGuest ::
  forall t tag api m.
  ( Reflex t
  , MonadIO m
  , ServantReflexServerEvents t tag api
  , Eq tag
  , Hashable tag
  , TupleListConstraints () api
  , HasServer api '[]
  , BasicGuestConstraints t m
  ) =>
  Proxy api ->
  IO tag ->
  (EventsIn' t tag () api -> BasicGuest t m (EventsOut t tag api)) ->
  BasicGuest t m (Application)
serverGuest pApi mkT network = do
  let
    pT = Proxy :: Proxy t
    pTag = Proxy :: Proxy tag
    pH = Proxy :: Proxy ()
  q <- liftIO $ mkQueue pTag pApi
  p :: PairIn' t tag () api <- mkPair pTag pH pApi
  let (ei, f) = splitPair pT pTag pH pApi p
  eo <- network ei
  addToQueue pTag pApi eo q
  pure . serve pApi $ serve' pApi mkT () f q

class ServantReflexServer tag api where
  type FireIn' tag h api
  type Queues tag api
  type TupleListConstraints h api :: Constraint

  mkQueue :: MonadIO m
          => Proxy tag
          -> Proxy api
          -> m (Queues tag api)

  serve' :: ( Eq tag
            , Hashable tag
            , TupleListConstraints h api
            )
         => Proxy api
         -> IO tag
         -> h
         -> FireIn' tag h api
         -> Queues tag api
         -> Server api

class ServantReflexServer tag api => ServantReflexServerEvents t tag api where
  type PairIn' t tag h api
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
            -> (EventsIn' t tag h api, FireIn' tag h api)

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


instance (ServantReflexServer tag a, ServantReflexServer tag b) =>
  ServantReflexServer tag (a :<|> b) where

  type FireIn' tag h (a :<|> b) =
    FireIn' tag h a :<|>
    FireIn' tag h b

  type Queues tag (a :<|> b) =
    Queues tag a :<|> Queues tag b

  type TupleListConstraints h (a :<|> b) =
    (TupleListConstraints h a, TupleListConstraints h b)

  mkQueue pt _ =
    (:<|>) <$>
      mkQueue pt (Proxy :: Proxy a) <*>
      mkQueue pt (Proxy :: Proxy b)

  serve' _ t h (fA :<|> fB) (qA :<|> qB) =
    serve' (Proxy :: Proxy a) t h fA qA :<|>
    serve' (Proxy :: Proxy b) t h fB qB

instance (ServantReflexServerEvents t tag a, ServantReflexServerEvents t tag b) =>
  ServantReflexServerEvents t tag (a :<|> b) where

  type PairIn' t tag h (a :<|> b) =
    PairIn' t tag h a :<|>
    PairIn' t tag h b

  type EventsIn' t tag h (a :<|> b) =
    EventsIn' t tag h a :<|>
    EventsIn' t tag h b

  type EventsOut t tag (a :<|> b) =
    EventsOut t tag a :<|> EventsOut t tag b

  mkPair pTag pH _ =
    (:<|>) <$>
      mkPair pTag pH (Proxy :: Proxy a) <*>
      mkPair pTag pH (Proxy :: Proxy b)

  splitPair pT pTag pH _ (pA :<|> pB) =
    let
      (fA, eA) = splitPair pT pTag pH (Proxy :: Proxy a) pA
      (fB, eB) = splitPair pT pTag pH (Proxy :: Proxy b) pB
    in
      (fA :<|> fB, eA :<|> eB)

  addToQueue pt _ (eoA :<|> eoB) (qA :<|> qB) = do
    addToQueue pt (Proxy :: Proxy a) eoA qA
    addToQueue pt (Proxy :: Proxy b) eoB qB

instance (KnownSymbol capture, FromHttpApiData a, ServantReflexServer tag api) =>
  ServantReflexServer tag (Capture capture a :> api)  where

  type FireIn' tag h (Capture capture a :> api) =
    FireIn' tag (a, h) api

  type Queues tag (Capture capture a :> api) =
    Queues tag api

  type TupleListConstraints h (Capture capture a :> api) =
    (TupleListConstraints (a, h) api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _ t h f q a =
    serve' (Proxy :: Proxy api) t (a, h) f q

instance (KnownSymbol capture, FromHttpApiData a, ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (Capture capture a :> api)  where

  type PairIn' t tag h (Capture capture a :> api) =
    PairIn' t tag (a, h) api

  type EventsIn' t tag h (Capture capture a :> api) =
    EventsIn' t tag (a, h) api

  type EventsOut t tag (Capture capture a :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy (a, h)) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (a, h)) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol capture, FromHttpApiData a, ServantReflexServer tag api) =>
  ServantReflexServer tag (CaptureAll capture a :> api) where

  type FireIn' tag h (CaptureAll capture a :> api) =
    FireIn' tag ([a], h) api

  type Queues tag (CaptureAll capture a :> api) =
    Queues tag api

  type TupleListConstraints h (CaptureAll capture a :> api) =
    (TupleListConstraints ([a], h) api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _ t h f q a =
    serve' (Proxy :: Proxy api) t (a, h) f q

instance (KnownSymbol capture, FromHttpApiData a, ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (CaptureAll capture a :> api) where

  type PairIn' t tag h (CaptureAll capture a :> api) =
    PairIn' t tag ([a], h) api

  type EventsIn' t tag h (CaptureAll capture a :> api) =
    EventsIn' t tag ([a], h) api

  type EventsOut t tag (CaptureAll capture a :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy ([a], h)) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy ([a], h)) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol sym, FromHttpApiData a, ServantReflexServer tag api) =>
  ServantReflexServer tag (Header sym a :> api) where

  type FireIn' tag h (Header sym a :> api) =
    FireIn' tag (Maybe a, h) api

  type Queues tag (Header sym a :> api) =
    Queues tag api

  type TupleListConstraints h (Header sym a :> api) =
    (TupleListConstraints (Maybe a, h) api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _ t h f q a =
    serve' (Proxy :: Proxy api) t (a, h) f q

instance (KnownSymbol sym, FromHttpApiData a, ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (Header sym a :> api) where

  type PairIn' t tag h (Header sym a :> api) =
    PairIn' t tag (Maybe a, h) api

  type EventsIn' t tag h (Header sym a :> api) =
    EventsIn' t tag (Maybe a, h) api

  type EventsOut t tag (Header sym a :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy (Maybe a, h)) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (Maybe a, h)) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol sym, FromHttpApiData a, ServantReflexServer tag api) =>
  ServantReflexServer tag (QueryParam sym a :> api) where

  type FireIn' tag h (QueryParam sym a :> api) =
    FireIn' tag ((Maybe a), h) api

  type Queues tag (QueryParam sym a :> api) =
    Queues tag api

  type TupleListConstraints h (QueryParam sym a :> api) =
    (TupleListConstraints (Maybe a, h) api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _ t h f q a =
    serve' (Proxy :: Proxy api) t (a, h) f q

instance (KnownSymbol sym, FromHttpApiData a, ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (QueryParam sym a :> api) where

  type PairIn' t tag h (QueryParam sym a :> api) =
    PairIn' t tag ((Maybe a), h) api

  type EventsIn' t tag h (QueryParam sym a :> api) =
    EventsIn' t tag ((Maybe a), h) api

  type EventsOut t tag (QueryParam sym a :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy (Maybe a, h)) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (Maybe a, h)) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol sym, FromHttpApiData a, ServantReflexServer tag api) =>
  ServantReflexServer tag (QueryParams sym a :> api) where

  type FireIn' tag h (QueryParams sym a :> api) =
    FireIn' tag ([a], h) api

  type Queues tag (QueryParams sym a :> api) =
    Queues tag api

  type TupleListConstraints h (QueryParams sym a :> api) =
    (TupleListConstraints ([a], h) api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _ t h f q a =
    serve' (Proxy :: Proxy api) t (a, h) f q

instance (KnownSymbol sym, FromHttpApiData a, ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (QueryParams sym a :> api) where

  type PairIn' t tag h (QueryParams sym a :> api) =
    PairIn' t tag ([a], h) api

  type EventsIn' t tag h (QueryParams sym a :> api) =
    EventsIn' t tag ([a], h) api

  type EventsOut t tag (QueryParams sym a :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy ([a], h)) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy ([a], h)) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol sym, ServantReflexServer tag api) =>
  ServantReflexServer tag (QueryFlag sym :> api) where

  type FireIn' tag h (QueryFlag sym :> api) =
    FireIn' tag (Bool, h) api

  type Queues tag (QueryFlag sym :> api) =
    Queues tag api

  type TupleListConstraints h (QueryFlag sym :> api) =
    (TupleListConstraints (Bool, h) api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _ t h f q a =
    serve' (Proxy :: Proxy api) t (a, h) f q

instance (KnownSymbol sym, ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (QueryFlag sym :> api) where

  type PairIn' t tag h (QueryFlag sym :> api) =
    PairIn' t tag (Bool, h) api

  type EventsIn' t tag h (QueryFlag sym :> api) =
    EventsIn' t tag (Bool, h) api

  type EventsOut t tag (QueryFlag sym :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy (Bool, h)) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (Bool, h)) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (AllCTUnrender list a, ServantReflexServer tag api) =>
  ServantReflexServer tag (ReqBody list a :> api) where

  type FireIn' tag h (ReqBody list a :> api) =
    FireIn' tag (a, h) api

  type Queues tag (ReqBody list a :> api) =
    Queues tag api

  type TupleListConstraints h (ReqBody list a :> api) =
    (TupleListConstraints (a, h) api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _ t h f q a =
    serve' (Proxy :: Proxy api) t (a, h) f q

instance (AllCTUnrender list a, ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (ReqBody list a :> api) where

  type PairIn' t tag h (ReqBody list a :> api) =
    PairIn' t tag (a, h) api

  type EventsIn' t tag h (ReqBody list a :> api) =
    EventsIn' t tag (a, h) api

  type EventsOut t tag (ReqBody list a :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy (a, h)) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (a, h)) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol path, ServantReflexServer tag api) =>
  ServantReflexServer tag (path :> api) where

  type FireIn' tag h (path :> api) =
    FireIn' tag h api

  type Queues tag (path :> api) =
    Queues tag api

  type TupleListConstraints h (path :> api) =
    (TupleListConstraints h api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _  =
    serve' (Proxy :: Proxy api)

instance (KnownSymbol path, ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (path :> api) where

  type PairIn' t tag h (path :> api) =
    PairIn' t tag h api

  type EventsIn' t tag h (path :> api) =
    EventsIn' t tag h api

  type EventsOut t tag (path :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy h) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy h) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (ServantReflexServer tag api) =>
  ServantReflexServer tag (IsSecure :> api) where

  type FireIn' tag h (IsSecure :> api) =
    FireIn' tag (IsSecure, h) api

  type Queues tag (IsSecure :> api) =
    Queues tag api

  type TupleListConstraints h (IsSecure :> api) =
    (TupleListConstraints (IsSecure, h) api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _ t h f q a =
    serve' (Proxy :: Proxy api) t (a, h) f q

instance (ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (IsSecure :> api) where

  type PairIn' t tag h (IsSecure :> api) =
    PairIn' t tag (IsSecure, h) api

  type EventsIn' t tag h (IsSecure :> api) =
    EventsIn' t tag (IsSecure, h) api

  type EventsOut t tag (IsSecure :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy (IsSecure, h)) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (IsSecure, h)) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (ServantReflexServer tag api) =>
  ServantReflexServer tag (HttpVersion :> api) where

  type FireIn' tag h (HttpVersion :> api) =
    FireIn' tag (HttpVersion, h) api

  type Queues tag (HttpVersion :> api) =
    Queues tag api

  type TupleListConstraints h (HttpVersion :> api) =
    (TupleListConstraints (HttpVersion, h) api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _ t h f q a =
    serve' (Proxy :: Proxy api) t (a, h) f q

instance (ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (HttpVersion :> api) where

  type PairIn' t tag h (HttpVersion :> api) =
    PairIn' t tag (HttpVersion, h) api

  type EventsIn' t tag h (HttpVersion :> api) =
    EventsIn' t tag (HttpVersion, h) api

  type EventsOut t tag (HttpVersion :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy (HttpVersion, h)) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (HttpVersion, h)) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (ServantReflexServer tag api) =>
  ServantReflexServer tag (RemoteHost :> api) where

  type FireIn' tag h (RemoteHost :> api) =
    FireIn' tag (SockAddr, h) api

  type Queues tag (RemoteHost :> api) =
    Queues tag api

  type TupleListConstraints h (RemoteHost :> api) =
    (TupleListConstraints (SockAddr, h) api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _ t h f q a =
    serve' (Proxy :: Proxy api) t (a, h) f q

instance (ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (RemoteHost :> api) where

  type PairIn' t tag h (RemoteHost :> api) =
    PairIn' t tag (SockAddr, h) api

  type EventsIn' t tag h (RemoteHost :> api) =
    EventsIn' t tag (SockAddr, h) api

  type EventsOut t tag (RemoteHost :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy (SockAddr, h)) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (SockAddr, h)) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (ServantReflexServer tag api) =>
  ServantReflexServer tag (Vault :> api) where

  type FireIn' tag h (Vault :> api) =
    FireIn' tag (Vault, h) api

  type Queues tag (Vault :> api) =
    Queues tag api

  type TupleListConstraints h (Vault :> api) =
    (TupleListConstraints (Vault, h) api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _ t h f q a =
    serve' (Proxy :: Proxy api) t (a, h) f q

instance (ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (Vault :> api) where

  type PairIn' t tag h (Vault :> api) =
    PairIn' t tag (Vault, h) api

  type EventsIn' t tag h (Vault :> api) =
    EventsIn' t tag (Vault, h) api

  type EventsOut t tag (Vault :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy (Vault, h)) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (Vault, h)) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (KnownSymbol realm, ServantReflexServer tag api) =>
  ServantReflexServer tag (BasicAuth realm usr :> api) where

  type FireIn' tag h (BasicAuth realm usr :> api) =
    FireIn' tag (usr, h) api

  type Queues tag (BasicAuth realm usr :> api) =
    Queues tag api

  type TupleListConstraints h (BasicAuth realm usr :> api) =
    (TupleListConstraints (usr, h) api)

  mkQueue pt _ =
    mkQueue pt (Proxy :: Proxy api)

  serve' _ t h f q a =
    serve' (Proxy :: Proxy api) t (a, h) f q

instance (KnownSymbol realm, ServantReflexServerEvents t tag api) =>
  ServantReflexServerEvents t tag (BasicAuth realm usr :> api) where

  type PairIn' t tag h (BasicAuth realm usr :> api) =
    PairIn' t tag (usr, h) api

  type EventsIn' t tag h (BasicAuth realm usr :> api) =
    EventsIn' t tag (usr, h) api

  type EventsOut t tag (BasicAuth realm usr :> api) =
    EventsOut t tag api

  mkPair pTag (_ :: Proxy h) _ =
    mkPair pTag (Proxy :: Proxy (usr, h)) (Proxy :: Proxy api)

  splitPair pT pTag (_ :: Proxy h) _ =
    splitPair pT pTag (Proxy :: Proxy (usr, h)) (Proxy :: Proxy api)

  addToQueue pt _ =
    addToQueue pt (Proxy :: Proxy api)

instance (AllCTRender ctypes a, ReflectMethod method, KnownNat status) =>
  ServantReflexServer tag (Verb (method :: k1) status ctypes a) where

  type FireIn' tag h (Verb method status ctypes a) =
    (tag, RevTupleListFlatten h) -> IO ()

  type Queues tag (Verb method status ctypes a) =
    SM.Map tag (Either ServantErr a)

  type TupleListConstraints h (Verb method status ctypes a) =
    TupleList h

  mkQueue _ _ =
    liftIO . atomically $ SM.empty

  serve' _ mkT h f q = do
    res <- liftIO $ do
      t <- mkT
      f (t, revTupleListFlatten h)
      atomically $ do
        v <- SM.lookup t q
        case v of
          Just x -> do
            SM.delete t q
            return x
          Nothing ->
            retry
    case res of
      Left e -> throwError e
      Right x -> pure x

instance (AllCTRender ctypes a, ReflectMethod method, KnownNat status) =>
  ServantReflexServerEvents t tag (Verb (method :: k1) status ctypes a) where

  type PairIn' t tag h (Verb method status ctypes a) =
    (Event t (tag, RevTupleListFlatten h), (tag, RevTupleListFlatten h) -> IO ())

  type EventsIn' t tag h (Verb method status ctypes a) =
    Event t (tag, RevTupleListFlatten h)

  type EventsOut t tag (Verb method status ctypes a) =
    Event t (Map tag (Either ServantErr a))

  mkPair _ _ _ = do
    newTriggerEvent

  splitPair _ _ _ _ =
    id

  addToQueue _ _ eo q =
    let
      f k v = liftIO . atomically $ SM.insert k v q
    in
      performEvent_ $ (void . Map.traverseWithKey f) <$> eo

instance ServantReflexServer tag Raw where
  type FireIn' tag h Raw =
    (tag, RevTupleListFlatten h, Request) -> IO ()

  type Queues tag Raw =
    SM.Map tag Response

  type TupleListConstraints h Raw =
    TupleList h

  mkQueue _ _ =
    liftIO . atomically $ SM.empty

  serve' _ mkT h f q = Tagged $ \req respond -> do
    res <- liftIO $ do
      t <- mkT
      f (t, revTupleListFlatten h, req)
      atomically $ do
        v <- SM.lookup t q
        case v of
          Just x -> do
            SM.delete t q
            return x
          Nothing ->
            retry
    respond res

instance ServantReflexServerEvents t tag Raw where

  type PairIn' t tag h Raw =
    (Event t (tag, RevTupleListFlatten h, Request), (tag, RevTupleListFlatten h, Request) -> IO ())

  type EventsIn' t tag h Raw =
    Event t (tag, RevTupleListFlatten h, Request)

  type EventsOut t tag Raw =
    Event t (Map tag Response)

  mkPair _ _ _ = do
    newTriggerEvent

  splitPair _ _ _ _ =
    id

  addToQueue _ _ eo q =
    let
      f k v = liftIO . atomically $ SM.insert k v q
    in
      performEvent_ $ (void . Map.traverseWithKey f) <$> eo
