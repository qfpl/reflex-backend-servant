{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scratch where

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)

import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownSymbol, KnownNat)

import Data.Hashable (Hashable(..))

import Control.Concurrent.STM
import qualified Control.Concurrent.STM.Map as SM

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Except (ExceptT(..), throwError)

import Network.Socket (SockAddr)

import Servant.API
import Servant.API.ContentTypes (AllCTRender(..), AllCTUnrender(..))
import Servant.Server (ServantErr, Handler(..), Server)

import Reflex

import Reflex.Basic.Host

-- for the sample API
import GHC.Generics
import Data.Aeson


{-

Probably want an inside and outside function as part of this class

Depends on if we want to serve requests in parallel and then process them in serial - in that case we need the separation

Maybe start with the serial version

-}

{-

outer:
we have the usual ticket dispenser
we use that to fire off events with a tag (ticket)
we use that to read from the stmmap
event generation happens outside of the serve function
  serve gets passed a ticket generating function
                    an event firing function
                    an stmmap to wait on

inner:
we do we what we do now, but
  we have a stmmap in context
  we transform the output to a handler and use it to write to
   the stmmap

one bit takes in
  tag -> IO ()
  Event t tag
  STMMap tag (Handler ())
and makes a server from that
  walk through the type until the return type
  fire the action
  use the event to read from the map
    - this implies we're in a network

would need to modify inner so that it wrote to the map on the way out


build a ticket generator / event / fire / map per choice
build up something that gathers these down at the leaves
union it together with serve and with inner to make use of it

need to work out how to have a separated out network that we can work with
-}

newtype Ticket = Ticket Int
  deriving (Eq, Ord, Show, Hashable)

data Endpoint t tag a =
  Endpoint {
    eMkTag :: TVar tag
  , eEvent :: Event t tag
  , eFire  :: tag -> IO ()
  , eMap   :: SM.Map tag (Either ServantErr a)
  }

-- mkEndpoint

getTicket :: Endpoint t Ticket a -> STM Ticket
getTicket (Endpoint tv _ _ _) = do
  Ticket t <- readTVar tv
  writeTVar tv $ Ticket (succ t)
  return $ Ticket t

class Embed t m tag api where
  type EmbedEndpoint t tag api
  type EmbedFn t m tag api

  mkEndpoint ::
    ( MonadIO m
    , TriggerEvent t m
    ) =>
    Proxy t ->
    Proxy tag ->
    Proxy api ->
    m (EmbedEndpoint t tag api)

  setupInner ::
    ( PostBuild t m
    , TriggerEvent t m
    , PerformEvent t m
    ) =>
    Proxy t ->
    Proxy m ->
    Proxy tag ->
    Proxy api ->
    EmbedEndpoint t tag api ->
    EmbedFn t m tag api ->
    Server api

instance (Embed t m tag a, Embed t m tag b)
    => Embed t m tag (a :<|> b) where
  type EmbedEndpoint t tag (a :<|> b) =
    EmbedEndpoint t tag a :<|> EmbedEndpoint t tag b
  type EmbedFn t m tag (a :<|> b) =
    EmbedFn t m tag a :<|> EmbedFn t m tag b

  mkEndpoint pT pTag _ = do
    eA <- mkEndpoint pT pTag (Proxy :: Proxy a)
    eB <- mkEndpoint pT pTag (Proxy :: Proxy b)
    pure (eA :<|> eB)

  setupInner pT pM pTag _ (eA :<|> eB) (iA :<|> iB) =
    setupInner pT pM pTag (Proxy :: Proxy a) eA iA :<|>
    setupInner pT pM pTag (Proxy :: Proxy b) eB iB

instance (KnownSymbol capture, FromHttpApiData a, Embed t m tag api)
    => Embed t m tag (Capture capture a :> api)  where
  type EmbedEndpoint t tag (Capture capture a :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (Capture capture a :> api) =
    a -> EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f x =
    setupInner pT pM pTag (Proxy :: Proxy api) e (f x)

instance (KnownSymbol capture, FromHttpApiData a, Embed t m tag api)
    => Embed t m tag (CaptureAll capture a :> api) where
  type EmbedEndpoint t tag (CaptureAll capture a :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (CaptureAll capture a :> api) =
    [a] -> EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f x =
    setupInner pT pM pTag (Proxy :: Proxy api) e (f x)

instance (KnownSymbol sym, FromHttpApiData a, Embed t m tag api)
    => Embed t m tag (Header sym a :> api) where
  type EmbedEndpoint t tag (Header sym a :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (Header sym a :> api) =
    Maybe a -> EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f x =
    setupInner pT pM pTag (Proxy :: Proxy api) e (f x)

instance (KnownSymbol sym, FromHttpApiData a, Embed t m tag api)
    => Embed t m tag (QueryParam sym a :> api) where
  type EmbedEndpoint t tag (QueryParam sym a :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (QueryParam sym a :> api) =
    Maybe a -> EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f x =
    setupInner pT pM pTag (Proxy :: Proxy api) e (f x)

instance (KnownSymbol sym, FromHttpApiData a, Embed t m tag api)
    => Embed t m tag (QueryParams sym a :> api) where
  type EmbedEndpoint t tag (QueryParams sym a :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (QueryParams sym a :> api) =
    [a] -> EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f x =
    setupInner pT pM pTag (Proxy :: Proxy api) e (f x)

instance (KnownSymbol sym, Embed t m tag api)
    => Embed t m tag (QueryFlag sym :> api) where
  type EmbedEndpoint t tag (QueryFlag sym :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (QueryFlag sym :> api) =
    Bool -> EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f x =
    setupInner pT pM pTag (Proxy :: Proxy api) e (f x)

instance (AllCTUnrender list a, Embed t m tag api)
    => Embed t m tag (ReqBody list a :> api) where
  type EmbedEndpoint t tag (ReqBody list a :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (ReqBody list a :> api) =
    a -> EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f x =
    setupInner pT pM pTag (Proxy :: Proxy api) e (f x)

instance (KnownSymbol path, Embed t m tag api)
    => Embed t m tag (path :> api) where
  type EmbedEndpoint t tag (path :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (path :> api) =
    EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f =
    setupInner pT pM pTag (Proxy :: Proxy api) e f

instance (Embed t m tag api)
    => Embed t m tag (IsSecure :> api) where
  type EmbedEndpoint t tag (IsSecure :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (IsSecure :> api) =
    IsSecure -> EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f x =
    setupInner pT pM pTag (Proxy :: Proxy api) e (f x)

instance (Embed t m tag api)
    => Embed t m tag (HttpVersion :> api) where
  type EmbedEndpoint t tag (HttpVersion :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (HttpVersion :> api) =
    HttpVersion -> EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f x =
    setupInner pT pM pTag (Proxy :: Proxy api) e (f x)

instance (Embed t m tag api)
    => Embed t m tag (RemoteHost :> api) where
  type EmbedEndpoint t tag (RemoteHost :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (RemoteHost :> api) =
    SockAddr -> EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f x =
    setupInner pT pM pTag (Proxy :: Proxy api) e (f x)

instance (Embed t m tag api)
    => Embed t m tag (Vault :> api) where
  type EmbedEndpoint t tag (Vault :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (Vault :> api) =
    Vault -> EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f x =
    setupInner pT pM pTag (Proxy :: Proxy api) e (f x)

instance (KnownSymbol realm, Embed t m tag api)
    => Embed t m tag (BasicAuth realm usr :> api) where
  type EmbedEndpoint t tag (BasicAuth realm usr :> api) =
    EmbedEndpoint t tag api
  type EmbedFn t m tag (BasicAuth realm usr :> api) =
    usr -> EmbedFn t m tag api

  mkEndpoint pT pTag _ =
    mkEndpoint pT pTag (Proxy :: Proxy api)

  setupInner pT pM pTag _ e f x =
    setupInner pT pM pTag (Proxy :: Proxy api) e (f x)

instance (AllCTRender ctypes a, ReflectMethod method, KnownNat status)
    => Embed t m Ticket (Verb (method :: k1) status ctypes a) where
  type EmbedEndpoint t Ticket (Verb method status ctypes a) =
    Endpoint t Ticket a
  type EmbedFn t m Ticket (Verb method status ctypes a) =
    Proxy t -> Proxy m -> Event t Ticket -> m (Event t (Ticket, Either ServantErr a))

  mkEndpoint pT _ _ = do
    t <- liftIO . atomically $ newTVar (Ticket 0)
    (e, f) <- newTriggerEvent
    m <- liftIO . atomically $ SM.empty
    pure $ Endpoint t e f m

  setupInner _ _ _ _ e f = do
    ticket <- liftIO . atomically $ getTicket e
    let
      m = eMap e
      guest :: forall t m. BasicGuest t m (Ticket -> IO ())
      guest = do
        (eTag, fTag) <- newTriggerEvent
        eRes <- f (Proxy :: Proxy t) (Proxy :: Proxy m) eTag
        performEvent_ $ (\(k, v) -> liftIO . atomically $ SM.insert k v m) <$> eRes
        pure fTag
    fire <- liftIO . basicHost $ guest

    liftIO $ fire ticket
    res <- liftIO . atomically $ do
      v <- SM.lookup ticket m
      case v of
        Just x -> do
          SM.delete ticket m
          return x
        Nothing ->
          retry

    case res of
      Left e -> throwError e
      Right x -> pure x

{-
-- TODO unwind the headers hlist into the output list
instance {-# OVERLAPPING #-} (AllCTRender ctypes a, ReflectMethod method, KnownNat status, GetHeaders (Headers h a))
      => Embed (Verb method status ctypes (Headers h a)) where
  type EmbedFn (Verb method status ctypes (Headers h a)) =
    Event t tag
-}

data Payload = Payload { value :: String }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Payload
instance ToJSON Payload

type MyAPI =
       "payloads" :> Get '[JSON] [Payload]
  :<|> "payloads" :> ReqBody '[JSON] Payload :> PostNoContent '[JSON] NoContent
  :<|> "payloads" :> Capture "index" Int :> DeleteNoContent '[JSON] NoContent

inner ::
  forall t m.
  ( Reflex t
  , MonadFix m
  , MonadHold t m
  , PerformEvent t m
  , MonadIO (Performable m)
  ) =>
  EmbedFn t m Ticket MyAPI
inner =
  let
    handleGet e = do
      eOut :<|> _ :<|> _ <- network e never never
      pure eOut

    handlePost p e = do
      _ :<|> eOut :<|> _ <- network never ((\t -> (t, p)) <$> e) never
      pure eOut

    handleDelete i e = do
      _ :<|> _ :<|> eOut <- network never never ((\t -> (t, i)) <$> e)
      pure eOut

    network ::
      ( MonadFix m
      , MonadHold t m
      , PerformEvent t m
      , MonadIO (Performable m)
      )=>
      Event t Ticket ->
      Event t (Ticket, Payload) ->
      Event t (Ticket, Int) ->
      m (Event t (Ticket, Either ServantErr [Payload]) :<|>
         Event t (Ticket, Either ServantErr NoContent) :<|>
         Event t (Ticket, Either ServantErr NoContent))
    network eGet ePost eDelete = do
      let
        remove i xs
          | i < 0 = xs
          | i >= length xs = xs
          | otherwise = let (ys, _ : zs) = splitAt i xs in ys ++ zs

      bList <- accum (flip ($)) [] . leftmost $ [
          ((:) . value . snd) <$> ePost
        , (remove . snd) <$> eDelete
        ]

      performEvent_ $ (liftIO . putStrLn $ "FRP: get") <$ eGet
      performEvent_ $ (liftIO . putStrLn $ "FRP: post") <$ ePost
      performEvent_ $ (liftIO . putStrLn $ "FRP: delete") <$ eDelete

      let
        fnGet = return . fmap Payload . reverse
        eGetOut = (\l t -> (t, fnGet l)) <$> bList <@> eGet

        ePostOut = (\(t, _) -> (t, return NoContent)) <$> ePost
        eDeleteOut = (\(t, _) -> (t, return NoContent)) <$> eDelete

      pure $
        eGetOut :<|>
        ePostOut :<|>
        eDeleteOut
  in
    handleGet :<|> handlePost :<|> handleDelete
