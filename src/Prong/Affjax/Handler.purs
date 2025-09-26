module Prong.Affjax.Handler where

import Prelude

import Affjax as Affjax
import Affjax.StatusCode as StatusCode
import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT)
import Data.Argonaut.Parser as JP
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Tuple (Tuple(..), uncurry)
import Effect.Aff.Class (class MonadAff, liftAff)
import Prong.Handler as Prong

type Exchange a = Tuple (Affjax.Request a) (Affjax.Response a)

type Handler err a = Prong.Handler (Either err) (Exchange a)

type ResponseFilter a = Prong.Predicate (Exchange a)

status ∷ ∀ a. Int → ResponseFilter a
status code (Tuple _ response) = response.status == StatusCode.StatusCode code

handler
  ∷ ∀ err a b
  . ResponseFilter a
  → (Affjax.Request a → Affjax.Response a → Either err b)
  → Handler err a b
handler filters f = Prong.handler filters (uncurry f)

decode
  ∷ ∀ err a
  . (String → err)
  → (CA.JsonDecodeError → err)
  → CA.JsonCodec a
  → Affjax.Response String
  → Either err a
decode parseError decodeError codec res =
  lmap decodeError
    <<< CA.decode codec
    =<< lmap parseError (JP.jsonParser res.body)

fail ∷ ∀ err a b. err → a → Either err b
fail err _ = Left err

ok ∷ ∀ err a. a → Either err Unit
ok _ = Right unit

handleExchange
  ∷ ∀ err a b
  . Affjax.Request a
  → Affjax.Response a
  → Handler err a b
  → Maybe (Either err b)
handleExchange req res (Prong.Handler f) = f (Tuple req res)

run
  ∷ ∀ m err a b
  . MonadAff m
  ⇒ (Affjax.Request a → Affjax.Error → err)
  → (Affjax.Request a → Affjax.Response a → err)
  → Affjax.AffjaxDriver
  → Affjax.Request a
  → Handler err a b
  → m (Either err b)
run reqErr unhandledErr driver req h = runExceptT do
  res ← ExceptT $ lmap (reqErr req) <$> liftAff (Affjax.request driver req)
  except $ fromMaybe (Left (unhandledErr req res)) $ handleExchange req res h
