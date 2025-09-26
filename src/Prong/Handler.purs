module Prong.Handler where

import Prelude

import Control.Alternative (guard)
import Data.Maybe (Maybe(..))

newtype Handler ∷ (Type → Type) → Type → Type → Type
newtype Handler f a b = Handler (a → Maybe (f b))

instance Semigroup (Handler f a b) where
  append (Handler f) (Handler g) =
    Handler \a → case f a of
      Just b → Just b
      Nothing → g a

instance Monoid (Handler f a b) where
  mempty = Handler (const Nothing)

instance Functor f ⇒ Functor (Handler f a) where
  map f (Handler g) = Handler \a → map f <$> g a

type Predicate a = a → Boolean

withPredicate ∷ ∀ f a b. Predicate a → Handler f a b → Handler f a b
withPredicate pred (Handler f) = Handler \a → guard (pred a) *> f a

handler ∷ ∀ f a b. Predicate a → (a → f b) → Handler f a b
handler pred f = withPredicate pred (Handler (\a → Just (f a)))
