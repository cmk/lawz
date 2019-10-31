module Test.Property.Function.Idempotent where

import Data.List (unfoldr)
import Numeric.Natural (Natural(..))
import Test.Property.Util

-- | \( \forall a: g \circ f (a) = f (a) \)
--
projective :: Eq r => (r -> r) -> (r -> r) -> r -> Bool
projective = projective_on (==)

-- | \( \forall a: g \circ f (a) \sim f (a) \)
--
projective_on :: Rel s -> (r -> s) -> (s -> s) -> r -> Bool
projective_on (~~) f g r = g (f r) ~~ f r

-- | \( \forall a: f \circ f(a) = f(a) \)
--
idempotent :: Eq r => (r -> r) -> r -> Bool
idempotent f = idempotent_on (==) f

-- | \( \forall a: f \circ f(a) \sim f(a) \)
--
idempotent_on :: Rel r -> (r -> r) -> r -> Bool
idempotent_on (~~) f = projective_on (~~) f f

idempotent_k :: Eq r => Natural -> (r -> r) -> r -> Bool
idempotent_k k f r = k >= 1 ==> foldr (.) id fs r == f r
  where fs = (`unfoldr` k) $ \m -> if m==1 then Nothing else Just (f,m-1)
