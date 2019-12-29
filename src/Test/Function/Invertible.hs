module Test.Function.Invertible where

import Test.Util


-- | \( \forall a: f a \# b \Leftrightarrow a \# g b \)
--
-- For example, a Galois connection is defined by @adjoint_on (<=)@.
--
adjoint_on :: Rel r -> Rel s -> (s -> r) -> (r -> s) -> (s -> r -> Bool)
adjoint_on (#) (%) f g a b = f a # b <==> a % g b

-- | \( \forall a: f (g a) \equiv a \)
--
invertible :: Eq r => (r -> s) -> (s -> r) -> (r -> Bool)
invertible = invertible_on (==)

-- | \( \forall a: f (g a) \doteq a \)
--
invertible_on :: Rel s -> (s -> r) -> (r -> s) -> (s -> Bool)
invertible_on (~~) f g a = g (f a) ~~ a
