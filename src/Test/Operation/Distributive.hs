module Test.Operation.Distributive where

import Test.Logic


-- | \( \forall a, b, c: (a \# b) \% c \equiv (a \% c) \# (b \% c) \)
--
distributive :: Eq r => (r -> r -> r) -> (r -> r -> r) -> (r -> r -> r -> Bool)
distributive = distributive_on (==)

distributive_on :: Rel r b -> (r -> r -> r) -> (r -> r -> r) -> (r -> r -> r -> b)
distributive_on (~~) (#) (%) a b c = ((a # b) % c) ~~ ((a % c) # (b % c))

-- | \( \forall a, b, c: c \% (a \# b) \equiv (c \% a) \# (c \% b) \)
--
distributive' :: Eq r => (r -> r -> r) -> (r -> r -> r) -> (r -> r -> r -> Bool)
distributive' = distributive_on' (==)

distributive_on' :: Rel r b -> (r -> r -> r) -> (r -> r -> r) -> (r -> r -> r -> b)
distributive_on' (~~) (#) (%) a b c = (c % (a # b)) ~~ ((c % a) # (c % b))
