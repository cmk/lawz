module Test.Operation.Associative where

import Test.Logic


-- | \( \forall a, b, c: (a \# b) \# c \equiv a \# (b \# c) \)
--
associative :: Eq r => (r -> r -> r) -> (r -> r -> r -> Bool)
associative = associative_on (==)


-- | \( \forall a, b, c: (a \# b) \# c \doteq a \# (b \# c) \)
--
associative_on :: Rel r b -> (r -> r -> r) -> (r -> r -> r -> b)
associative_on (~~) (#) a b c = ((a # b) # c) ~~ (a # (b # c)) 
