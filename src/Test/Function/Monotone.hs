module Test.Function.Monotone where

import Test.Util

monotone :: Ord r => (r -> r) -> r -> r -> Bool
monotone = monotone_on (<=) (<=)

-- | \( \forall a, b: a \leq b \Rightarrow f(a) \leq f(b) \)
--
monotone_on :: Rel r -> Rel s -> (r -> s) -> r -> r -> Bool
monotone_on (#) (%) f a b = a # b ==> f a % f b

antitone :: Ord r => (r -> r) -> r -> r -> Bool
antitone = antitone_on (<=) (<=)

-- | \( \forall a, b: a \leq b \Rightarrow f(b) \leq f(a) \)
--
antitone_on :: Rel r -> Rel s -> (r -> s) -> r -> r -> Bool
antitone_on (#) (%) f a b = a # b ==> f b % f a

