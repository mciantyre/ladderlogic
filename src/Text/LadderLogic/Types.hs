module Text.LadderLogic.Types where

-- | Ladder logic... uh... logic!
data Logic
  = Input String          -- ^ an input
  | Output String         -- ^ an output
  | And Logic Logic       -- ^ ANDing of two logic blocks
  | Or Logic Logic        -- ^ ORing of two logic blocks
  | Not Logic             -- ^ NOTing a logic block
  deriving (Show)

instance Eq Logic where
  (Input x) == (Input y)      = x == y
  (Output x) == (Output y)    = x == y
  (Not x) == (Not y)          = x == y

  -- And may match to the flip of itself, since the operation is
  -- commutative
  (And r s) == (And x y) = 
    ((r == x) && (s == y)) || ((r == y) && (s == x))

  -- Or may match to the flip of itself, since the operation is
  -- commutative
  (Or r s) == (Or x y) =
    ((r == x) && (s == y)) || ((r == y) && (s == x))

  (==) _ _ = False
