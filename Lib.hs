module Lib
  ( initState
  , initScript
  , runEval
  )
where

import           Data.Char                                ( ord
                                                          , chr
                                                          )

data Command =
  IncP | DecP | Inc | Dec | Out | In | JumpF | JumpB | Ignore | End
  deriving(Eq)

parseCmd :: Char -> Command
parseCmd '>' = IncP
parseCmd '<' = DecP
parseCmd '+' = Inc
parseCmd '-' = Dec
parseCmd '.' = Out
parseCmd ',' = In
parseCmd '[' = JumpF
parseCmd ']' = JumpB
parseCmd _   = Ignore -- Ignore all other characters

type Value = Char
data JumpDir = None | Forward | Backward

data Tape a = Tape [a] a [a]
  deriving (Show)
type Script = Tape Command
type Array = Tape Value
type State = (Array, JumpDir)

class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate = extend id

  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

instance Functor Tape where
  fmap f (Tape l c r) = Tape (f <$> l) (f c) (f <$> r)

instance Comonad Tape where
  extract (Tape _ c _) = c
  duplicate w@(Tape l _ r) = Tape (initTape <$> l) w (initTape <$> r)

initTape :: a -> Tape a
initTape x = Tape [] x []

inc :: Value -> Value
inc v = chr (ord v + 1)

inc' :: Tape Value -> Value
inc' (Tape _ c _) = chr (ord c + 1)

dec :: Value -> Value
dec v = chr (ord v - 1)

dec' :: Tape Value -> Value
dec' (Tape _ c _) = chr (ord c - 1)

mount :: [a] -> Tape a
mount l = Tape [] (head l) (tail l)

advance :: Tape a -> Tape a
advance (Tape prev cmd next) = Tape (cmd : prev) (head next) (tail next)

retreat :: Tape a -> Tape a
retreat (Tape prev cmd next) = Tape (tail prev) (head prev) (cmd : next)

update :: (a -> a) -> Tape a -> Tape a
update f (Tape prev cmd next) = Tape prev (f cmd) next

initState :: State
initState = (mount $ repeat '\x00', None)

initScript :: String -> Script
initScript script = mount (map parseCmd script ++ [End])

eval :: (Script, State) -> IO (Script, State)
-- forward jumping
eval (script, (array, Forward))
  | extract script == JumpB = pure (advance script, (array, None))
  | otherwise               = pure (advance script, (array, Forward))

-- backwards jumping
eval (script, (array, Backward))
  | extract script == JumpF = pure (advance script, (array, None))
  | otherwise               = pure (retreat script, (array, Backward))

-- no jumping
eval (script, (array, None))
  | extract script == IncP = pure (advance script, (advance array, None))
  | extract script == DecP = pure (advance script, (retreat array, None))
  | extract script == Inc = pure (advance script, (extend inc' array, None))
  | extract script == Dec = pure (advance script, (extend dec' array, None))
  | extract script == Out =  putChar (extract array)
  >> pure (advance script, (array, None))
  | extract script == In = getChar
  >>= \c -> pure (advance script, (update (const c) array, None))
  | extract script == JumpF = case extract array of
    '\x00' -> pure (advance script, (array, Forward))
    _      -> pure (advance script, (array, None))
  | extract script == JumpB = case extract array of
    '\x00' -> pure (advance script, (array, None))
    _      -> pure (retreat script, (array, Backward))

-- Ignore all other states
eval (script, (array, None)) = pure (advance script, (array, None))

runEval :: (Script, State) -> IO ()
runEval = go
 where
  go :: (Script, State) -> IO ()
  go (Tape _ End _, _    ) = pure ()
  go (script      , state) = eval (script, state) >>= go
