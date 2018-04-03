module Main where

import Data.Char (ord, chr)

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

type Tape a = ([a], a, [a])
type Script = Tape Command
type Array = Tape Value
type State = (Array, JumpDir)

inc :: Value -> Value
inc v = chr (ord v + 1)

dec :: Value -> Value
dec v = chr (ord v - 1)

mount :: [a] -> Tape a
mount l = ([], head l, tail l)

current :: Tape a -> a
current (_, c, _) = c

advance :: Tape a -> Tape a
advance (prev, cmd, next) = (cmd : prev, head next, tail next)

retreat :: Tape a -> Tape a
retreat (prev, cmd, next) = (tail prev, head prev, cmd : next)

update :: (a -> a) -> Tape a -> Tape a
update f (prev, cmd, next) = (prev, f cmd, next)

initState :: State
initState = (mount $ repeat '\x00', None)

eval :: (Script, State) -> IO (Script, State)
-- forward jumping
eval (script, (array, Forward))
  | current script == JumpB = pure (advance script, (array, None))
  | otherwise               = pure (advance script, (array, Forward))

-- backwards jumping
eval (script, (array, Backward))
  | current script == JumpF = pure (advance script, (array, None))
  | otherwise               = pure (retreat script, (array, Backward))

-- no jumping
eval (script, (array, None))
  | current script == IncP = pure (advance script, (advance array, None))
  | current script == DecP = pure (advance script, (retreat array, None))
  | current script == Inc = pure (advance script, (update inc array, None))
  | current script == Dec = pure (advance script, (update dec array, None))
  | current script == Out =   putChar (current array)
  >> pure (advance script, (array, None))
  | current script == In = getChar
  >>= \c -> pure (advance script, (update (const c) array, None))
  | current script == JumpF = case current array of
    '\x00' -> pure (advance script, (array, Forward))
    _      -> pure (advance script, (array, None))
  | current script == JumpB = case current array of
    '\x00' -> pure (advance script, (array, None))
    _      -> pure (retreat script, (array, Backward))

-- Ignore all other states
eval (script, (array, None)) = pure (advance script, (array, None))


runEval :: (Script, State) -> IO ()
runEval = go
 where
  go :: (Script, State) -> IO ()
  go ((_, End, _), _    ) = pure ()
  go (script     , state) = eval (script, state) >>= go

main :: IO ()
main = go
 where
  go :: IO ()
  go = do
    putStrLn "Please enter script on one line"
    script <- putStr ">>" >>= const getLine
    case script of
      "" -> pure ()
      _ ->
        runEval (mount (map parseCmd script ++ [End]), initState)
