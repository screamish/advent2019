{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Day2
  ( Args(..)
  , Expr(..)
  , p
  , f
  , run
  , solvePartA
  , solvePartB
  ) where

import RIO
import RIO.List
import qualified RIO.Text as T
import qualified RIO.Partial as RIO'
import qualified RIO.Vector.Unboxed as VU
import RIO.Vector.Unboxed ((!?))
import RIO.Vector.Unboxed.Partial ((//), (!))
import Text.Parsec
import Text.Parsec.String

-- the above felt like a dead end, actually, time to try a new approach
numListParser :: Parser [Int]
numListParser = sepBy1 integer (char ',')
  where
    integer :: Parser Int
    integer = RIO'.read <$> many1 digit

run :: Text -> [Int]
run = VU.toList . f . p

solvePartA :: Text -> Int
solvePartA = runOnce 12 2 . p

solvePartB :: Text -> Int
solvePartB input =
  let state = p input
      (result : _) = do
        i1 <- [0..99]
        i2 <- [0..99]
        guard $ runOnce i1 i2 state == 19690720
        return $ 100 * i1 + i2
  in result

runOnce :: Int -> Int -> VU.Vector Int -> Int
runOnce i1 i2 v =
  let initial = v // [(1, i1), (2, i2)]
  in f initial ! 0 

p :: Text -> VU.Vector Int 
p input = case runParser numListParser () "" (T.unpack input) of
  Left err -> error (show err)
  Right e -> VU.fromList e

f :: VU.Vector Int -> VU.Vector Int
f state = fromMaybe state $ lastMaybe $ unfoldr step (0, state)
  where
    step :: (Int, VU.Vector Int) -> Maybe (VU.Vector Int, (Int, VU.Vector Int))
    step (i, v) = do
      op <- v !? i >>= \case
        1 -> Just (+)
        2 -> Just (*)
        99 -> Nothing
        err -> error $ "unknown op code: " ++ show err
      a <- v !? (i + 1)
      b <- v !? (i + 2)
      output <- v !? (i + 3)
      result <- op <$> v !? a <*> v !? b
      let next = v // [(output, result)]
      return (next, (i + 4, next))


-- an earlier dead end

data Args = Args { inputAPosition :: Int , inputBPosition :: Int , outputPosition :: Int }
    deriving (Show, Eq)

data Expr
  = Add Args
  | Mul Args
  | Terminate
  deriving (Show, Eq)

exprParser :: Parser Expr
exprParser =
  choice [add, mul, term]
  where
    integer :: Parser Int
    integer = RIO'.read <$> many1 digit
    args :: Parser Args
    args = do
      a <- integer
      _ <- char ','
      b <- integer
      _ <- char ','
      out <- integer
      return Args { inputAPosition = a, inputBPosition = b, outputPosition = out }
    add :: Parser Expr
    add = do
      _ <- char '1'
      _ <- char ','
      Add <$> args
    mul = do
      _ <- char '2'
      _ <- char ','
      Mul <$> args
    term = string "99" $> Terminate
