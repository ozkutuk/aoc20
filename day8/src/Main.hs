{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Control.Applicative ((<|>))
import           Data.Either (fromRight)
import           Data.Functor (($>))
import           Data.IntSet (IntSet)
import qualified Data.IntSet as S
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import           Data.Void (Void)
import           Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as PL

type Parser = Parsec Void Text
type ParseError = P.ParseErrorBundle Text Void

type Program = Vector Op

data Op
  = Nop
  | Acc !Int
  | Jmp !Int
  deriving Show

data CheckCycle = CheckCycle | NoCheckCycle deriving Show

data ProgramState =
  ProgramState
    { currentInstruction :: !Int
    , accumulator :: !Int
    , executedInstructions :: !IntSet
    }
    deriving Show

main :: IO ()
main = do
  instructions <- T.readFile "input.txt"
  putStrLn $ "part1: " <> show (part1 instructions)
  putStrLn $ "part2: " <> show (part2 instructions)
  
part1 :: Text -> Int
part1 =
  accumulator .
  runProgram CheckCycle initState .
  fromRight V.empty .
  parseProgram

part2 :: Text -> Int
part2 =
  accumulator .
  runProgram NoCheckCycle initState .
  fromJust .
  findCorrectProgram .
  fromRight V.empty .
  parseProgram

initState :: ProgramState
initState = ProgramState
  { currentInstruction = 0
  , accumulator = 0
  , executedInstructions = S.empty
  }

step :: ProgramState -> Program -> ProgramState
step s@ProgramState{ currentInstruction, accumulator,
                     executedInstructions } prg =
  result { executedInstructions = S.insert currentInstruction executedInstructions
         }
  where
    result =
      case prg ! currentInstruction of
        Nop -> s { currentInstruction = currentInstruction + 1 }
        Acc n -> s { accumulator = accumulator + n
                   , currentInstruction = currentInstruction + 1
                   }
        Jmp n -> s { currentInstruction = currentInstruction + n }

runProgram :: CheckCycle -> ProgramState -> Program -> ProgramState
runProgram checkCycle s@ProgramState{ currentInstruction, executedInstructions } prg =
  if noCycles && currentInstruction < V.length prg
  then runProgram checkCycle (step s prg) prg
  else s
  where
    noCycles :: Bool
    noCycles =
      case checkCycle of
        CheckCycle -> currentInstruction `S.notMember` executedInstructions
        NoCheckCycle -> True

findCorrectProgram :: Program -> Maybe Program
findCorrectProgram prg = V.find isCorrectProgram jumpVariations
  where
    isJmp :: Op -> Bool
    isJmp = \case
      Jmp _ -> True
      _     -> False

    jumpInsts :: Vector Int
    jumpInsts = V.findIndices isJmp prg

    jumpVariations :: Vector Program
    jumpVariations = V.map (\i -> prg // [(i, Nop)]) jumpInsts

    isCorrectProgram :: Program -> Bool
    isCorrectProgram p = currentInstruction (runProgram CheckCycle initState p) >= V.length prg

parseProgram :: Text -> Either ParseError Program
parseProgram = P.parse program ""

program :: Parser Program
program = V.fromList <$> (P.sepEndBy1 opcode PC.newline <* P.eof)

opcode :: Parser Op
opcode = ((PC.string "nop " $> Nop) <* signedInt)
     <|> ((PC.string "acc " $> Acc) <*> signedInt)
     <|> ((PC.string "jmp " $> Jmp) <*> signedInt)
  where
    signedInt :: Parser Int
    signedInt = PL.signed (pure ()) PL.decimal
