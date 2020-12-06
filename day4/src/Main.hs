{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Control.Applicative ((<|>))
import           Data.Either (rights)
import           Data.Functor (($>))
import           Data.Hashable (Hashable)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Void (Void)
import           Text.Megaparsec (Parsec)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as PC
import qualified Text.Megaparsec.Char.Lexer as PL

import GHC.Generics

main :: IO ()
main = do
  passportBatch <- T.readFile "input.txt"
  putStrLn $ "part1: " <> show (part1 passportBatch)
  putStrLn $ "part2: " <> show (part2 passportBatch)

type Parser = Parsec Void Text

newtype GenericColor = GenericColor Text

newtype Year = Year Integer

data LengthType = In | Cm

data Length = Length !Integer LengthType

data EyeColor = AMB | BLU | BRN | GRY | GRN | HZL | OTH

type Passport = [Field]

data Field
  = BYR !Year
  | IYR !Year
  | EYR !Year
  | HGT !Length
  | HCL !GenericColor
  | ECL !EyeColor
  | PID !Text  -- Number with leading zeroes
  | CID  -- No fields required

data FieldType
  = TyBYR
  | TyIYR
  | TyEYR
  | TyHGT
  | TyHCL
  | TyECL
  | TyPID
  | TyCID
  deriving (Eq, Generic)

instance Hashable FieldType

byField :: Field -> FieldType
byField = \case
  BYR _ -> TyBYR
  IYR _ -> TyIYR
  EYR _ -> TyEYR
  HGT _ -> TyHGT
  HCL _ -> TyHCL
  ECL _ -> TyECL
  PID _ -> TyPID
  CID   -> TyCID

byText :: Text -> FieldType
byText = \case
  "byr" -> TyBYR
  "iyr" -> TyIYR
  "eyr" -> TyEYR
  "hgt" -> TyHGT
  "hcl" -> TyHCL
  "ecl" -> TyECL
  "pid" -> TyPID
  "cid" -> TyCID

part1 :: Text -> Int
part1 =
  length
  . filter (validateMustFields byText)
  . fmap splitFields
  . splitPassports

part2 :: Text -> Int
part2 =
  length
  . filter validatePassport
  . rights
  . fmap (P.parse passport "")
  . splitPassports

splitPassports :: Text -> [Text]
splitPassports = fmap (T.intercalate " " . T.lines) . T.splitOn "\n\n"

-- Only sufficient for part1
splitFields :: Text -> [Text]
splitFields = fmap (T.takeWhile (/= ':')) . T.words

validatePassport :: Passport -> Bool
validatePassport p = validateMustFields byField p && all validateField p

validateField :: Field -> Bool
validateField = \case
  BYR (Year y) -> between 1920 2002 y
  IYR (Year y) -> between 2010 2020 y
  EYR (Year y) -> between 2020 2030 y
  HGT (Length l unit) -> case unit of
                           Cm -> between 150 193 l
                           In -> between 59 76 l
  _            -> True
  where
    between :: Ord a => a -> a -> a -> Bool
    between lower upper x = x >= lower && x <= upper

validateMustFields :: (a -> FieldType) -> [a] -> Bool
validateMustFields fieldTypeBy = (mustFields `HS.isSubsetOf`) . HS.fromList . fmap fieldTypeBy

mustFields :: HashSet FieldType
mustFields = HS.fromList
  [ TyBYR
  , TyIYR
  , TyEYR
  , TyHGT
  , TyHCL
  , TyECL
  , TyPID
  ]

field :: Parser Field
field = ((PC.string "byr" $> BYR <* PC.char ':')  <*> year)
    <|> ((PC.string "iyr" $> IYR <* PC.char ':')  <*> year)
    <|> ((PC.string "eyr" $> EYR <* PC.char ':')  <*> year)
    <|> ((PC.string "hgt" $> HGT <* PC.char ':')  <*> length')
    <|> ((PC.string "hcl" $> HCL <* PC.char ':')  <*> genericColor)
    <|> ((PC.string "ecl" $> ECL <* PC.char ':')  <*> eyeColor)
    <|> ((PC.string "pid" $> PID <* PC.char ':')  <*> pid)
    <|> ((PC.string "cid" $> CID <* PC.char ':')  <* cid)

  where
    year :: Parser Year
    year = Year <$> PL.decimal

    length' :: Parser Length
    length' = (Length <$> PL.decimal) <*> lengthType

    lengthType :: Parser LengthType
    lengthType = (PC.string "cm" $> Cm) <|> (PC.string "in" $> In)

    genericColor :: Parser GenericColor
    genericColor = GenericColor . T.pack <$> (PC.char '#' *> P.count 6 PC.hexDigitChar)

    eyeColor :: Parser EyeColor
    eyeColor = (PC.string "amb" $> AMB)
           <|> (PC.string "blu" $> BLU)
           <|> (PC.string "brn" $> BRN)
           <|> (PC.string "gry" $> GRY)
           <|> (PC.string "grn" $> GRN)
           <|> (PC.string "hzl" $> HZL)
           <|> (PC.string "oth" $> OTH)

    pid :: Parser Text
    pid = T.pack <$> P.count 9 PC.digitChar

    cid :: Parser [Char]
    cid = P.some PC.alphaNumChar

passport :: Parser Passport
passport = (field `P.sepBy1` PC.space) <* P.eof
