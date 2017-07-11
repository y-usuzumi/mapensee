{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE MultiWayIf #-}

import           "cryptohash" Crypto.Hash
import qualified Data.ByteString.Char8 as C8
import           Data.Char
import           Numeric
import           Options.Applicative

data PasswordData = PasswordData { uniqueKey :: {-# UNPACK #-} !String
                                 , salt      :: {-# UNPACK #-} !String
                                 , passwordLength :: {-# UNPACK #-} !Int
                                 }

plReader :: ReadM Int
plReader = eitherReader $ \arg ->
  case () of _
              | [(r, "")] <- readDec arg
              , r > 0 && r <= 32 -> return r
              | otherwise -> Left $ "cannot parse value `" ++ arg ++ "'"

argsParser :: Parser PasswordData
argsParser = PasswordData
             <$> argument str (metavar "UNIQUEKEY" <> help "Unique key")
             <*> argument str (metavar "SALT" <> help "Salt")
             <*> argument plReader (metavar "PASSWORDLENGTH" <> help "Password length (1 ~ 32)")

makePassword :: PasswordData -> String
makePassword pd@PasswordData{..} = map intToPasswordChar $ take passwordLength $ (splitto . hashToInt . makeHash) pd
  where

    intToPasswordChar :: Integer -> Char
    intToPasswordChar i
      | i >= 0 && i <= 9 = chr (48 + fromIntegral i)  -- 0..9
      | i >= 10 && i <= 35 = chr (87 + fromIntegral i)  -- a..z
      | i >= 36 && i <= 61 = chr (29 + fromIntegral i)  -- A..Z
      | otherwise = error "BUG: intToPasswordChar i out of bound"

    splitto :: Integer -> [Integer]
    splitto i = let (q, r) = i `quotRem` 61 in r:splitto q

    hashToInt :: String -> Integer
    hashToInt = fst . head . readHex

    makeHash :: PasswordData -> String
    makeHash PasswordData{..} = C8.unpack $ digestToHexByteString (hash . C8.pack $ (uniqueKey ++ salt) :: Digest SHA256)

main :: IO ()
main = do
  passwordData <- execParser (
    info (helper <*> argsParser) (
      fullDesc <> progDesc "Password generator" <> header "Forget Your Password"
      )
    )

  putStrLn $ makePassword passwordData
