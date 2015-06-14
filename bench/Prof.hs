{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data
import Nested
import qualified Data.ByteString as BS

writeBurst :: IO ()
writeBurst = do
  BS.writeFile "burst.pb" $ enc $ burst 1000000

readBurst :: IO ()
readBurst = do
  a <- BS.readFile "burst.pb"
  print $ decBurst a

writeNested :: IO ()
writeNested = do
  BS.writeFile "nested.pb" $ enc $ nested 100 1000

readNested :: IO ()
readNested = do
  a <- BS.readFile "nested.pb"
  print $ decNested a

main = readNested
