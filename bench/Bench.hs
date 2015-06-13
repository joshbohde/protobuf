{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.DeepSeq         (NFData)
import Criterion.Main
import Data.ProtocolBuffers
import GHC.Generics(Generic)
import Data.Serialize.Put
import Data.Serialize.Get
import Data.ByteString(ByteString)

data Burst = Burst {
    frames :: Repeated 1 (Value ByteString)
} deriving (Generic, Eq, Show)

instance NFData Burst
instance Encode Burst
instance Decode Burst

burst :: Int -> Burst
burst = Burst . putField . flip replicate "abcd"

encodeBurst :: Int -> ByteString
encodeBurst = enc . burst

enc :: Encode a => a -> ByteString
enc = runPut . encodeMessage

dec :: Decode a => ByteString -> a
dec a = case runGet decodeMessage a of
  Right a' -> a'
  Left s -> error s

decBurst :: ByteString -> Burst
decBurst = dec

main :: IO ()
main = defaultMain [
  bgroup "encoding" [
     bgroup "burst" [
        bench "1" $ nf enc (burst 1),
        bench "10" $ nf enc (burst 10),
        bench "100" $ nf enc (burst 100),
        bench "1000" $ nf enc (burst 1000)
        ]
     ],
  bgroup "decoding" [
     bgroup "burst" [
        bench "1" $ nf decBurst (encodeBurst 1),
        bench "10" $ nf decBurst (encodeBurst 10),
        bench "100" $ nf decBurst (encodeBurst 100),
        bench "1000" $ nf decBurst (encodeBurst 1000)
        ]
     ]
  ]
