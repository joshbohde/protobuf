{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data where

import Control.DeepSeq         (NFData)
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
dec m = case runGet decodeMessage m of
  Right a -> a
  Left s -> error s

decBurst :: ByteString -> Burst
decBurst = dec
