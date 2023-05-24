module Main where

import Control.Lens
import Data.ByteString
import Data.Word

main :: IO ()
main = putStrLn "TODO!"

data DNSHeader = DNSHeader
  { queryID :: Word16
  , flags :: Word16
  , numQuestions :: Word16
  , numAnswers :: Word16
  , numAuthorities :: Word16
  , numAdditionals :: Word16
  } deriving (Show, Eq)

headerToBytes :: DNSHeader -> ByteString
headerToBytes = undefined

data DNSQuestion = DNSQuestion
  { name :: ByteString
  , type_ :: Int
  , class_ :: Int
  } deriving (Show, Eq)

questionToBytes :: DNSQuestion -> ByteString
questionToBytes = undefined

data QueryOrReply = Query | Reply deriving (Show, Eq)

flagQueryOrReply :: Lens' Word16 QueryOrReply
flagQueryOrReply = lens getter setter
  where
    getter = (flip testBit 0) >>> bool Query Reply
    setter Query = flip setBit 0
    setter Reply = flip setBit 1
