{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Lens
import Data.Bits hiding (bit)
import Data.Bool
import Data.ByteString as ByteString hiding (split)
import Data.ByteString.Builder
import Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Text.IO qualified as Text
import Data.Word
import Network.Socket
import Network.Socket.ByteString
import System.Random

main :: IO ()
main = do
  s <- socket AF_INET Datagram defaultProtocol
  gen <- getStdGen
  let query = toStrict (toLazyByteString (buildQuery gen "www.example.com" 1))
  putStrLn ("Query is: " <> show (byteStringHex query))
  connect s (SockAddrInet 53 (tupleToHostAddress (8, 8, 8, 8)))
  sendAll s query
  response <- recv s 1024
  putStrLn ("Response is: " <> show (byteStringHex response))

data DNSHeader = DNSHeader
  { queryID :: HeaderField
  , flags :: HeaderField
  , numQuestions :: HeaderField
  , numAnswers :: HeaderField
  , numAuthorities :: HeaderField
  , numAdditionals :: HeaderField
  } deriving (Show, Eq)

-- | (most significant byte, least significant byte)
type HeaderField = Word16

doubleByteToBytes :: Word16 -> Builder
doubleByteToBytes = word16BE

headerToBytes :: DNSHeader -> Builder
headerToBytes DNSHeader{..} = foldMap doubleByteToBytes [ queryID
                                                        , flags
                                                        , numQuestions
                                                        , numAnswers
                                                        , numAuthorities
                                                        , numAdditionals
                                                        ]

data DNSQuestion = DNSQuestion
  { name :: ByteString
  , type_ :: Word16
  , class_ :: Word16
  } deriving (Show, Eq)

questionToBytes :: DNSQuestion -> Builder
questionToBytes DNSQuestion{..} = byteString name <> foldMap doubleByteToBytes [type_, class_]

data QR = Query | Reply deriving (Show, Eq)

qr :: Lens' HeaderField QR
qr = bit 15 . iso fromBool toBool
  where
    toBool Query = False
    toBool Reply = True
    fromBool = bool Query Reply

recursionDesired :: Lens' HeaderField Bool
recursionDesired = bit 8

bit :: Bits w => Int -> Lens' w Bool
bit n = lens (flip testBit n) (\word -> bool (clearBit word n) (setBit word n))

encodeDomain :: Text -> Builder
encodeDomain d = foldMap prefix (Text.split (== '.') d) <> word8 zeroBits
  where
    prefix :: Text -> Builder
    prefix t = int8 (fromIntegral (Text.length t)) <> byteString (encodeUtf8 t)

buildQuery :: RandomGen g => g -> Text -> HeaderField -> Builder
buildQuery gen domain recordType = let
  (id, _) = genWord16 gen
  question = DNSQuestion (toStrict (toLazyByteString (encodeDomain domain))) recordType 1
  header = DNSHeader
    { queryID = id
    , flags = (0 & recursionDesired .~ True)
    , numQuestions = 1
    , numAnswers = 0
    , numAuthorities = 0
    , numAdditionals = 0
    }
  in headerToBytes header <> questionToBytes question

-- data Opcode = Query | IQuery | Status deriving (Show, Eq)

-- opcode :: Lens' HeaderField Opcode
-- opcode = lens getter setter
--   where
--     getter =
