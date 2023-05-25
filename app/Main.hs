{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (id)

import Control.Category hiding ((.), id)
import Control.Lens
import Data.Bifunctor
import Data.Bits hiding (bit)
import Data.Bool
import Data.ByteString as ByteString hiding (split)
import Data.ByteString.Builder
import Data.Int
import Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
-- import Data.Text.IO qualified as Text
import Data.Word
import Network.Socket
import Network.Socket.ByteString
import System.Random

main :: IO ()
main = do
  s <- socket AF_INET Datagram defaultProtocol
  gen <- getStdGen
  let query = toStrict (toLazyByteString (buildQuery gen "www.example.com" (0, 1)))
  putStrLn ("Query is: " <> show (byteStringHex query))
  connect s (SockAddrInet 53 (tupleToHostAddress (8, 8, 8, 8)))
  sendAll s query
  response <- recv s 1024
  let (header, rest) = parseHeader response
  let (question, _rest') = parseQuestion rest
  putStrLn ("Response is: " <> show (byteStringHex response))
  putStrLn ("Parsed header is: " <> show header)
  putStrLn ("Parsed question is: " <> show question)

data DNSHeader = DNSHeader
  { queryID :: HeaderField
  , flags :: HeaderField
  , numQuestions :: HeaderField
  , numAnswers :: HeaderField
  , numAuthorities :: HeaderField
  , numAdditionals :: HeaderField
  } deriving (Show, Eq)

type HeaderField = DoubleByte

-- | (most significant byte, least significant byte)
type DoubleByte = (Word8, Word8)

doubleByteToBytes :: DoubleByte -> Builder
doubleByteToBytes (msb, lsb) = word8 msb <> word8 lsb

headerToBytes :: DNSHeader -> Builder
headerToBytes DNSHeader{..} = foldMap doubleByteToBytes [ queryID
                                                        , flags
                                                        , numQuestions
                                                        , numAnswers
                                                        , numAuthorities
                                                        , numAdditionals
                                                        ]

parseHeader :: ByteString -> (Maybe DNSHeader, ByteString)
parseHeader bs = case first ByteString.unpack (ByteString.splitAt 12 bs) of
  ([i1, i2, fl1, fl2, nq1, nq2, na1, na2, nu1, nu2, nd1, nd2], rest) -> (Just DNSHeader
    { queryID = (i1, i2)
    , flags = (fl1, fl2)
    , numQuestions = (nq1, nq2)
    , numAnswers = (na1, na2)
    , numAuthorities = (nu1, nu2)
    , numAdditionals = (nd1, nd2)
    }, rest)
  _ -> (Nothing, bs)

data DNSQuestion = DNSQuestion
  { name :: ByteString
  , type_ :: DoubleByte
  , class_ :: DoubleByte
  } deriving (Show, Eq)

questionToBytes :: DNSQuestion -> Builder
questionToBytes DNSQuestion{..} = byteString name <> foldMap doubleByteToBytes [type_, class_]

parseQuestion :: ByteString -> (Maybe DNSQuestion, ByteString)
parseQuestion bs = let
  (domain, rest) = decodeDomainSimple bs
  in case fmap (first ByteString.unpack . (ByteString.splitAt 4)) rest of
    Just ([t1, t2, c1, c2], rest') ->
      ( Just DNSQuestion { name = domain
                         , type_ = (t1, t2)
                         , class_ = (c1, c2)
                         }
      , rest'
      )
    _ -> (Nothing, bs)


data QR = Query | Reply deriving (Show, Eq)

qr :: Lens' HeaderField QR
qr = _1 . bit 7 . iso fromBool toBool
  where
    toBool Query = False
    toBool Reply = True
    fromBool = bool Query Reply

recursionDesired :: Lens' HeaderField Bool
recursionDesired = _2 . bit 0

bit :: Bits w => Int -> Lens' w Bool
bit n = lens (flip testBit n) (\word -> bool (clearBit word n) (setBit word n))

encodeDomain :: Text -> Builder
encodeDomain d = foldMap prefix (Text.split (== '.') d) <> word8 zeroBits
  where
    prefix :: Text -> Builder
    prefix t = int8 (fromIntegral (Text.length t)) <> byteString (encodeUtf8 t)

decodeDomainSimple :: ByteString -> (ByteString, Maybe ByteString)
decodeDomainSimple =
  splitFragments >>>
  first (Prelude.foldr joinWithDot "")
  where
    joinWithDot :: ByteString -> ByteString -> ByteString
    joinWithDot x acc =
      if ByteString.null acc
      then x
      else x <> "." <> acc
    splitFragments :: ByteString -> ([ByteString], Maybe ByteString)
    splitFragments bs = case splitFragment bs of
      Nothing -> ([], Nothing)
      Just (Nothing, rest) -> ([], Just rest)
      Just (Just f, rest) -> first (f :) (splitFragments rest)
    splitFragment bs = case ByteString.uncons bs of
        Nothing -> Nothing
        Just (0, rest) -> Just (Nothing, rest)
        Just (n, rest) -> let
          (fragment, rest') = ByteString.splitAt (fromIntegral n) rest
          in Just (Just fragment, rest')

buildQuery :: RandomGen g => g -> Text -> HeaderField -> Builder
buildQuery gen domain recordType = let
  (idMsb, gen') = genWord8 gen
  (idLsb, _) = genWord8 gen'
  id = (idMsb, idLsb)
  question = DNSQuestion (toStrict (toLazyByteString (encodeDomain domain))) recordType (0, 1)
  header = DNSHeader
    { queryID = id
    , flags = ((0, 0) & recursionDesired .~ True)
    , numQuestions = (0, 1)
    , numAnswers = (0, 0)
    , numAuthorities = (0, 0)
    , numAdditionals = (0, 0)
    }
  in headerToBytes header <> questionToBytes question

data DNSRecord = DNSRecord
  { name :: ByteString
  , type_ :: DoubleByte
  , class_ :: DoubleByte
  , ttl :: Int32
  , data_ :: ByteString
  }

parseRecord :: ByteString -> (Maybe DNSRecord, ByteString)
parseRecord bs = let
  (domain, rest) = decodeDomainSimple bs
  in case fmap (first ByteString.unpack . (ByteString.splitAt 10)) rest of
    Just ([n1, n2, t1, t2, c1, c2, ttl1, ttl2, ttl3, ttl4, dl1, dl2], rest') -> let
      (data_, rest'') = ByteString.splitAt (dl1, dl2) rest'
      in ( Just DNSRecord
        { name = (n1, n2)
        , type_ = (t1, t2)
        , class_ = (c1, c2)
        , ttl =  (ttl1, ttl2, ttl3, ttl4)
        , data_
        }
      , rest''
      )
    _ -> (Nothing, bs)
