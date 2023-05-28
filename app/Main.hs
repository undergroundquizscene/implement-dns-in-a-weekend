{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (id)

import Control.Category hiding ((.), id)
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State.Strict hiding (put, get)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits hiding (bit)
import Data.Bool
import Data.ByteString as ByteString hiding (split)
import Data.ByteString.Builder
import Data.Generics.Product
import Data.Int
import Data.List as List
import Data.Map.Strict as Map
import Data.Maybe
import Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Debug.Trace
import GHC.Generics
import GHC.Stack
import Network.Socket
import Network.Socket.ByteString
import System.Random

main :: IO ()
main = do
  gen <- getStdGen
  exampleCom <- lookupDomain gen "www.example.com"
  putStrLn ("example.com: " <> exampleCom)
  recurseCom <- lookupDomain gen "recurse.com"
  putStrLn ("recurse.com: " <> recurseCom)
  metafilterCom <- lookupDomain gen "metafilter.com"
  putStrLn ("metafilter.com: " <> metafilterCom)
  facebookCom <- lookupDomain gen "www.facebook.com"
  putStrLn ("www.facebook.com: " <> facebookCom)
  wwwMetafilterCom <- lookupDomain gen "www.metafilter.com"
  putStrLn ("www.metafilter.com: " <> wwwMetafilterCom)


lookupDomain :: StdGen -> Text -> IO String
lookupDomain gen domain = do
  s <- socket AF_INET Datagram defaultProtocol
  let query = buildQuery gen domain 1
  let encodedDomain = runPut (putUncompressedDomain (domainFromText domain))
  connect s (SockAddrInet 53 (tupleToHostAddress (8, 8, 8, 8)))
  sendAll s query
  response <- recv s 1024
  let parsedResponse = decodeOrFail @DNSPacket (fromStrict response)
  case parsedResponse of
    Right (_, _, DNSPacket{..}) ->
      return (showAsIPAddress ((answers !! 0) ^. field @"data_" . field @"data_"))

data DNSPacket = DNSPacket
  { header :: DNSHeader
  , questions :: [DNSQuestion]
  , answers :: [DNSRecord]
  , authorities :: [DNSRecord]
  , additionals :: [DNSRecord]
  } deriving (Show, Eq, Generic)

instance Binary DNSPacket where
  get = do
    header@DNSHeader{..} <- get
    getQuestions <- replicateM (fromIntegral numQuestions) getDNSQuestion
    getAnswers <- replicateM (fromIntegral numAnswers) getDNSRecord
    getAuthorities <- replicateM (fromIntegral numAuthorities) getDNSRecord
    getAdditionals <- replicateM (fromIntegral numAdditionals) getDNSRecord
    return (fst (flip runState Map.empty do
      questions <- sequence getQuestions
      answers <- sequence getAnswers
      authorities <- sequence getAuthorities
      additionals <- sequence getAdditionals
      return DNSPacket{..}))

  put DNSPacket{..} = do
    put header
    foldMap putDNSQuestion questions
    foldMap putDNSRecord answers
    foldMap putDNSRecord authorities
    foldMap putDNSRecord additionals

data DNSHeader = DNSHeader
  { queryID :: Word16
  , flags :: Word16
  , numQuestions :: Word16
  , numAnswers :: Word16
  , numAuthorities :: Word16
  , numAdditionals :: Word16
  } deriving (Show, Eq, Generic)

instance Binary DNSHeader where

data DNSQuestion = DNSQuestion
  { name :: DNSDomain
  , type_ :: Word16
  , class_ :: Word16
  } deriving (Show, Eq, Generic)

putDNSQuestion :: DNSQuestion -> Put
putDNSQuestion DNSQuestion{..} = do
  putUncompressedDomain name
  put type_
  put class_

getDNSQuestion :: Get (WithDomainCache DNSQuestion)
getDNSQuestion = do
  f <- getDomain
  type_ <- get
  class_ <- get
  return do
    name <- f
    return DNSQuestion{..}

type WithDomainCache a = State (Map Word16 DNSDomain) a

newtype DNSDomain = DNSDomain [Text]
  deriving (Show, Eq, Generic)

domainFromText :: Text -> DNSDomain
domainFromText = Text.splitOn "." >>> DNSDomain

domainToText :: DNSDomain -> Text
domainToText (DNSDomain fs) = mconcat (List.intersperse "." fs)

putUncompressedDomain :: DNSDomain -> Put
putUncompressedDomain (DNSDomain fragments) = do
  let putFragment f =
        put (lengthPrefixed @Word8 (encodeUtf8 f))
  _ <- traverse putFragment fragments
  put (zeroBits :: Word8)

getDomainFragment :: Get (Maybe (Int64, Text))
getDomainFragment = do
  location <- bytesRead
  LengthPrefixed{..} <- get @(LengthPrefixed Word8)
  if ByteString.null data_
  then return Nothing
  else return (Just (location, decodeUtf8 data_))

followDomainPointer :: HasCallStack => Get (Map Word16 DNSDomain -> DNSDomain)
followDomainPointer = do
  w :: Word16 <- get
  let pointer = w `clearBit` 15 `clearBit` 14
  return (\map ->
            fromMaybe (error ("Couldn't find location " <> show pointer <> " in domain map: " <> show map))
            (Map.lookup pointer map))

getDomain :: Get (WithDomainCache DNSDomain)
getDomain = do
  length <- lookAhead getWord8
  if testBit length 7 && testBit length 6
  then do
    f <- followDomainPointer
    return (state (\m -> (f m, m)))
  else do
    getDomainFragment >>=
      (\case
        Nothing -> return (return (DNSDomain []))
        Just (location, fragment) -> do
          f <- getDomain
          return do
            DNSDomain tail <- f
            let newDomain = DNSDomain (fragment : tail)
            modify (Map.insert (fromIntegral location) newDomain)
            return newDomain)

lengthPrefixed :: Num l => ByteString -> LengthPrefixed l
lengthPrefixed data_ = let
  length = fromIntegral (ByteString.length data_)
  in LengthPrefixed{..}

data LengthPrefixed l = LengthPrefixed
  { length :: l
  , data_ :: ByteString
  }
  deriving (Show, Eq, Generic)

instance (Binary l, Integral l) => Binary (LengthPrefixed l) where
  get = do
    numberOfBytes <- get
    if numberOfBytes == 0
    then return (LengthPrefixed numberOfBytes "")
    else do
      bytes <- replicateM (fromIntegral numberOfBytes) get
      return (LengthPrefixed numberOfBytes (ByteString.pack bytes))

  put (LengthPrefixed l bs) = do
    put l
    putByteString bs

-- decodeDomainSimple :: ByteString -> (ByteString, Maybe ByteString)
-- decodeDomainSimple =
--   splitFragments >>>
--   first (Prelude.foldr joinWithDot "")
--   where
--     joinWithDot :: ByteString -> ByteString -> ByteString
--     joinWithDot x acc =
--       if ByteString.null acc
--       then x
--       else x <> "." <> acc
--     splitFragments :: ByteString -> ([ByteString], Maybe ByteString)
--     splitFragments bs = case splitFragment bs of
--       Nothing -> ([], Nothing)
--       Just (Nothing, rest) -> ([], Just rest)
--       Just (Just f, rest) -> first (f :) (splitFragments rest)
--     splitFragment bs = case ByteString.uncons bs of
--         Nothing -> Nothing
--         Just (0, rest) -> Just (Nothing, rest)
--         Just (n, rest) -> let
--           (fragment, rest') = ByteString.splitAt (fromIntegral n) rest
--           in Just (Just fragment, rest')


data QR = Query | Reply deriving (Show, Eq)

qr :: Lens' Word16 QR
qr = bit 15 . iso fromBool toBool
  where
    toBool Query = False
    toBool Reply = True
    fromBool = bool Query Reply

recursionDesired :: Lens' Word16 Bool
recursionDesired = bit 8

bit :: Bits w => Int -> Lens' w Bool
bit n = lens (flip testBit n) (\word -> bool (clearBit word n) (setBit word n))

buildQuery :: RandomGen g => g -> Text -> Word16 -> ByteString
buildQuery gen domain recordType = let
  (id, _) = genWord16 gen
  question = DNSQuestion (domainFromText domain) recordType 1
  header = DNSHeader
    { queryID = id
    , flags = 0 & recursionDesired .~ True
    , numQuestions = 1
    , numAnswers = 0
    , numAuthorities = 0
    , numAdditionals = 0
    }
  in toStrict (encode header <> runPut (putDNSQuestion question))

data DNSRecord = DNSRecord
  { name :: DNSDomain
  , type_ :: Word16
  , class_ :: Word16
  , ttl :: Int32
  , data_ :: LengthPrefixed Word16
  } deriving (Show, Eq, Generic)

getDNSRecord :: Get (WithDomainCache DNSRecord)
getDNSRecord = do
  s <- getDomain
  type_ <- get
  class_ <- get
  ttl <- get
  data_ <- get
  return do
    name <- s
    return DNSRecord{..}

putDNSRecord :: DNSRecord -> Put
putDNSRecord DNSRecord{..} = do
  putUncompressedDomain name
  put type_
  put class_
  put ttl
  put data_

showAsIPAddress :: ByteString -> String
showAsIPAddress bs =
  mconcat (List.intersperse "." (fmap show (ByteString.unpack bs)))
