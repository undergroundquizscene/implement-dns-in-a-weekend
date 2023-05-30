{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

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
import Data.Either
import Data.Foldable
import Data.Generics.Product (field)
import Data.Int
import Data.List as List
import Data.Map.Strict as Map
import Data.Maybe
import Data.Text as Text
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Data.Text.Encoding.Error
import GHC.Generics hiding (from, to)
import GHC.Stack
import Network.Socket
import Network.Socket.ByteString
import System.Random

main :: IO ()
main = do
  gen <- getStdGen
  (response, gen) <- resolve gen "google.com" A
  print response
  (response, gen) <- resolve gen "facebook.com" A
  print response
  (response, _) <- resolve gen "twitter.com" A
  print response

resolve :: RandomGen g => g -> Text -> RecordType -> IO (Either String DNSRecord, g)
resolve gen domain recordType = query gen (198, 41, 0, 4)
  where
    query :: RandomGen g => g -> (Word8, Word8, Word8, Word8) -> IO (Either String DNSRecord, g)
    query gen ns = do
      putStrLn ("Querying " <> show ns <> " for " <> show domain)
      (response, gen) <- sendQuery gen ns domain recordType
      case fmap firstAnswer response of
        Right (Just x) -> return (Right x, gen)
        Right Nothing -> case fmap firstNameserverIp response of
          Right (Just DNSRecord{data_ = IPv4 ns}) -> query gen ns
          Right Nothing -> return (Left ("No answer or nameserver IP in: " <> show response), gen)
          _ -> return (Left "Unknown problem", gen)
        Left s -> return (Left ("DNS problem: " <> s), gen)

firstAnswer :: DNSPacket -> Maybe DNSRecord
firstAnswer DNSPacket{..} = List.find (^. field @"type_" . to (== A)) answers

firstNameserverIp :: DNSPacket -> Maybe DNSRecord
firstNameserverIp DNSPacket{..} = List.find (^. field @"type_" . to (== A)) additionals

firstNameserver :: DNSPacket -> Maybe DNSDomain
firstNameserver DNSPacket{..} = fmap (^. field @"name") (List.find (^. field @"type_" . to (== A)) authorities)

sendQuery :: RandomGen g => g -> (Word8, Word8, Word8, Word8) -> Text -> RecordType -> IO (Either String DNSPacket, g)
sendQuery gen server domain recordType = do
  s <- socket AF_INET Datagram defaultProtocol
  let (query, gen') = buildQuery gen domain recordType
  connect s (SockAddrInet 53 (tupleToHostAddress server))
  sendAll s query
  response <- recv s 1024
  _ <- close s
  return (bimap (^. _3) (^. _3) (decodeOrFail @DNSPacket (fromStrict response)), gen')

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
  , flags :: DNSHeaderFlags
  , numQuestions :: Word16
  , numAnswers :: Word16
  , numAuthorities :: Word16
  , numAdditionals :: Word16
  } deriving (Show, Eq, Generic)

instance Binary DNSHeader where

data DNSHeaderFlags = DNSHeaderFlags
  { queryOrReply :: QueryOrReply
  , recursionDesired :: Bool
  } deriving (Show, Eq, Generic)

instance Binary DNSHeaderFlags where
  get = (^. from flagsAsWord16) <$> get
  put = put . (^. flagsAsWord16)

flagsAsWord16 :: Iso' DNSHeaderFlags Word16
flagsAsWord16 = iso
  (\DNSHeaderFlags{..} ->
     zeroBits
     & (queryOrReply' .~ queryOrReply)
     . (recursionDesired' .~ recursionDesired)
   )
  (\w -> DNSHeaderFlags (w ^. queryOrReply') (w ^. recursionDesired'))

data QueryOrReply = Query | Reply deriving (Show, Eq)

queryOrReply' :: Lens' Word16 QueryOrReply
queryOrReply' = bit 15 . iso fromBool toBool
  where
    toBool Query = False
    toBool Reply = True
    fromBool = bool Query Reply

recursionDesired' :: Lens' Word16 Bool
recursionDesired' = bit 8

bit :: Bits w => Int -> Lens' w Bool
bit n = lens (flip testBit n) (\word -> bool (clearBit word n) (setBit word n))

data DNSQuestion = DNSQuestion
  { name :: DNSDomain
  , type_ :: RecordType
  , class_ :: Word16
  } deriving (Show, Eq, Generic)

putDNSQuestion :: DNSQuestion -> Put
putDNSQuestion DNSQuestion{..} = do
  putUncompressedDomain name
  putRecordType type_
  put class_

getDNSQuestion :: Get (WithDomainCache DNSQuestion)
getDNSQuestion = do
  f <- getDomain
  type_ <- getRecordType
  class_ <- get
  return do
    name <- f
    return DNSQuestion{..}

classIn :: Word16
classIn = 1

type WithDomainCache a = State (Map Word16 DNSDomain) a

newtype DNSDomain = DNSDomain [Either UnicodeException Text]
  deriving (Show, Eq, Generic)

domainFromText :: Text -> DNSDomain
domainFromText = Text.splitOn "." >>> (DNSDomain . fmap Right)

domainToText :: DNSDomain -> Either ([UnicodeException], [Text]) Text
domainToText (DNSDomain fs) = case partitionEithers fs of
  ([], ts) -> Right (mconcat (List.intersperse "." ts))
  t -> Left t

putUncompressedDomain :: DNSDomain -> Put
putUncompressedDomain (DNSDomain fragments) = do
  let putFragment f =
        put (lengthPrefixed @Word8 (encodeUtf8 f))
  _ <- traverse putFragment (rights fragments)
  put (zeroBits :: Word8)

getDomainFragment :: Get (Maybe (Int64, Either UnicodeException Text))
getDomainFragment = do
  location <- bytesRead
  LengthPrefixed{..} <- get @(LengthPrefixed Word8)
  if ByteString.null data_
  then return Nothing
  else return (Just (location, decodeUtf8' data_))

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

buildQuery :: RandomGen g => g -> Text -> RecordType -> (ByteString, g)
buildQuery gen domain recordType = let
  (queryID, gen') = genWord16 gen
  question = DNSQuestion (domainFromText domain) recordType classIn
  header = DNSHeader
    { queryID
    , flags = DNSHeaderFlags { queryOrReply = Query, recursionDesired = False }
    , numQuestions = 1
    , numAnswers = 0
    , numAuthorities = 0
    , numAdditionals = 0
    }
  in (toStrict (encode header <> runPut (putDNSQuestion question)), gen')

data DNSRecord = DNSRecord
  { name :: DNSDomain
  , type_ :: RecordType
  , class_ :: Word16
  , ttl :: Int32
  , data_ :: RecordData
  } deriving (Show, Eq, Generic)

getDNSRecord :: Get (WithDomainCache DNSRecord)
getDNSRecord = do
  getName <- getDomain
  type_ <- getRecordType
  class_ <- get
  ttl <- get
  getData_ <- case type_ of
    NS -> skip 2 *> (fmap DomainName <$> getDomain)
    A -> do
      addressLength <- get @Word16
      case addressLength of
        4 -> (return . IPv4) <$> ((,,,) <$> get <*> get <*> get <*> get)
        _ -> error "Unexpected IP Address length"
    AAAA -> (return . IPv6) <$> get
    TXT -> (return . RawData) <$> get
    UnrecognisedType _ -> (return . RawData) <$> get
  return do
    name <- getName
    data_ <- getData_
    return DNSRecord{..}

putDNSRecord :: DNSRecord -> Put
putDNSRecord DNSRecord{..} = do
  putUncompressedDomain name
  putRecordType type_
  put class_
  put ttl
  putRecordData data_

data RecordType
  = A
  | NS
  | TXT
  | AAAA
  | UnrecognisedType Word16
  deriving (Show, Eq, Generic)

typeToWord16 :: RecordType -> Word16
typeToWord16 = \case
  A -> 1
  AAAA -> 28
  NS -> 2
  TXT -> 16
  UnrecognisedType n -> n

typeFromWord16 :: Word16 -> RecordType
typeFromWord16 = \case
  1 -> A
  2 -> NS
  16 -> TXT
  28 -> AAAA
  n -> UnrecognisedType n

putRecordType :: RecordType -> Put
putRecordType = put . typeToWord16

getRecordType :: Get RecordType
getRecordType = typeFromWord16 <$> get

data RecordData
  = RawData (LengthPrefixed Word16)
  | IPv4 (Word8, Word8, Word8, Word8)
  | IPv6 (LengthPrefixed Word16)
  | DomainName DNSDomain
  deriving (Show, Eq, Generic)

putRecordData :: RecordData -> Put
putRecordData = \case
  RawData bs -> put bs
  IPv4 (n1, n2, n3, n4) -> traverse_ put [n1, n2, n3, n4]
  IPv6 bs -> put bs
  DomainName n -> putUncompressedDomain n

showAsIPAddress :: ByteString -> String
showAsIPAddress bs =
  mconcat (List.intersperse "." (fmap show (ByteString.unpack bs)))
