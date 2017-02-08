{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts     #-}

module UseHaskellAPI where


import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics
import           Servant
import           Crypto.BCrypt
import           Crypto.Cipher.AES
import           Codec.Crypto.RSA
import qualified Data.ByteString.Char8        as BS
import           RSAhelpers
import           Database.MongoDB
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           Network.HTTP.Client          (defaultManagerSettings,newManager)
import           Data.Time.Clock
import           Data.Time.Format
import           Data.List
import           System.Process
import           System.IO

-- MongoDB helper (for services and client)
-- | helper method to ensure we force extraction of all results
-- note how it is defined recursively - meaning that draincursor' calls itself.
-- the purpose is to iterate through all documents returned if the connection is
-- returning the documents in batch mode, meaning in batches of retruned results with more
-- to come on each call. The function recurses until there are no results left, building an
-- array of returned [Document]
drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

-- timestamp comparison for client and services
cmpTime :: String -> String -> Double
cmpTime time1 time2 = realToFrac (diffUTCTime (read time1) (read time2)) :: Double

-- sharedCrypto functions
-- appends null ('\0') characters until multiple of 16
aesPad :: String -> String
aesPad text
  | ((mod (length text) 16) == 0) = text
  | otherwise = aesPad (text ++ "\0")

-- strips null ('\0') characters from end of string
aesUnpad :: String -> String
aesUnpad text = takeWhile (/= '\0') text

-- seed -> string_to_encrypt -> encrypted_string
myEncryptAES :: String -> String -> String
myEncryptAES seed text = do
  let bseed = (BS.pack $ aesPad seed)
      btext = (BS.pack $ aesPad text)
  let myKey = initKey bseed
  let encryption = encryptECB myKey btext
  BS.unpack encryption

-- seed -> string_to_decrypt -> decrypted_string (unpadded)
myDecryptAES :: String -> String -> String
myDecryptAES seed text = do
  let bseed = (BS.pack $ aesPad seed)
      btext = (BS.pack text)
  let myKey = initKey bseed
  let decryption = decryptECB myKey btext
  aesUnpad $ BS.unpack decryption

-- RSA to ResponseData
toResponseData :: PubKeyInfo -> [ResponseData]
toResponseData msg@(PubKeyInfo strKey strN strE)=((ResponseData $ strKey):(ResponseData $ strN):(ResponseData $ strE):[])

-- | System data types.
-- | Data type used to store user's credentials after login.
data Details = Details { clientKey    :: String
                       , clientName   :: String
                       , clientSesh   :: String
                       , clientTicket :: String
                       , clientExpiry :: String
                       } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

-- | Generic one element message
data StrWrap = StrWrap { aVal :: String
                       } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

-- | Generic two element message.
data Message = Message { name    :: String
                       , message :: String
                       } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

-- | Generic three element message.
data Message3 = Message3 { one    :: String
                         , two    :: String
                         , three  :: String
                         } deriving (Show, Eq, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

-- | Type to identify transaction owner.
data CurrentTrans = CurrentTrans { tOwner  :: String
                                 , myTID :: String
                                 } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

-- auth stuff
data Login = Login { userName :: String
                   , password :: String
                   } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

-- locking stuff
data Lock = Lock { fPath :: String
                 , state :: Bool
                 , owner :: String
                 }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

deriving instance FromBSON Bool
deriving instance ToBSON   Bool

-- directory stuff starts here
data FileRef = FileRef { fp   :: String
                       , myd  :: String
                       , fid  :: String
                       , fts  :: String
                       }deriving (Show, Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

-- transaction types for directory service
data ShadowRef = ShadowRef { dTID :: String
                           , references :: [FileRef]
                           }deriving (Show, Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data ShadowDirs = ShadowDirs { dTransID :: String
                             , tDirs :: [String]
                             }deriving (Show, Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data SendFileRef = SendFileRef { filePath    :: String
                               , myDirectory :: String
                               , fID         :: String
                               , fTimeStamp  :: String
                               , fServerIP   :: String
                               , fServerPort :: String
                               }deriving (Show, Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

-- file server data that directory server uses to manage replication process
data FsAttributes = FsAttributes { ip      :: String
                                 , port    :: String
                                 }deriving (Show, Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data FsInfo = FsInfo  { myName        :: String
                      , primaryServer :: Maybe FsAttributes
                      , servers       :: [FsAttributes]
                      }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data FileID = FileID  { directory :: String
                      , idNum     :: String
                      }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data FsContents = FsContents  { dirName :: String
                              , fsFiles :: [String]
                              }deriving (Show, Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON)
-- directory stuff ends here

-- transaction stuff starts here
data Modification = Modification { fileRef      :: SendFileRef
                                 , fileContents :: String
                                 }deriving (Show, Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

-- this data type is upload variation for tServer
data FileTransaction = FileTransaction { tID          :: String
                                       , modification :: Modification
                                       }deriving (Show, Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

-- this data type is stored on transaction server
data Transaction = Transaction { transID       :: String
                               , modifications :: [Modification]
                               , readyStates   :: [String]
                               }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data TransLocks = TransLocks { tLocksID   :: String
                             , tFilePaths :: [String]
                             }deriving (Show, Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data Shadow = Shadow { fTID :: String
                     , file :: Message
                     }deriving (Show, Eq, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data ShadowInfo = ShadowInfo { trID  :: String
                             , files :: [Message]
                             }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)
-- transaction stuff ends here

deriving instance FromBSON [Message]
deriving instance ToBSON   [Message]

deriving instance FromBSON [String]
deriving instance ToBSON   [String]

deriving instance FromBSON [Modification]
deriving instance ToBSON   [Modification]

deriving instance FromBSON [FsAttributes]
deriving instance ToBSON   [FsAttributes]

deriving instance FromBSON [FileRef]
deriving instance ToBSON   [FileRef]

deriving instance FromBSON [FsContents]
deriving instance ToBSON   [FsContents]

deriving instance FromBSON (Maybe FsAttributes)
deriving instance ToBSON   (Maybe FsAttributes)

-- | We will also define a simple data type for returning data from a REST call.
data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON,FromBSON, Show)

-- Service information begins --
defaultHost = "10.6.94.79" -- temporary ip to work with tcd proxy

-- parses ip from ifconfig
--defaultHost :: IO String
--defaultHost = do
--  (_, Just hout, _, _) <- createProcess (proc "hostname" ["-I"]){ std_out = CreatePipe }
--  getHost <- hGetContents hout
--  return $ head $ words getHost

servDoCall f p = (SC.runClientM f =<< servEnv p)

servEnv :: Int -> IO SC.ClientEnv
servEnv p = do
  man <- newManager defaultManagerSettings
  --h <- defaultHost
  --putStrLn h
  return (SC.ClientEnv man (SC.BaseUrl SC.Http defaultHost p ""))

-- possibly convert ports to strings...
fs1IP = defaultHost
fs2IP = defaultHost
fs3IP = defaultHost

fs1Port = 8081 :: Int
fs2Port = 8082 :: Int
fs3Port = 8083 :: Int

authIP = defaultHost
authPort = 8001 :: Int

dirIP = defaultHost
dirPort = 8000 :: Int

transIP = defaultHost
transPort = 8080 :: Int

lockIP = defaultHost
lockPort = 8002 :: Int
-- Service information ends --


-- | Next we will define the API for the REST service.
type AuthAPI = "signUp"                     :> ReqBody '[JSON] Login  :> Post '[JSON] ResponseData
          :<|> "logIn"                      :> ReqBody '[JSON] Login  :> Post '[JSON] [ResponseData]
          :<|> "loadPublicKey"              :> Get '[JSON] [ResponseData]

type LockAPI = "lock"                   :> ReqBody '[JSON] Message3 :> Post '[JSON] Bool
          :<|> "unlock"                 :> ReqBody '[JSON] Message3 :> Post '[JSON] Bool
          :<|> "locked"                 :> ReqBody '[JSON] Message :> Get '[JSON] Bool

-- using Message type for files
-- requests rooted through directory service first
type FileAPI = "download"               :> QueryParam "name" String :> Get '[JSON] [Message]
          :<|> "upload"                 :> ReqBody '[JSON] Message  :> Post '[JSON] Bool
          :<|> "updateShadowDB"         :> ReqBody '[JSON] Shadow  :> Post '[JSON] Bool
          :<|> "pushTransaction"        :> ReqBody '[JSON] String  :> Post '[JSON] Bool
          :<|> "replicateFile"          :> ReqBody '[JSON] Message  :> Post '[JSON] Bool

type DirAPI = "lsDir"                   :> ReqBody '[JSON] StrWrap :> Get '[JSON] [FsContents]
         :<|> "lsFile"                  :> ReqBody '[JSON] Message :> Get '[JSON] [FsContents]
         :<|> "fileQuery"               :> ReqBody '[JSON] Message :> Get '[JSON] [SendFileRef]
         :<|> "mapFile"                 :> ReqBody '[JSON] Message :> Get '[JSON] [SendFileRef]
         :<|> "dirShadowing"            :> ReqBody '[JSON] Message3 :> Post '[JSON] [SendFileRef]

type InterServerAPI = "ping"               :> ReqBody '[JSON] Message :> Post '[JSON] Bool -- Message sufficient?
                 :<|> "registerFS"         :> ReqBody '[JSON] Message3 :> Post '[JSON] Bool
                 :<|> "getPropagationInfo" :> ReqBody '[JSON] Message :> Get '[JSON] [FsAttributes]
                 :<|> "dirCommitShadow"     :> ReqBody '[JSON] Message  :> Post '[JSON] Bool
                 :<|> "dirAbortShadow"      :> ReqBody '[JSON] Message  :> Post '[JSON] Bool

-- client will specify transactions separate from regular upload/download
-- upload still rooted through directory service first
type TransAPI = "beginTransaction"      :> Get '[JSON] ResponseData -- tID
           :<|> "tUpload"               :> ReqBody '[JSON] FileTransaction :> Post '[JSON] Bool
           :<|> "commit"                :> ReqBody '[JSON] String :> Get '[JSON] Bool
           :<|> "abort"                 :> ReqBody '[JSON] String :> Get '[JSON] Bool
           :<|> "readyCommit"           :> ReqBody '[JSON] Message :> Get '[JSON] Bool -- tid ++ fpath?
           :<|> "confirmCommit"         :> ReqBody '[JSON] Message :> Get '[JSON] Bool-- tid ++ fpath?
