{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

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

-- generic message
data Message = Message { name    :: String
                       , message :: String
                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

-- auth stuff
data Login = Login { userName :: String
                   , password :: String
                   } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

-- locking stuff
data Lock = Lock { fName :: String
                 , state :: Bool
                 }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

deriving instance FromBSON Bool
deriving instance ToBSON   Bool

-- directory stuff starts here
data FileRef = FileRef { filePath :: String
                       , fID :: String
                       , fTimeStamp :: String
                       , fServerIP :: String
                       , fServerPort :: String
                       }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data FsInfo = FsInfo  { myName :: String
                      , ip :: String
                      , port :: String
                      }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data FileID = FileID  { directory :: String
                      , idNum :: String
                      }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

data FsContents = FsContents  { dirName :: String
                              , fsFiles :: [String]
                              }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)
-- directory stuff ends here

deriving instance FromBSON [String]
deriving instance ToBSON   [String]

data StrWrap = StrWrap { line :: String
                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

-- | We will also define a simple data type for returning data from a REST call, again with nothing special or
-- particular in the response, but instead merely as a demonstration.

data ResponseData = ResponseData { response :: String
                                 } deriving (Generic, ToJSON, FromJSON,FromBSON, Show)

-- | Next we will define the API for the REST service. This is defined as a 'type' using a special syntax from the
-- Servant Library. A REST endpoint is defined by chaining together a series of elements in the format `A :> B :> C`. A
-- set of rest endpoints are chained in the format `X :<|> Y :<|> Z`. We define a set of endpoints to demonstrate
-- functionality as described int he README.md file below.
--
-- Note in the API below that we can mix GET and Post methods. The type of call is determined by the last element in the
-- :> chain. If the method is Get, then the set of QueryParams determine the attributes of the Get call. If the method
-- is Post, then there will be a single ReqBody element that defines the type being transmitted. The return type for
-- each method is noted in the last element in the :> chain.

type API = "load_environment_variables" :> QueryParam "name" String :> Get '[JSON] ResponseData
      :<|> "getREADME"                  :> Get '[JSON] ResponseData
      :<|> "storeMessage"               :> ReqBody '[JSON] Message  :> Post '[JSON] Bool
      :<|> "loadPublicKey"              :> Get '[JSON] [ResponseData]
      :<|> "logIn"                      :> ReqBody '[JSON] Login  :> Post '[JSON] [ResponseData]
      :<|> "signUp"                     :> ReqBody '[JSON] Login  :> Post '[JSON] ResponseData
      :<|> "searchMessage"              :> QueryParam "name" String :> Get '[JSON] [Message]
      :<|> "performRESTCall"            :> QueryParam "filter" String  :> Get '[JSON] ResponseData

type LockAPI = "lock"                   :> ReqBody '[JSON] String :> Post '[JSON] Bool
          :<|> "unlock"                 :> ReqBody '[JSON] String :> Post '[JSON] Bool
          :<|> "locked"                 :> QueryParam "fName" String :> Get '[JSON] Bool

-- currently using Message type for files... (NOT TESTED!)
type FileAPI = "download"               :> QueryParam "name" String :> Get '[JSON] [Message]
          :<|> "upload"                 :> ReqBody '[JSON] Message  :> Post '[JSON] Bool

-- execute these commands in app as part of upload/download
type DirAPI = "lsDir"                   :> Get '[JSON] [FsContents]
         :<|> "lsFile"                  :> QueryParam "name" String :> Get '[JSON] [FsContents]
         :<|> "fileQuery"               :> ReqBody '[JSON] Message :> Get '[JSON] [FileRef]
         :<|> "mapFile"                 :> ReqBody '[JSON] Message :> Get '[JSON] [FileRef]
