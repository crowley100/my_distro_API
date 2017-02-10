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
import           UseHaskellAPITypes


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

-- Service information begins --
defaultHost = "10.6.77.140" -- temporary ip to work with tcd proxy

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
type FileAPI = "download"               :> ReqBody '[JSON] Message :> Get '[JSON] [Message]
          :<|> "upload"                 :> ReqBody '[JSON] Message :> Post '[JSON] Bool
          :<|> "updateShadowDB"         :> ReqBody '[JSON] Shadow :> Post '[JSON] Bool
          :<|> "pushTransaction"        :> ReqBody '[JSON] String :> Post '[JSON] Bool
          :<|> "replicateFile"          :> ReqBody '[JSON] Message :> Post '[JSON] Bool

type DirAPI = "lsDir"                   :> ReqBody '[JSON] StrWrap :> Get '[JSON] ResponseData
         :<|> "lsFile"                  :> ReqBody '[JSON] Message :> Get '[JSON] [FsContents]
         :<|> "fileQuery"               :> ReqBody '[JSON] Message3 :> Get '[JSON] [SendFileRef]
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
