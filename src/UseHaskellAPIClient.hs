{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module UseHaskellAPIClient where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           UseHaskellAPI


restAPI :: Proxy API
restAPI = Proxy

restLockAPI :: Proxy LockAPI
restLockAPI = Proxy

restFileAPI :: Proxy FileAPI
restFileAPI = Proxy

restDirAPI :: Proxy DirAPI
restDirAPI = Proxy

-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from UseHaskellAPI.hs

loadEnvVars :: Maybe String -> ClientM ResponseData
getREADME :: ClientM ResponseData
storeMessage :: Message -> ClientM Bool
loadPublicKey :: ClientM [ResponseData]
logIn :: Login -> ClientM [ResponseData]
signUp :: Login -> ClientM ResponseData
searchMessage :: Maybe String -> ClientM [Message]
performRestCall :: Maybe String -> ClientM ResponseData
-- lock stuff here
lock :: String -> ClientM Bool
unlock :: String -> ClientM Bool
locked :: Maybe String -> ClientM Bool
-- file service stuff here
download :: Maybe String -> ClientM [Message]
upload :: Message -> ClientM Bool
-- directory service stuff here
lsDir :: ClientM [ResponseData]
lsFile :: Maybe String -> ClientM [ResponseData]
fileQuery :: Message -> ClientM [FileRef]
mapFile :: Message -> ClientM [FileRef]

-- | The following provides the implementations of these types
-- Note that the order of the functions must match the endpoints in the type API from UseHaskell.hs

(loadEnvVars :<|> getREADME :<|> storeMessage :<|> loadPublicKey :<|> logIn :<|> signUp :<|> searchMessage :<|> performRestCall) = client restAPI
(lock :<|> unlock :<|> locked) = client restLockAPI
(download :<|> upload) = client restFileAPI
(lsDir :<|> lsFile :<|> fileQuery :<|> mapFile) = client restDirAPI
