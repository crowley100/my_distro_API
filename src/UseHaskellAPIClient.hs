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

-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from UseHaskellAPI.hs

loadEnvVars :: Maybe String -> ClientM ResponseData
getREADME :: ClientM ResponseData
storeMessage :: Message -> ClientM Bool
logIn :: Login -> ClientM [ResponseData]
signUp :: Login -> ClientM Bool
searchMessage :: Maybe String -> ClientM [Message]
performRestCall :: Maybe String -> ClientM ResponseData
-- lock stuff here
doLockFile :: String -> ClientM Bool
doUnlockFile :: String -> ClientM Bool
doFileLocked :: Maybe String -> ClientM Bool


-- | The following provides the implementations of these types
-- Note that the order of the functions must match the endpoints in the type API from UseHaskell.hs

(loadEnvVars :<|> getREADME :<|> storeMessage :<|> logIn :<|> signUp :<|> searchMessage :<|> performRestCall) = client restAPI
(doLockFile :<|> doUnlockFile :<|> doFileLocked) = client restLockAPI
