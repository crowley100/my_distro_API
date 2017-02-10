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
import           UseHaskellAPITypes


restAuthAPI :: Proxy AuthAPI
restAuthAPI = Proxy

restLockAPI :: Proxy LockAPI
restLockAPI = Proxy

restFileAPI :: Proxy FileAPI
restFileAPI = Proxy

restDirAPI :: Proxy DirAPI
restDirAPI = Proxy

restTransAPI :: Proxy TransAPI
restTransAPI = Proxy

-- | The function type of the interface here.
-- Each function matches one of the endpoints in type API from UseHaskellAPI.hs
signUp :: Login -> ClientM ResponseData
logIn :: Login -> ClientM [ResponseData]
loadPublicKey :: ClientM [ResponseData]

-- lock stuff here
lock :: Message3 -> ClientM Bool
unlock :: Message3 -> ClientM Bool
locked :: Message -> ClientM Bool
-- file service stuff here
download :: Message -> ClientM [Message]
upload :: Message -> ClientM Bool
updateShadowDB :: Shadow -> ClientM Bool
pushTransaction :: String -> ClientM Bool
replicateFile :: Message -> ClientM Bool
-- directory service stuff here
lsDir :: StrWrap -> ClientM ResponseData
lsFile :: Message -> ClientM [FsContents]
fileQuery :: Message3 -> ClientM [SendFileRef]
mapFile :: Message -> ClientM [SendFileRef]
dirShadowing :: Message3 -> ClientM [SendFileRef]
-- transaction stuff here (phase 1 client to transaction server)
beginTransaction :: ClientM ResponseData
tUpload :: FileTransaction -> ClientM Bool
commit :: String -> ClientM Bool
abort :: String -> ClientM Bool
readyCommit :: Message -> ClientM Bool
confirmCommit :: Message -> ClientM Bool


-- | The following provides the implementations of these types
-- Note that the order of the functions must match the endpoints in the type API from UseHaskell.hs

(signUp :<|> logIn :<|> loadPublicKey) = client restAuthAPI
(lock :<|> unlock :<|> locked) = client restLockAPI
(download :<|> upload :<|> updateShadowDB :<|> pushTransaction :<|> replicateFile) = client restFileAPI
(lsDir :<|> lsFile :<|> fileQuery :<|> mapFile :<|> dirShadowing) = client restDirAPI
(beginTransaction :<|> tUpload :<|> commit :<|> abort :<|> readyCommit :<|> confirmCommit) = client restTransAPI
