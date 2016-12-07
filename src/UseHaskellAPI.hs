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

-- generic message
data Message = Message { name    :: String
                       , message :: String
                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

-- auth stuff
data Login = Login { userName :: String
                   , password :: String
                   } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

-- locking stuff
data Lock = Lock { fName :: String
                 , state :: Bool
                 }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

deriving instance FromBSON Bool
deriving instance ToBSON   Bool

-- directory stuff
data FileRef = FileRef { fID :: String
                       , fServerIP :: String
                       , fServerPort :: String
                       }deriving (Show, Generic, ToJSON, FromJSON, ToBSON, FromBSON)

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
      :<|> "logIn"                      :> ReqBody '[JSON] Login  :> Post '[JSON] [ResponseData]
      :<|> "signUp"                     :> ReqBody '[JSON] Login  :> Post '[JSON] Bool
      :<|> "searchMessage"              :> QueryParam "name" String :> Get '[JSON] [Message]
      :<|> "performRESTCall"            :> QueryParam "filter" String  :> Get '[JSON] ResponseData

type LockAPI = "lock"                   :> ReqBody '[JSON] String :> Post '[JSON] Bool
          :<|> "unlock"                 :> ReqBody '[JSON] String :> Post '[JSON] Bool
          :<|> "locked"                 :> QueryParam "fName" String :> Get '[JSON] Bool

-- currently using Message type for files... (NOT TESTED!)
type FileAPI = "download"               :> QueryParam "name" String :> Get '[JSON] [Message]
          :<|> "upload"                 :> ReqBody '[JSON] Message  :> Post '[JSON] Bool

type DirAPI = "fileQuery"               :> QueryParam "name" String :> Get '[JSON] [Message]
