{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts     #-}

module UseHaskellAPITypes where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           GHC.Generics


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

-- | Generic four element message.
data Message4 = Message4 { val1    :: String
                         , val2    :: String
                         , val3  :: String
                         , val4   :: String
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
                                       , authTicket   :: String
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

deriving instance FromBSON String
deriving instance ToBSON   String

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
