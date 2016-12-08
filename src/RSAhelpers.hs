{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RSAhelpers where

import           GHC.Generics
import           Codec.Crypto.RSA
import           Crypto.Hash.SHA256 (hash)
import qualified System.Random  as R
import qualified Data.ByteString.Char8        as BS
import           Crypto.Random.DRBG
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic

-- types
data PubKeyInfo = PubKeyInfo { key_size :: String
                             , key_n :: String
                             , key_e :: String
                             } deriving (Show, Eq, Generic, FromJSON, ToBSON, FromBSON, ToJSON)

data PrivKeyInfo = PrivKeyInfo { prv_pub :: PubKeyInfo
                               , prv_d :: String
                               , prv_p :: String
                               , prv_q :: String
                               , prv_dP :: String
                               , prv_dQ :: String
                               , prv_qinv :: String
                               } deriving (Show, Eq,Generic, FromJSON, ToBSON, FromBSON, ToJSON)
data Keys = Keys { owner :: String
                 , pubickey :: PubKeyInfo
                 , privateKey :: PrivKeyInfo
                 } deriving (Show, Eq,Generic, FromJSON, ToBSON, FromBSON, ToJSON)

deriving instance FromBSON String  -- we need these as BSON does not provide
deriving instance ToBSON   String

-- functions
fromPublicKey :: PublicKey -> PubKeyInfo
fromPublicKey msg@(PublicKey key_size n e) =   PubKeyInfo strKey strN strE
  where
      strKey =  show key_size
      strN =  show n
      strE=  show e

toPublicKey :: PubKeyInfo -> PublicKey
toPublicKey msg@(PubKeyInfo strKey strN strE) =    PublicKey key_size n e
    where
        key_size = read strKey :: Int  ----  not sure about this
        n = read strN :: Integer
        e= read strE :: Integer


fromPrivateKey :: PrivateKey -> PrivKeyInfo
fromPrivateKey msg@(PrivateKey private_pub private_d private_p private_q private_dP private_dQ private_qinv) =  PrivKeyInfo strPub strPrvD strPrvP strPrvQ strPrvDP strPrvDQ strQINV
  where
      strPub  =  fromPublicKey private_pub
      strPrvD =  show private_d
      strPrvP =  show private_p
      strPrvQ =  show private_q
      strPrvDP=  show private_dP
      strPrvDQ=  show private_dQ
      strQINV =  show private_qinv

toPrivateKey ::  PrivKeyInfo -> PrivateKey
toPrivateKey msg@(PrivKeyInfo  strPub strPrvD strPrvP strPrvQ strPrvDP strPrvDQ strQINV) =  PrivateKey   private_pub private_d private_p private_q private_dP private_dQ private_qinv
  where
      private_pub  =  toPublicKey strPub
      private_d    =  read strPrvD :: Integer
      private_p    =  read strPrvP :: Integer
      private_q    =  read strPrvQ :: Integer
      private_dP   =  read strPrvDP :: Integer
      private_dQ   =  read strPrvDQ :: Integer
      private_qinv =  read strQINV :: Integer
