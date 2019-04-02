{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module NuxeoAudit.Types
  ( OptPgCnx(..)
  , OptDestination(..)
  , CmdPurge(..)
  , CmdExport(..)
  , Cmd(..)
  , AuditFile(..)
  , Audit(..)
  , FileType(..)
  )
where

import           Data.Aeson
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC8
import           Data.Csv                       ( ToRecord
                                                , ToField
                                                , toField
                                                )
import           Data.Int
import           Data.String
import qualified Data.Text                     as T
import           Data.Time.Calendar             ( Day )
import           Data.Time.LocalTime
import           Data.Word                      ( Word16 )
import           GHC.Generics

type OptPurgeDay = Integer
type Instance = String

data FileType = JSON | CSV deriving (Read, Eq)

instance Show FileType where
  show JSON = ".json"
  show CSV = ".csv"

data OptDestination = OptDestFile FilePath | OptDestStdOut deriving (Show, Read, Eq)
data OptPurge = OptPurge OptPurgeDay FilePath deriving (Show, Read, Eq)

data CmdExport = CmdExport {
  cmdExportInstance :: Instance
  , cmdExportDaysAgo :: Integer
  , cmdExportPgCnx :: OptPgCnx
  , cmdExportFileType :: FileType
  , cmdExportDestination :: OptDestination
  } deriving (Eq, Show)

data CmdPurge = CmdPurge {
  cmdPurgeInstance :: Instance
  , cmdPurgeKeepDays :: Integer
  , cmdPurgeFilePath :: FilePath
  , cmdPurgeFileType :: FileType
  }  deriving (Eq, Show)

data Cmd = SubCmdExport CmdExport | SubCmdPurge CmdPurge deriving (Eq, Show)

instance IsString CmdPurge where
  fromString = pure "CmdPurge"

instance IsString CmdExport where
  fromString = pure "CmdExport"

data OptPgCnx = OptPgCnx {
  optPgLogin :: BS.ByteString
  , optPgPassword :: BS.ByteString
  , optPgName :: BS.ByteString
  , optPgHost :: BS.ByteString
  , optPgPort :: Word16
  } deriving (Show, Read, Eq)

instance IsString OptDestination where
  fromString = OptDestFile

data AuditFile = AuditFile {
  afDay :: Day
  , afName :: FilePath
  , afPath :: FilePath } deriving Show

data Audit = Audit {
  auditId :: Int64
  , auditDthr :: LocalTime
  , auditEventDate :: LocalTime
  , auditEventId :: T.Text
  , auditEventCategory :: Maybe T.Text
  , auditDocLifeCycle :: Maybe T.Text
  , auditDocPath :: Maybe T.Text
  , auditDocUuid :: Maybe T.Text
  , auditPrincipalName :: T.Text
  } deriving (Show, Generic)

instance ToRecord Audit

instance ToField LocalTime where
  toField (LocalTime d _) = BSC8.pack (show d)

instance ToJSON Audit where
  toJSON (Audit
          id
          dthr
          eventdate
          eventid
          eventcategory
          doclifecyle
          docpath
          docuuid
          pname) = object ["id" .= id
                          , "dthr" .= dthr
                          , "eventdate" .= eventdate
                          , "eventid" .= eventid
                          , "eventcategory" .= eventcategory
                          , "doclifecyle" .= doclifecyle
                          , "docpath" .= docpath
                          , "docuuid" .= docuuid
                          , "login" .= pname]
