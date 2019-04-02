{-# LANGUAGE OverloadedStrings #-}

module NuxeoAudit.OptArgs
  ( parseCmd
  )
where

import           NuxeoAudit.Types

import           Control.Concurrent
import           Data.Aeson.Text
import qualified Data.ByteString               as BS
import           Data.Maybe
import qualified Data.Text.Lazy.IO             as TLIO
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Word                      ( Word16 )
import qualified Hasql.Connection              as Connection
import qualified Hasql.Decoders                as Decoders
import qualified Hasql.Encoders                as Encoders
import qualified Hasql.Session                 as Session
import qualified Hasql.Statement               as Statement
import           Options.Applicative
import           Options.Applicative.Help
import           System.Directory
import           System.FilePath.Posix

parseSubCmdExport :: Parser Cmd
parseSubCmdExport = SubCmdExport <$> parseCmdExport

parseSubCmdPurge :: Parser Cmd
parseSubCmdPurge = SubCmdPurge <$> parseCmdPurge

parseCmdExport :: Parser CmdExport
parseCmdExport =
  CmdExport
    <$> strOption
          (long "instance" <> metavar "NAME" <> help "Nom de l'instance")
    <*> option
          auto
          (  long "days-ago"
          <> metavar "INT"
          <> value 1
          <> help "Exporter les logs de n jours."
          <> showDefault
          )
    <*> parseOptPgCnx
    <*> (flag' JSON (long "json") <|> flag' CSV (long "csv"))
    <*> parseOptDestination

parseCmdPurge :: Parser CmdPurge
parseCmdPurge =
  CmdPurge
    <$> strOption
          (long "instance" <> metavar "NAME" <> help "Nom de l'instance")
    <*> option
          auto
          (  long "keep-days"
          <> short 'k'
          <> metavar "INT"
          <> value 7
          <> help "Purge les fichiers plus vieux de n jours."
          <> showDefault
          )
    <*> parseOptFilePath
    <*> (flag' JSON (long "json") <|> flag' CSV (long "csv"))

parseCmd :: Parser Cmd
parseCmd = subparser
  (  command
      "export"
      (info (parseSubCmdExport <**> helper) (progDesc "Export audit logs"))
  <> command
       "purge"
       (info (parseSubCmdPurge <**> helper) (progDesc "Purge logs"))
  )

parseOptPgCnx :: Parser OptPgCnx
parseOptPgCnx =
  OptPgCnx
    <$> strOption (long "login" <> value "nuxeodb")
    <*> strOption
          (long "password" <> value "nuxeodb" <> showDefault <> style dullred)
    <*> strOption (long "db" <> value "nuxeodb")
    <*> strOption (long "host" <> value "localhost")
    <*> option auto (long "port" <> value 5432)

parseOptFilePath :: Parser FilePath
parseOptFilePath = strOption
  (long "destination" <> short 'd' <> metavar "PATH" <> help
    "Répertoire de déstination"
  )

parseOptDestination :: Parser OptDestination
parseOptDestination =
  strOption (long "destination" <> short 'd' <> metavar "PATH")
    <|> flag' OptDestStdOut (long "stdout")
