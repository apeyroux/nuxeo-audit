{-# LANGUAGE OverloadedStrings #-}

module Main where

import           NuxeoAudit.OptArgs
import           NuxeoAudit.Types

import           Control.Concurrent
import           Data.Aeson.Text
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy.Char8    as LBSC8
import qualified Data.Csv                      as Cassava
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

auditRow :: Decoders.Row Audit
auditRow =
  Audit
    <$> Decoders.column Decoders.int8
    <*> Decoders.column Decoders.timestamp
    <*> Decoders.column Decoders.timestamp
    <*> Decoders.column Decoders.text
    <*> Decoders.nullableColumn Decoders.text
    <*> Decoders.nullableColumn Decoders.text
    <*> Decoders.nullableColumn Decoders.text
    <*> Decoders.nullableColumn Decoders.text
    <*> Decoders.column Decoders.text

selectAuditLogStatement :: Statement.Statement DiffTime [Audit]
selectAuditLogStatement = Statement.Statement sql encoder decoder True
 where
  sql
    = "select log_id, \
         \ log_date, \
         \ log_event_date, \
         \ log_event_id, \
         \ log_event_category, \
         \ log_doc_life_cycle, \
         \ log_doc_path, \
         \ log_doc_uuid, \
         \ log_principal_name \
         \ from nxp_logs where log_date >= now() - $1 order by log_date"
  encoder = Encoders.param Encoders.interval
  decoder = Decoders.rowList auditRow

fpath :: FilePath -> FileType -> String -> Integer -> IO String
fpath basePath fileType instanceName daysAgo = fname instanceName daysAgo
  >>= \f -> pure $ basePath System.FilePath.Posix.</> f
 where
  fname :: String -> Integer -> IO FilePath
  fname instanceName daysAgo = do
    now <- nowDay
    pure
      $  instanceName
      <> "-"
      <> formatTime
           defaultTimeLocale
           "%d-%m-%Y"
           (foldr (.) id (replicate (fromIntegral daysAgo :: Int) pred) now)
      <> show fileType

nowDay :: IO Day
nowDay =
  (formatTime defaultTimeLocale "%d-%m-%Y" <$> getCurrentTime)
    >>= parseTimeM True defaultTimeLocale "%d-%m-%Y"

exportTo :: OptPgCnx -> FileType -> Integer -> String -> OptDestination -> IO ()
exportTo db fileType daysAgo instanceName dest = do
  connection <- Connection.acquire $ Connection.settings (optPgHost db)
                                                         (optPgPort db)
                                                         (optPgLogin db)
                                                         (optPgPassword db)
                                                         (optPgName db)
  -- Ecriture du json de Day -1
  case connection of
    Right cnx -> do
      n      <- getCurrentTime
      result <- Session.run
        (Session.statement
          (secondsToDiffTime (daysAgo * 86400) + utctDayTime n)
          selectAuditLogStatement
        )
        cnx
      case result of
        Right auditLogs -> case dest of
          OptDestFile d -> do
            absDest <- makeAbsolute d
            fp      <- fpath absDest fileType instanceName daysAgo
            createDirectoryIfMissing True d
            putStrLn $ "Ecriture des logs dans " <> fp
            case fileType of
              JSON -> TLIO.writeFile fp (encodeToLazyText auditLogs)
              CSV  -> LBSC8.writeFile fp (Cassava.encode auditLogs)
          OptDestStdOut -> case fileType of
            JSON -> TLIO.putStrLn $ encodeToLazyText auditLogs
            CSV  -> LBSC8.putStrLn $ Cassava.encode auditLogs
        Left err -> print err
    Left err -> print err

main :: IO ()
main = do
  opts <- execParser $ info (parseCmd <**> helper) Options.Applicative.fullDesc
  nowd <- nowDay

  case opts of
    --
    -- purge des anciens fichiers
    -- 
    SubCmdPurge purge -> do
      putStrLn "\n=== Purge des anciens logs ===\n"
      let basePath = cmdPurgeFilePath purge
      ld <- listDirectory (cmdPurgeFilePath purge)
      let
        lf =
          filter
              (\d ->
                diffDays nowd (afDay $ fromJust d) >= cmdPurgeKeepDays purge
              )
            $ filter isJust
            $ fmap
                (\f -> ptime (cmdPurgeInstance purge)
                             (takeFileName f)
                             basePath
                             (cmdPurgeFileType purge)
                )
                ld
      mapM_ (\x -> putStrLn $ "- delete " <> afName (fromJust x)) lf
    --
    -- export des fichiers
    --
    SubCmdExport export -> do
      let db = cmdExportPgCnx export
      exportTo db
               (cmdExportFileType export)
               (cmdExportDaysAgo export)
               (cmdExportInstance export)
               (cmdExportDestination export)
 where
  ptime :: String -> FilePath -> FilePath -> FileType -> Maybe AuditFile
  ptime instanceName fname fpath ftype =
    AuditFile
      <$> (parseTimeM True
                      defaultTimeLocale
                      (instanceName <> "-%d-%m-%Y" <> show ftype)
                      fname :: Maybe Day
          )
      <*> Just fname
      <*> Just fpath
