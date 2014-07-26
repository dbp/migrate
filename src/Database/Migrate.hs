{-# LANGUAGE OverloadedStrings #-}
module Database.Migrate where

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.Trans (lift, liftIO)
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromString)
import Database.PostgreSQL.Simple
import System.Posix.Env (getEnv)

-- Helpers for running snaplets
import Data.Map as M (empty)
import Snap.Snaplet (Handler, SnapletInit)
import Snap.Test (get)
import Snap.Snaplet.Test (runHandler)

data Env = Env { unConn :: Connection, unTable :: String }

type MigrateT m a = ReaderT Env m a

-- NOTE(dbp 2014-05-28): We're using environment variables to configure libpq.
getEnvSettings :: IO Env
getEnvSettings = do conn <- connectPostgreSQL ""
                    t <- fromMaybe "migrations" <$> getEnv "PGTABLE"
                    return (Env conn t)

runMain :: MigrateT IO () -> IO ()
runMain act = do e <- getEnvSettings
                 runReaderT act e

runMainSnap :: SnapletInit b b -> MigrateT (Handler b b) () -> IO ()
runMainSnap app act = do e <- getEnvSettings
                         runH (runReaderT act e) "devel"
  where runH h env = do r <- runHandler (Just env) (get "" M.empty) h app
                        case r of
                          Left err -> error (show err)
                          Right _ -> return ()


-- NOTE(dbp 2014-05-27): Need to know when things were successful, to mark as migrated.
up :: (Functor m, MonadIO m) => m Bool -> MigrateT m ()
up migration =
  do upMode <- liftIO $ (== Just "up") <$> getEnv "MIGRATION_MODE"
     nm <- liftIO $ fromJust <$> getEnv "MIGRATION_NAME"
     when upMode $
       do s <- lift migration
          if s then
             do (Env c t) <- ask
                void $ liftIO $ execute c (fromString $ "INSERT INTO " ++
                                           t ++ " (name) values (?)") (Only nm)
                liftIO $ putStrLn $ "Applied migration: " ++ nm
            else liftIO $ putStrLn $ "Applying migration failed: " ++ nm


down :: (Functor m, MonadIO m) => m Bool -> MigrateT m ()
down migration =
  do downMode <- liftIO $ (== Just "down") <$> getEnv "MIGRATION_MODE"
     nm <- liftIO $ fromJust <$> getEnv "MIGRATION_NAME"
     when downMode $
       do s <- lift migration
          if s then
             do (Env c t) <- ask
                void $ liftIO $ execute c (fromString $ "DELETE FROM " ++
                                           t ++ " WHERE name = ?") (Only nm)
                liftIO $ putStrLn $ "Reverted migration: " ++ nm
            else liftIO $ putStrLn $ "Reverting migration failed: " ++ nm


upSql :: (Functor m, MonadIO m) => String -> MigrateT m ()
upSql sql = do upMode <- liftIO $ (== Just "up") <$> getEnv "MIGRATION_MODE"
               nm <- liftIO $ fromJust <$> getEnv "MIGRATION_NAME"
               when upMode $
                 do (Env c t) <- ask
                    liftIO $ execute_ c (fromString sql)
                    void $ liftIO $ execute c (fromString $ "INSERT INTO " ++
                                               t ++ " (name) values (?)") (Only nm)
                    liftIO $ putStrLn $ "Applied migration: " ++ nm


downSql :: (Functor m, MonadIO m) => String -> MigrateT m ()
downSql sql = do downMode <- liftIO $ (== Just "down") <$> getEnv "MIGRATION_MODE"
                 nm <- liftIO $ fromJust <$> getEnv "MIGRATION_NAME"
                 when downMode $
                   do (Env c t) <- ask
                      liftIO $ execute_ c (fromString sql)
                      void $ liftIO $ execute c (fromString $ "DELETE FROM " ++
                                                 t ++ " WHERE name = ?") (Only nm)
                      liftIO $ putStrLn $ "Reverted migration: " ++ nm
