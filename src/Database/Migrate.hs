{-# LANGUAGE OverloadedStrings #-}
module Database.Migrate where

import           Control.Applicative
import           Control.Monad              (when)
import           Control.Monad.Reader
import           Control.Monad.Trans        (lift, liftIO)
import           Data.Maybe                 (fromJust, fromMaybe)
import           Data.String                (fromString)
import           Database.PostgreSQL.Simple
import           System.Posix.Env           (getEnv)

-- Helpers for running snaplets
import           Data.Map                   as M (empty)
import           Snap.Snaplet               (Handler, SnapletInit)
import           Snap.Snaplet.Test          (runHandler)
import           Snap.Test                  (get)

data Env = Env { unConn :: Connection, unAppEnv :: String, unTable :: String }

type MigrateT m a = ReaderT Env m a

-- NOTE(dbp 2014-05-28): We're using environment variables to configure libpq.
getEnvSettings :: IO Env
getEnvSettings = do conn <- connectPostgreSQL ""
                    t <- fromMaybe "migrations" <$> getEnv "PGTABLE"
                    e <- fromMaybe "devel" <$> getEnv "MIGRATION_APPENV"
                    return (Env conn e t)

runMain :: MigrateT IO () -> IO ()
runMain act = do e <- getEnvSettings
                 runReaderT act e

runMainSnap :: SnapletInit b b -> MigrateT (Handler b b) () -> IO ()
runMainSnap app act = do e <- getEnvSettings
                         runH (runReaderT act e) (unAppEnv e)
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
          (Env c appenv t) <- ask
          if s then
             do void $ liftIO $ execute c (fromString $ "INSERT INTO " ++
                                           t ++ " (name) values (?)") (Only nm)
                liftIO $ putStrLn $ "Applied migration in " ++ appenv ++ ": " ++ nm
            else liftIO $ putStrLn $ "Applying migration in " ++ appenv ++ "failed: " ++ nm


down :: (Functor m, MonadIO m) => m Bool -> MigrateT m ()
down migration =
  do downMode <- liftIO $ (== Just "down") <$> getEnv "MIGRATION_MODE"
     nm <- liftIO $ fromJust <$> getEnv "MIGRATION_NAME"
     when downMode $
       do s <- lift migration
          (Env c appenv t) <- ask
          if s then
             do void $ liftIO $ execute c (fromString $ "DELETE FROM " ++
                                           t ++ " WHERE name = ?") (Only nm)
                liftIO $ putStrLn $ "Reverted migration in " ++ appenv ++ ": " ++ nm
            else liftIO $ putStrLn $ "Reverting migration in " ++ appenv ++ " failed: " ++ nm


upSql :: (Functor m, MonadIO m) => String -> MigrateT m ()
upSql sql = do upMode <- liftIO $ (== Just "up") <$> getEnv "MIGRATION_MODE"
               nm <- liftIO $ fromJust <$> getEnv "MIGRATION_NAME"
               when upMode $
                 do (Env c appenv t) <- ask
                    liftIO $ execute_ c (fromString sql)
                    void $ liftIO $ execute c (fromString $ "INSERT INTO " ++
                                               t ++ " (name) values (?)") (Only nm)
                    liftIO $ putStrLn $ "Applied migration in " ++ appenv ++ ": " ++ nm


downSql :: (Functor m, MonadIO m) => String -> MigrateT m ()
downSql sql = do downMode <- liftIO $ (== Just "down") <$> getEnv "MIGRATION_MODE"
                 nm <- liftIO $ fromJust <$> getEnv "MIGRATION_NAME"
                 when downMode $
                   do (Env c appenv t) <- ask
                      liftIO $ execute_ c (fromString sql)
                      void $ liftIO $ execute c (fromString $ "DELETE FROM " ++
                                                 t ++ " WHERE name = ?") (Only nm)
                      liftIO $ putStrLn $ "Reverted migration in " ++ appenv ++ ": " ++ nm
