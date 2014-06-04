{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Base types
import Control.Applicative
import Control.Monad (mapM_, filterM, void)
import Data.List (sort, isSuffixOf)
import Data.String (fromString)
import Data.Maybe (fromJust)

-- Commandline processing
import System.Environment (getArgs)
import System.Posix.Env (getEnv)

-- Config file parsing
import Data.Configurator (autoReload, autoConfig, Worth (Required), lookupDefault, require)

-- Directory manipulations
import System.Directory (getDirectoryContents, doesFileExist)

-- Running shell commands
import System.Process (createProcess, shell, waitForProcess, CreateProcess(..))

-- Database connections
import Database.PostgreSQL.Simple

-- Library code
import Database.Migrate

data DBType = PG

main :: IO ()
main = do (cfg, _) <- autoReload autoConfig [Required "devel.cfg"]
          dir <- lookupDefault "migrations" cfg "migrate-directory"
          table <- lookupDefault "migrations" cfg "migrate-table"
          dbtype <- lookupDefault "postgresql" cfg "migrate-database-type" :: IO String
          ghcargs <- lookupDefault "" cfg "migrate-ghc-args"
          (conn, typ, env) <- case dbtype of
                              "postgresql" ->
                                do dbuser <- require cfg "migrate-database-user"
                                   dbpass <- require cfg "migrate-database-password"
                                   dbname <- require cfg "migrate-database-name"
                                   dbhost <- lookupDefault "127.0.0.1" cfg "migrate-database-host"
                                   dbport <- lookupDefault 5432 cfg "migrate-database-port"
                                   c <- connect (ConnectInfo dbhost dbport dbuser dbpass dbname)
                                   execute_ c (fromString createTable)
                                   -- NOTE(dbp 2014-05-28): To appease cabal, so we can use sandboxes.
                                   home <- fromJust <$> getEnv "HOME"
                                   path <- fromJust <$> getEnv "PATH"
                                   let env = [("PGHOST", dbhost)
                                             ,("PGPORT", show dbport)
                                             ,("PGDATABASE", dbname)
                                             ,("PGUSER", dbuser)
                                             ,("PGPASSWORD", dbpass)
                                             ,("PGTABLE", table)
                                             ,("HOME", home)
                                             ,("PATH", path)]
                                   return (c, PG, env)
                                  where createTable = "CREATE TABLE IF NOT EXISTS " ++ table ++
                                                      " (name text NOT NULL PRIMARY KEY)"
          migrations <- sort . map stripSuffix . filter isCode <$> getDirectoryContents dir
          args <- getArgs
          case length args of
            1 -> do let mode = head args
                    case mode of
                      "up" -> do
                        missing <- filterM (notExists typ table conn) migrations
                        if null missing
                           then putStrLn "No migrations to run."
                           else mapM_ (runMigration "up" ghcargs env dir) missing
                      "down" -> do
                        toDown <- dropWhileM (notExists typ table conn) $ reverse migrations
                        case toDown of
                          (x:xs) -> runMigration "down" ghcargs env dir x
                          _ -> putStrLn "No migrations remaining."
            _ -> putStrLn "usage: migrate [up|down]"

  where stripSuffix = reverse . drop 3 . reverse
        isCode = isSuffixOf ".hs"
        notExists PG tb c m =
          null <$> getMigration tb c m
        getMigration :: String -> Connection -> String -> IO [(Only String)]
        getMigration tb c m = query c (fromString $ "SELECT name FROM " ++ tb ++ " WHERE name = ?") (Only m)
        dropWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
        dropWhileM f [] = return []
        dropWhileM f (x:xs) = do r <- f x
                                 if r
                                    then dropWhileM f xs
                                    else return (x:xs)

runMigration :: String -> String -> [(String,String)] -> FilePath -> String -> IO ()
runMigration mode ghcargs env dir p =
  do home <- fromJust <$> getEnv "HOME"
     -- NOTE(dbp 2014-06-04): If a binary exists, just run it. Else, runghc the source.
     exists <- doesFileExist (dir ++ "/" ++ p)
     let command = if exists
                      then "./" ++ dir ++ "/" ++ p
                      else home ++ "/.cabal/bin/cabal exec runghc -- " ++
                           ghcargs ++ " " ++ dir ++ "/" ++ p ++ ".hs"
     (_,_,_,h) <- createProcess $ (shell command)
                                  { env = Just (env ++ [("MIGRATION_NAME", p)
                                                       ,("MIGRATION_MODE", mode)])}
     void $ waitForProcess h
