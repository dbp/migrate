{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Main where

import           Prelude                    hiding (lookup)

-- Base types
import           Control.Applicative
import           Control.Monad              (filterM, mapM_, void)
import           Data.List                  (isSuffixOf, sort)
import           Data.Maybe                 (fromJust)
import           Data.String                (fromString)

-- Commandline processing
import           System.Environment         (getArgs)
import           System.Posix.Env           (getEnv)

-- Config file parsing
import           Data.Configurator          (Worth (Required), autoConfig,
                                             autoReload, lookup, lookupDefault,
                                             require)

-- Directory manipulations
import           System.Directory           (doesFileExist,
                                             getDirectoryContents)

-- Running shell commands
import           System.Process             (CreateProcess (..), createProcess,
                                             shell, waitForProcess)

-- Database connections
import           Database.PostgreSQL.Simple

-- Library code
import           Database.Migrate

data DBType = PG

main :: IO ()
main = do args <- getArgs
          let appenv = case length args of
                         2 -> head (tail args)
                         1 -> "devel"
                         _ -> error "usage: migrate [up|down] (devel|test|otherenv)"
          (cfg, _) <- autoReload autoConfig [Required (appenv ++ ".cfg")]
          dir <- lookupDefault "migrations" cfg "migrate-directory"
          table <- lookupDefault "migrations" cfg "migrate-table"
          dbtype <- lookupDefault "postgresql" cfg "migrate-database-type" :: IO String
          ghcargs <- lookupDefault "" cfg "migrate-ghc-args"
          (conn, typ, env) <- case dbtype of
                              "postgresql" ->
                                do dbuser <- require cfg "migrate-database-user"
                                   dbpass <- require cfg "migrate-database-password"
                                   dbname <- require cfg "migrate-database-name"
                                   dbhost <- lookup cfg "migrate-database-host"
                                   dbport <- lookup cfg "migrate-database-port"
                                   c <- connectPostgreSQL (fromString $ "user='" ++ dbuser ++ "' password='" ++ dbpass ++ "' dbname='" ++ dbname ++ "'" ++ maybe "" (\h -> " host='" ++ h ++ "'") dbhost ++ maybe "" (\p -> " port='" ++ p ++ "'") dbport)
                                   execute_ c (fromString createTable)
                                   -- NOTE(dbp 2014-05-28): To appease cabal, so we can use sandboxes.
                                   home <- fromJust <$> getEnv "HOME"
                                   path <- fromJust <$> getEnv "PATH"
                                   let env = [("PGDATABASE", dbname)
                                             ,("PGUSER", dbuser)
                                             ,("PGPASSWORD", dbpass)
                                             ,("PGTABLE", table)
                                             ,("HOME", home)
                                             ,("PATH", path)] ++
                                             maybe [] ((:[]) . ("PGHOST", )) dbhost ++
                                             maybe [] ((:[]) . ("PGPORT", ) . show) dbport
                                   return (c, PG, env)
                                  where createTable = "CREATE TABLE IF NOT EXISTS " ++ table ++
                                                      " (name text NOT NULL PRIMARY KEY)"
          migrations <- sort . map stripSuffix . filter isCode <$> getDirectoryContents dir
          let mode = head args
          case mode of
            "up" -> do
              missing <- filterM (notExists typ table conn) migrations
              if null missing
                 then putStrLn "No migrations to run."
                 else mapM_ (runMigration "up" appenv ghcargs env dir) missing
            "down" -> do
              toDown <- dropWhileM (notExists typ table conn) $ reverse migrations
              case toDown of
                (x:xs) -> runMigration "down" appenv ghcargs env dir x
                _ -> putStrLn "No migrations remaining."
            "status" -> mapM_ (\m -> do ne <- notExists typ table conn m
                                        if ne
                                           then putStrLn $ m ++ " in " ++ appenv
                                           else putStrLn $ " APPLIED " ++ m ++ " in " ++ appenv)
                              migrations
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

runMigration :: String -> String -> String -> [(String,String)] -> FilePath -> String -> IO ()
runMigration mode appenv ghcargs env dir p =
  do -- NOTE(dbp 2014-06-04): If a binary exists, just run it. Else, runghc the source.
     exists <- doesFileExist (dir ++ "/" ++ p)
     let command = if exists
                      then "./" ++ dir ++ "/" ++ p
                      else "cabal exec runghc -- " ++
                           ghcargs ++ " " ++ dir ++ "/" ++ p ++ ".hs"
     (_,_,_,h) <- createProcess $ (shell command)
                                  { env = Just (env ++ [("MIGRATION_NAME", p)
                                                       ,("MIGRATION_APPENV", appenv)
                                                       ,("MIGRATION_MODE", mode)])}
     void $ waitForProcess h
