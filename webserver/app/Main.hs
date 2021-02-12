{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Location
import qualified Runtime as R

import Control.Concurrent.STM
import Control.Concurrent.MonadIO
import Control.Monad.Trans.State.Lazy (evalStateT)

import Data.List
import Data.Text.Lazy
import Data.Default.Class
-- import Web.Scotty
import Web.Scotty.Trans (scottyT, ScottyT, ActionT, middleware, post, capture, json, jsonData)
import Data.Monoid (mconcat)
import Data.Aeson hiding (json)
import GHC.Generics
import Control.Monad.IO.Class

import Network.Wai.Middleware.Cors

import System.Environment(getArgs)
import System.Exit

main :: IO ()
main = do
    appName <- fromProgArgs
    putStrLn appName
    funMap <- initialize appName

    chan <- newChan :: IO (Chan R.Value)
    let send mem v = do writeChan chan v :: IO ()
                        return mem
    let receive mem = do v <- readChan chan :: IO R.Value
                         return (v, mem)

    server_thread_id <-
      fork (evalStateT (R.loop_server funMap) (R.initMem, send, receive) )
    
    scottyT 3000 id $ scottyapp appName chan

-- | Initialize and run app

initialize :: String -> IO R.RuntimeFunctionMap
initialize appName = do
    r_funStore <- R.load_funstore $ "../prog/" ++ appName ++ "_server.r"
    let funStore = R.interpFunMap serverLocName r_funStore
    putStrLn $ show (Data.List.length funStore) ++ " functions are loaded..."
    return funStore

scottyapp :: String -> Chan R.Value -> ScottyT Text IO ()
scottyapp appName chan = do
    middleware simpleCors
    post (capture $ "/" ++ appName) $ action chan

action :: Chan R.Value -> ActionT Text IO ()
action chan = do
    input_value <- (jsonData :: ActionT Text IO R.Value)
    liftIO $ putStrLn $ "Received:"
    liftIO $ putStrLn $ show input_value

    writeChan chan input_value
    output_value <- readChan chan 

    json $ output_value

-- | Utility

fromProgArgs :: IO String
fromProgArgs = do
    args <- getArgs
    case args of
      [appName] -> return appName
      _ -> do putStrLn $ "No app name or multiple app names: " ++ show args
              exitWith $ ExitFailure (-1)              
