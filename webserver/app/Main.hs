{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Location
import qualified CSExpr as TE
import CodeGen
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
    let send v = writeChan chan v :: IO ()
    let receive = readChan chan :: IO R.Value

    server_thread_id <- fork (evalStateT (R.loop_server funMap) (R.initMem, send, receive) )
    
    scottyT 3000 id $ scottyapp appName chan

-- | Initialize and run app

initialize :: String -> IO R.FunctionMap
initialize appName = do
    cs_funStore <- R.load_funstore $ "../csprog/" ++ appName ++ "_server.cs"
    let funStore = cgFunMap clientLocName cs_funStore
    putStrLn $ show (Data.List.length funStore) ++ " functions are loaded..."
    return funStore

scottyapp :: String -> Chan R.Value -> ScottyT Text IO ()
scottyapp appName chan = do
    middleware simpleCors
    post (capture $ "/" ++ appName) $ action chan

action :: Chan R.Value -> ActionT Text IO ()
action chan = do
    input_value <- (jsonData :: ActionT Text IO R.Value)

    writeChan chan input_value
    output_value <- readChan chan 

    json $ output_value

receive :: ActionT Text IO R.Value
receive = jsonData

send :: R.Value -> ActionT Text IO ()
send v = json v

-- | Utility

fromProgArgs :: IO String
fromProgArgs = do
    args <- getArgs
    case args of
      [appName] -> return appName
      _ -> do putStrLn $ "No app name or multiple app names: " ++ show args
              exitWith $ ExitFailure (-1)              
