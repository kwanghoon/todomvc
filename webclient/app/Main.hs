{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import           GHC.Generics
import           JavaScript.Web.XMLHttpRequest

import           Miso                          hiding (defaultOptions)
import           Miso.String

import System.Environment(getArgs)
import System.Exit

--
import qualified WebApp as WebApp

import Control.Monad.Trans.State.Lazy (runStateT)
import Location
import qualified Runtime as R
import qualified WebRuntime as WR

-- | Main entry point
main :: IO ()
main = do
  let appName = WebApp.appName
  putStrLn appName

  let funMap = WebApp.funMap   
  let webMain = WebApp.webMain 
  
  let initEnv = []

  let runtimeFunMap = R.interpFunMap clientLocName funMap
  
  ((init, view, update, whereInHtml), state) <- runStateT
      (do pageV <- R.interpExpr clientLocName webMain runtimeFunMap initEnv
          WR.pageWebApp runtimeFunMap pageV)
      (R.initMem, WR.webSend appName, WR.webReceive)

  startApp App {
        model  = init
      , view   = view :: WR.Model -> View WR.Action
      , update = update :: WR.Action -> WR.Model -> Effect WR.Action WR.Model
      
      , initialAction = WR.NoOp
      , mountPoint    = Just whereInHtml -- Nothing
      , ..
      }
    
    where
      events = defaultEvents
      subs   = []
      logLevel = Off

