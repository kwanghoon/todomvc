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
import Todomvc_main
import Todomvc_client

import Control.Monad.Trans.State.Lazy (runStateT)
import Location
import qualified Runtime as R
import qualified WebRuntime as WR


-- | Model
-- type Model = Value

-- data Model
--   = Model
--   { info :: Maybe APIInfo
--   } deriving (Eq, Show)

-- | Action
-- type Action = Value

-- data Action
--   = FetchGitHub
--   | SetGitHub APIInfo
--   | NoOp
--   deriving (Show, Eq)

-- | Main entry point
main :: IO ()
main = do
  appName <- fromProgArgs
  putStrLn appName

  -- Todo: Fix this:
  --   - Reading a function map and the main expression
  --     from files are not supported yet by GHCJS.
  
  let funMap = todomvc_client
  let webMain = todomvc_main
  
  let initEnv = []

  let runtimeFunMap = R.interpFunMap clientLocName funMap
  
  (pageV, state) <- runStateT
      (R.interpExpr clientLocName webMain runtimeFunMap initEnv)
      (R.initMem, WR.send, WR.receive)

  let (initModel, viewModel, updateModel, whereMountPoint) = WR.pageWebApp runtimeFunMap pageV state

  startApp App { model = initModel
               , initialAction = WR.NoOp
               , mountPoint = Just whereMountPoint
               , update = updateModel :: WR.Action -> WR.Model -> Effect WR.Action WR.Model
               , view = viewModel :: WR.Model -> View WR.Action
               , ..
               }
    where
      events = defaultEvents
      subs   = []
      logLevel = Off

-- | Update your model
-- updateModel :: Action -> Model -> Effect Action Model
-- updateModel FetchGitHub m = m <# do
--   SetGitHub <$> getGitHubAPIInfo
-- updateModel (SetGitHub apiInfo) m =
--   noEff m { info = Just apiInfo }
-- updateModel NoOp m = noEff m

-- | View function, with routing
-- viewModel :: Model -> View Action
-- viewModel Model {..} = view
--   where
--     view = div_ [ style_ $ M.fromList [
--                   (pack "text-align", pack "center")
--                 , (pack "margin", pack "200px")
--                 ]
--                ] [
--         h1_ [class_ $ pack "title" ] [ text $ pack "Miso XHR Example" ]
--       , button_ attrs [
--           text $ pack "Fetch JSON from https://api.github.com via XHR"
--           ]
--       , case info of
--           Nothing -> div_ [] [ text $ pack "No data" ]
--           Just APIInfo{..} ->
--             table_ [ class_ $ pack "table is-striped" ] [
--               thead_ [] [
--                 tr_ [] [
--                   th_ [] [ text $ pack "URLs"]
--                 ]
--               ]
--             , tbody_ [] [
--                 tr_ [] [ td_ [] [ text current_user_url ] ]
--               , tr_ [] [ td_ [] [ text emojis_url ] ]
--               , tr_ [] [ td_ [] [ text emails_url ] ]
--               , tr_ [] [ td_ [] [ text events_url ] ]
--               , tr_ [] [ td_ [] [ text gists_url ] ]
--               , tr_ [] [ td_ [] [ text feeds_url ] ]
--               , tr_ [] [ td_ [] [ text followers_url ] ]
--               , tr_ [] [ td_ [] [ text following_url ] ]
--               ]
--             ]
--           ]
--       where
--         attrs = [ onClick FetchGitHub
--                 , class_ $ pack "button is-large is-outlined"
--                 ] ++ [ disabled_ True | isJust info ]

-- data APIInfo
--   = APIInfo
--   { current_user_url :: MisoString
--   , current_user_authorizations_html_url :: MisoString
--   , authorizations_url :: MisoString
--   , code_search_url :: MisoString
--   , commit_search_url :: MisoString
--   , emails_url :: MisoString
--   , emojis_url :: MisoString
--   , events_url :: MisoString
--   , feeds_url :: MisoString
--   , followers_url :: MisoString
--   , following_url :: MisoString
--   , gists_url :: MisoString
--   , hub_url :: MisoString
--   , issue_search_url :: MisoString
--   , issues_url :: MisoString
--   , keys_url :: MisoString
--   , notifications_url :: MisoString
--   , organization_repositories_url :: MisoString
--   , organization_url :: MisoString
--   , public_gists_url :: MisoString
--   , rate_limit_url :: MisoString
--   , repository_url :: MisoString
--   , repository_search_url :: MisoString
--   , current_user_repositories_url :: MisoString
--   , starred_url :: MisoString
--   , starred_gists_url :: MisoString
--   , user_url :: MisoString
--   , user_organizations_url :: MisoString
--   , user_repositories_url :: MisoString
--   , user_search_url :: MisoString
--   } deriving (Show, Eq, Generic)

-- instance FromJSON APIInfo where
--   parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo '_' }

-- getGitHubAPIInfo :: IO APIInfo
-- getGitHubAPIInfo = do
--   Just resp <- contents <$> xhrByteString req
--   case eitherDecodeStrict resp :: Either String APIInfo of
--     Left s -> error s
--     Right j -> pure j
--   where
--     req = Request { reqMethod = GET
-- --                  , reqURI = pack "https://api.github.com"
--                   , reqURI = pack "http://localhost:3000/api"
--                   , reqLogin = Nothing
--                   , reqHeaders = []
--                   , reqWithCredentials = False
--                   , reqData = NoData
--                   }

-- | Initialize the client cs program

-- | File reading is not supported by GHCJS!

-- initialize :: String -> IO R.FunctionMap
-- initialize appName = do
--     cs_funStore <- R.load_funstore $ "../prog/" ++ appName ++ "_server.r"
--     let funStore = cgFunMap clientLocName cs_funStore
--     putStrLn $ show (Data.List.length funStore) ++ " functions are loaded..."
--     return funStore

-- | Utility

fromProgArgs :: IO String
fromProgArgs = do
    args <- getArgs
    case args of
      [appName] -> return appName
      _ -> return "todomvc" -- ToDo: Fix this
           -- do putStrLn $ "No app name or multiple app names: " ++ show args
           --   exitWith $ ExitFailure (-1)              
