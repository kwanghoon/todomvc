{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib

import Data.Text.Lazy
import Web.Scotty
import Data.Monoid (mconcat)
import Data.Aeson hiding (json)
import GHC.Generics

import Network.Wai.Middleware.Cors

main :: IO ()
main = scotty 3000 $ do
    middleware simpleCors
    get "/api" $ do
        json $ apiInfo

apiInfo = APIInfo
  { current_user_url = "https://api.github.com/user"
  , current_user_authorizations_html_url = "https://github.com/settings/connections/applications{/client_id}"
  , authorizations_url = "https://api.github.com/authorizations"
  , code_search_url = "https://api.github.com/search/code?q={query}{&page,per_page,sort,order}"
  , commit_search_url = "https://api.github.com/search/commits?q={query}{&page,per_page,sort,order}"
  , emails_url = "https://api.github.com/user/emails"
  , emojis_url = "https://api.github.com/emojis"
  , events_url = "https://api.github.com/events"
  , feeds_url = "https://api.github.com/feeds"
  , followers_url = "https://api.github.com/user/followers"
  , following_url = "https://api.github.com/user/following{/target}"
  , gists_url = "https://api.github.com/gists{/gist_id}"
  , hub_url = "https://api.github.com/hub"
  , issue_search_url  = "https://api.github.com/search/issues?q={query}{&page,per_page,sort,order}"
  , issues_url = "https://api.github.com/issues"
  , keys_url = "https://api.github.com/user/keys"
  , notifications_url = "https://api.github.com/notifications"
  , organization_repositories_url = "https://api.github.com/orgs/{org}/repos{?type,page,per_page,sort}"
  , organization_url = "https://api.github.com/orgs/{org}"
  , public_gists_url = "https://api.github.com/gists/public"
  , rate_limit_url = "https://api.github.com/rate_limit"
  , repository_url = "https://api.github.com/repos/{owner}/{repo}"
  , repository_search_url = "https://api.github.com/search/repositories?q={query}{&page,per_page,sort,order}"
  , current_user_repositories_url = "https://api.github.com/user/repos{?type,page,per_page,sort}"
  , starred_url = "https://api.github.com/user/starred{/owner}{/repo}"
  , starred_gists_url = "https://api.github.com/gists/starred"
  , user_url = "https://api.github.com/users/{user}"
  , user_organizations_url = "https://api.github.com/user/orgs"
  , user_repositories_url = "https://api.github.com/users/{user}/repos{?type,page,per_page,sort}"
  , user_search_url = "https://api.github.com/search/users?q={query}{&page,per_page,sort,order}"
  }

data APIInfo
  = APIInfo
  { current_user_url :: String
  , current_user_authorizations_html_url :: String
  , authorizations_url :: String
  , code_search_url :: String
  , commit_search_url :: String
  , emails_url :: String
  , emojis_url :: String
  , events_url :: String
  , feeds_url :: String
  , followers_url :: String
  , following_url :: String
  , gists_url :: String
  , hub_url :: String
  , issue_search_url :: String
  , issues_url :: String
  , keys_url :: String
  , notifications_url :: String
  , organization_repositories_url :: String
  , organization_url :: String
  , public_gists_url :: String
  , rate_limit_url :: String
  , repository_url :: String
  , repository_search_url :: String
  , current_user_repositories_url :: String
  , starred_url :: String
  , starred_gists_url :: String
  , user_url :: String
  , user_organizations_url :: String
  , user_repositories_url :: String
  , user_search_url :: String
  } deriving (Show, Eq, Generic)

instance ToJSON APIInfo where
--  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo '_' }
