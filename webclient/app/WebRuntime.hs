module WebRuntime where

import Miso
import Miso.String

import Literal
import Runtime

import Data.List
import Control.Monad.Trans.State.Lazy (runStateT)

-------------------------------------------------------------------------
-- | A PolyRpc Web applicaion runtime system
-------------------------------------------------------------------------
-- This module depends on the design and implementation of HTML,
-- events, and MVC strcuture as demonstraed in <todomvc.rl>
-------------------------------------------------------------------------

-- | Page

-- Page is: init x view x update x mount point (query selector e.g., #id)

-- data Page = [a e].
--      Page a
--           (a -client-> Html [e])
--           (e -client-> a -client-> a)
--           String
--

-- | HTML and Attr

-- data Html =
--     [a]. Element String (List [Attr [a]]) (List [Html [a]])
--   | Txt String
--
-- data Attr =
--     [a]. Property String String
--   | Attribute String String
--   | EventBind String a
--   | KeyBind Int a
--   | ValueBind String (String -client-> a)

-------------------------------------------------------------------------

mainName = "main"

data Model = Model { progModel :: Value, progState :: RuntimeState }

instance Eq Model where
  (Model a (b, _, _)) == (Model c (d, _, _)) = a == c && b == d

data Action =
    Execute Value
  | SetResult (Value, RuntimeState)   -- a new program model
  | NoOp

pageWebApp :: RuntimeFunctionMap -> Value -> RuntimeState
    -> (Model, Model -> View Action, Action -> Model -> Effect Action Model, MisoString)
pageWebApp runtimeFunMap
  (Constr "Page" [ initModelValue, viewFunValue, updateFunValue, mount_pointValue ])
  state =  ( initModel, view, update, miso_mount_point )
    
  where
    initModel = Model { progModel=initModelValue, progState=state }
    
    view model = do
      error $ "Not supported yet"  -- Todo: Fix it!
      
      -- htmlValue <- apply runtimeFunMap viewFunValue model 
      -- return fromHtmlToView htmlValue

    ----------------------------------------------------------------------------
    -- | update
    ----------------------------------------------------------------------------
    update :: Action -> Model -> Effect Action Model
    
    update (Execute argActionValue) model = model <# do
      SetResult <$> doExecute runtimeFunMap model updateFunValue argActionValue
      
    update (SetResult (_progModel, _progState)) model =
      noEff Model { progModel=_progModel, progState=_progState }
    ----------------------------------------------------------------------------

    miso_mount_point = toMiso_String mount_pointValue

pageWebApp runtimeFunMap pageValue state =
  error $ "[pageWebApp] Unexpected page structure: " ++ show pageValue


doExecute :: RuntimeFunctionMap -> Model -> Value -> Value -> IO (Value, RuntimeState)
doExecute runtimeFunMap model updateValue argActionValue = do
  runStateT (do 
     updateAction <- apply runtimeFunMap updateValue argActionValue
     newModel <- apply runtimeFunMap updateAction (progModel model)
     return newModel) (progState model)
  

toMiso_String (Lit (StrLit str)) = ms str
toMiso_String v = error $ "[WebRuntime: toMiso_String] Expected string literal but met: " ++ show v

fromHtmlToView :: Value -> View Value  -- Value=HTML [Msg], View Value=View Action
fromHtmlToView htmlValue = error $ "[WebRuntime:fromHtmlToView] Not supported yet" -- Todo: Fix it!
