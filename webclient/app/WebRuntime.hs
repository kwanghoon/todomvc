module WebRuntime where

import Miso
import Miso.String

import Literal
import Runtime

import Data.Aeson (eitherDecodeStrict, toJSON)
import Data.List
import Control.Monad.Trans.State.Lazy (runStateT)

import           JavaScript.Web.XMLHttpRequest

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

data Model = Model { progModel :: Value, progState :: RuntimeState, updatedView :: Maybe Value }

instance Eq Model where
  (Model a (b, _, _) c) == (Model d (e, _, _) f) = a == d && b == e && c == f

data Action =
    Execute Value
  | SetResult ((Value, Value), RuntimeState)   -- (a new program model, a new view), prog state
  | NoOp

pageWebApp :: RuntimeFunctionMap -> Value -> RuntimeState
    -> (Model, Model -> View Action, Action -> Model -> Effect Action Model, MisoString)
pageWebApp runtimeFunMap
  (Constr "Page" [ initModelValue, viewFunValue, updateFunValue, mount_pointValue ])
  state =  ( initModel, view, update, miso_mount_point )
    
  where
    initModel = Model { progModel=initModelValue, progState=state, updatedView = Nothing }

    view :: Model -> View Action
    view model = do
      error $ "Not supported yet"  -- Todo: Fix it!
      
      -- htmlValue <- apply runtimeFunMap viewFunValue model 
      -- return fromHtmlToView htmlValue

    ----------------------------------------------------------------------------
    -- | update
    ----------------------------------------------------------------------------
    update :: Action -> Model -> Effect Action Model
    
    update (Execute argActionValue) model = model <# do
      SetResult <$> doExecute runtimeFunMap model updateFunValue argActionValue viewFunValue
      
    update (SetResult ((_progModel, _updatedView), _progState)) model =
      noEff Model { progModel=_progModel, progState=_progState, updatedView=Just _updatedView }
    ----------------------------------------------------------------------------

    miso_mount_point = toMiso_String mount_pointValue

pageWebApp runtimeFunMap pageValue state =
  error $ "[pageWebApp] Unexpected page structure: " ++ show pageValue


-- | For update

doExecute :: RuntimeFunctionMap -> Model -> Value -> Value -> Value -> IO ((Value, Value), RuntimeState)
doExecute runtimeFunMap model updateValue argActionValue viewValue = do
  runStateT (do 
     updateAction <- apply runtimeFunMap updateValue argActionValue
     newModel <- apply runtimeFunMap updateAction (progModel model)
     
     newView <- apply runtimeFunMap viewValue newModel
     
     return (newModel, newView) ) (progState model)

webSend :: Mem -> Value -> IO Mem
webSend mem v = do
  Just resp <- contents <$> xhrByteString (req v)
  case eitherDecodeStrict resp :: Either String Value of
    Left s -> error s
    Right j -> return $ mem { _reg = Just j }
    -- set the resp in the program state for receive to be able to take later!!
  
  where
    req v = Request
      { reqMethod = POST
      , reqURI = pack "https://api.github.com" -- Todo: Fix this!
      , reqLogin = Nothing
      , reqHeaders = []
      , reqWithCredentials = False
      , reqData = StringData (pack $ show $ toJSON $ v)
      }

webReceive :: Mem -> IO (Value, Mem)
webReceive mem = do
  let receivedValue = _reg mem
  case receivedValue of
    Just v -> return ( v, mem { _reg = Nothing } )
    Nothing -> error $ "[WebRuntime:webReceive] it should be Just v but Nothing"

-- | For mount point

toMiso_String (Lit (StrLit str)) = ms str
toMiso_String v = error $ "[WebRuntime: toMiso_String] Expected string literal but met: " ++ show v

-- | For view

fromHtmlToView :: Value -> View Action  -- Value=HTML [Msg], View Value=View Action
fromHtmlToView htmlValue = error $ "[WebRuntime:fromHtmlToView] Not supported yet" -- Todo: Fix it!

