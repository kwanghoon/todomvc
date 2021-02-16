module WebRuntime where

import Miso
import Miso.String

import Literal
import Runtime

import Data.Aeson (eitherDecodeStrict, encode) -- toJSON)
import qualified Data.ByteString.Lazy as BSLazy(toStrict)
import qualified Data.ByteString as BS(unpack)
import Data.Bool
import Data.Char (chr)
import Data.List
import qualified Data.Map as M (singleton)
import Control.Monad.Trans.State.Lazy (runStateT, get)

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

-- data List = [a]. Nil | Cons a (List [a])

-------------------------------------------------------------------------

mainName = "main"

data Model = Model { progModel :: Value, progState :: RuntimeState, updatedView :: Maybe Value }

instance Eq Model where
  (Model a (b, _, _) c) == (Model d (e, _, _) f) = a == d && b == e && c == f

data Action =
    Execute Value [Value]  -- V [] for Action or V [W] for String -> Action
  | SetResult ((Value, Value), RuntimeState)   -- (a new program model, a new view), prog state
  | NoOp

pageWebApp :: RuntimeFunctionMap -> Value 
    -> RuntimeM ( Model
                , Model -> View Action
                , Action -> Model -> Effect Action Model
                , MisoString)
pageWebApp r_FunMap
  (UnitM (Constr "Page" [ initModelV, viewFunV, updateFunV, mount_pointV ] )) =  do
    UnitM htmlV <- apply r_FunMap viewFunV initModelV

    state <- get
    
    ----------------------------------------------------------------------------
    -- | Initial model
    ----------------------------------------------------------------------------
    let initModel =
         Model { progModel=initModelV
               , progState=state
               , updatedView = Just htmlV }


    return ( initModel, view, update, miso_mount_point )
    
  where
    ----------------------------------------------------------------------------
    -- | view
    ----------------------------------------------------------------------------
    view :: Model -> View Action
    view model = htmlToView $ case updatedView model of
                                Nothing -> Constr "Txt" [Lit (StrLit "Empty page")]
                                Just v  -> v
      
    ----------------------------------------------------------------------------
    -- | update
    ----------------------------------------------------------------------------
    update :: Action -> Model -> Effect Action Model
    
    update (Execute argActionValue parmsV) model = model <# do
      SetResult <$> doExecute r_FunMap model updateFunV argActionValue parmsV viewFunV
      
    update (SetResult ((_progModel, _updatedView), _progState)) model =
      noEff Model { progModel=_progModel
                  , progState=_progState
                  , updatedView=Just _updatedView }

    update NoOp model = noEff model

    ----------------------------------------------------------------------------
    -- | mount point
    ----------------------------------------------------------------------------
    miso_mount_point = toMiso_String mount_pointV
    ----------------------------------------------------------------------------

pageWebApp runtimeFunMap pageValue =
  error $ "[pageWebApp] Unexpected page structure: " ++ show pageValue


-- | For update

doExecute :: RuntimeFunctionMap -> Model -> Value -> Value -> [Value] -> Value -> IO ((Value, Value), RuntimeState)
doExecute runtimeFunMap model updateV funActionV parmsV viewV = do
  runStateT (do
     argActionV <-
      case parmsV of
       [] -> return funActionV
       [parmV] -> do UnitM resultV <- apply runtimeFunMap funActionV parmV
                     return resultV
       _ -> error $ "[WebRuntime:webModel:doExecute] Not support more than one parm: "
                        ++ show (Data.List.length parmsV)
        
     UnitM updateAction <- apply runtimeFunMap updateV argActionV
     UnitM newModel <- apply runtimeFunMap updateAction (progModel model)
     
     UnitM newView <- apply runtimeFunMap viewV newModel
     
     return (newModel, newView) ) (progState model)

webSend :: String -> Mem -> Value -> IO Mem
webSend appName mem v = do
  debug flag $ putStrLn $ "[client] webSend: " ++ show v ++ "\n"
  Just resp <- contents <$> xhrByteString (httpReq v)
  case eitherDecodeStrict resp :: Either String Value of
    Left s -> error s
    Right j -> do
       debug flag $ putStrLn $ "[client] webSend: result: " ++ show j ++ "\n"
       return $ mem { _reg = Just j }
    -- set the resp in the program state for receive to be able to take later!!
  
  where
    httpReq v = Request
      { reqMethod = POST
      , reqURI = pack $ "http://localhost:3000/" ++ appName
      , reqLogin = Nothing
      , reqHeaders = []
      , reqWithCredentials = False
      , reqData = StringData $ pack $ Data.List.map (chr. fromEnum) $ BS.unpack $ BSLazy.toStrict $ encode v -- (pack $ show $ toJSON $ v)
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

htmlToView :: Value -> View Action  -- Value=HTML [Msg], View Action

htmlToView (Constr "Element" [Lit (StrLit tag), attrs, htmls]) = htmlToView' tag
  where
    attrAttrs = attrToViews attrs
    htmlViews = htmlToViews htmls  

    htmlToView' "h1" = h1_ attrAttrs htmlViews
    htmlToView' "h2" = h2_ attrAttrs htmlViews
    htmlToView' "h3" = h3_ attrAttrs htmlViews
    htmlToView' "h4" = h4_ attrAttrs htmlViews
    htmlToView' "h5" = h5_ attrAttrs htmlViews
    htmlToView' "h6" = h6_ attrAttrs htmlViews

    htmlToView' "div" = div_ attrAttrs htmlViews
    
    htmlToView' "ul" = ul_ attrAttrs htmlViews
    htmlToView' "li" = li_ attrAttrs htmlViews

    htmlToView' "a" = a_ attrAttrs htmlViews

    htmlToView' "input" = input_ attrAttrs            -- Todo: htmlViews should be Nil ???
    htmlToView' "label" = label_ attrAttrs htmlViews
    htmlToView' "span" = span_ attrAttrs htmlViews

    htmlToView' "button" = button_ attrAttrs htmlViews
    
    htmlToView' "section" = section_ attrAttrs htmlViews
    htmlToView' "header" = header_ attrAttrs htmlViews
    htmlToView' "footer" = footer_ attrAttrs htmlViews

    htmlToView' "link" = link_ attrAttrs              -- htmls are supposed to be null.

    htmlToView' tag = error $ "[WebRuntime:htmlToView] Not supported yet: " ++ tag
  
htmlToView (Constr "Txt" [Lit (StrLit textLit)]) = text $ pack textLit

htmlToView v = error $ "[WebRuntime:htmlToView] Unexpected: " ++ show v

--
htmlToViews :: Value -> [View Action]

htmlToViews (Constr "Nil" []) = []

htmlToViews (Constr "Cons" [h, t]) =
  htmlToView h : htmlToViews t

htmlToViews v = error $ "[WebRuntime:htmlToViews] Unexpected: " ++ show v

--
attrToView :: Value -> Attribute Action -- Value=Attr [Msg], View Action

attrToView (Constr "Property" [Lit (StrLit "checked"), Lit (StrLit valueText)]) =
  if valueText=="true" then checked_ False         -- Todo: this should be reversed!
  else if valueText=="false" then checked_ True
  else error $ "[WebRuntime:attrToViews] Property: Unexpected: checked " ++ "=" ++ valueText

attrToView (Constr "Property" [Lit (StrLit keyText), Lit (StrLit valueText)]) = 
  stringProp (pack keyText) valueText

attrToView (Constr "Attribute" [Lit (StrLit keyText), Lit (StrLit valueText)]) =
  attrToView' keyText
  where
    attrToView' "class" = class_ $ pack valueText
    attrToView' "type" = type_ $ pack valueText
    attrToView' "style" = style_ $ toStyleMap valueText
    attrToView' "id" = id_ $ pack valueText
    attrToView' "for" = for_ $ pack valueText
    attrToView' "placeholder" = placeholder_ $ pack valueText
    attrToView' "value" = value_ $ pack valueText
    attrToView' "rel" = rel_ $ pack valueText
    attrToView' "href" = href_ $ pack valueText
    attrToView' _ = error $ "[WebRuntime:attrToViews] Attribute: Unexpected: "
                              ++ show keyText ++ "=" ++ valueText

    toStyleMap "display; block" = M.singleton (pack "display") (pack "block")

attrToView (Constr "EventBind" [Lit (StrLit eventName), msgV]) = attrToView' eventName
  where
    packedEventName = pack eventName
    
    attrToView' "click"    = onClick $ Execute msgV []
    attrToView' "dblclick" = onDoubleClick $ Execute msgV []
    attrToView' "blur"     = onBlur $ Execute msgV []
    attrToView' _ = error $ "[WebRuntime:attrToView] EventBind: Not supported yet:" ++ eventName

attrToView (Constr "KeyBind" [Lit (IntLit num), msgV]) = attrToView' num
  where
    attrToView' 13 = onEnter $ Execute msgV []
    attrToView' _ = error $ "[WebRuntime:attrToView] KeyBind: Not supported yet:" ++ show num

    onEnter :: Action -> Attribute Action
    onEnter action = onKeyDown $ bool NoOp action . (== KeyCode 13)
  

attrToView (Constr "ValueBind" [Lit (StrLit keyText), strToMsgFunV]) = attrToView' keyText
  where
    attrToView' "input" = onInput $ \x -> Execute strToMsgFunV [Lit (StrLit $ unpack $ x)]
    attrToView' _ = error $ "[WebRuntime:attrToView] ValueBind: Not supported yet:" ++ keyText

attrToView v = 
  error $ "[WebRuntime:attrToView] Unexpected: " ++ show v

--
attrToViews :: Value -> [Attribute Action]

attrToViews (Constr "Nil" []) = []

attrToViews (Constr "Cons" [h, t]) =
  attrToView h : attrToViews t

attrToViews v = error $ "[WebRuntime:attrToViews] Unexpected: " ++ show v

--------------
-- | Debugging
--------------

flag = True

debug True  io = do io
debug False io = return ()

