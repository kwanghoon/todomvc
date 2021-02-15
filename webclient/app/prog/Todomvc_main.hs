
module Todomvc_main where

import Location
import Literal
import Prim
import Runtime

todomvc_main :: Expr
todomvc_main = BindM [Binding False "concat" (UnitM (Closure [] (CodeName "csF_18") []))] (BindM [Binding False "!" (UnitM (Closure [] (CodeName "csF_26") []))] (BindM [Binding True "thunk" (BindM [Binding False "cg$jk" (UnitM (Closure [] (CodeName "csF_35") []))] (UnitM (Var "cg$jk")))] (BindM [Binding True "mapWithCount" (UnitM (Closure [Var "mapWithCount"] (CodeName "csF_74") ["mapWithCount"]))] (BindM [Binding True "mapOnIndex" (UnitM (Closure [Var "mapWithCount"] (CodeName "csF_107") []))] (BindM [Binding True "count" (UnitM (Closure [Var "count"] (CodeName "csF_117") ["count"]))] (BindM [Binding True "filter" (UnitM (Closure [Var "filter"] (CodeName "csF_152") ["filter"]))] (BindM [Binding True "delete" (UnitM (Closure [Var "delete"] (CodeName "csF_179") ["delete"]))] (BindM [Binding True "map" (UnitM (Closure [Var "mapWithCount"] (CodeName "csF_197") []))] (BindM [Binding True "cs" (BindM [Binding False "cg$jl" (UnitM (Closure [] (CodeName "csF_208") []))] (UnitM (Var "cg$jl")))] (BindM [Binding True "onClick" (BindM [Binding False "cg$jm" (UnitM (Closure [] (CodeName "csF_218") []))] (UnitM (Var "cg$jm")))] (BindM [Binding True "onDblClick" (BindM [Binding False "cg$jn" (UnitM (Closure [] (CodeName "csF_228") []))] (UnitM (Var "cg$jn")))] (BindM [Binding True "onBlur" (BindM [Binding False "cg$jo" (UnitM (Closure [] (CodeName "csF_238") []))] (UnitM (Var "cg$jo")))] (BindM [Binding True "onEnter" (BindM [Binding False "cg$jp" (UnitM (Closure [] (CodeName "csF_248") []))] (UnitM (Var "cg$jp")))] (BindM [Binding True "onInput" (BindM [Binding False "cg$jq" (UnitM (Closure [] (CodeName "csF_261") []))] (UnitM (Var "cg$jq")))] (BindM [Binding False "cg$jr" (UnitM (Constr "Nil" []))] (BindM [Binding True "nlH" (Let [Binding False "cg$js" (Var "cg$jr")] (UnitM (Var "cg$js")))] (BindM [Binding False "cg$jt" (UnitM (Constr "Nil" []))] (BindM [Binding True "nlA" (Let [Binding False "cg$ju" (Var "cg$jt")] (UnitM (Var "cg$ju")))] (BindM [Binding True "csH" (UnitM (Closure [Var "cs"] (CodeName "csF_270") []))] (BindM [Binding True "csA" (UnitM (Closure [Var "cs"] (CodeName "csF_277") []))] (BindM [Binding True "toggleEditing" (UnitM (Closure [] (CodeName "csF_293") []))] (BindM [Binding True "toggleItem" (UnitM (Closure [] (CodeName "csF_309") []))] (BindM [Binding True "newContent" (UnitM (Closure [] (CodeName "csF_325") []))] (BindM [Binding True "showItem" (UnitM (Closure [Var "concat",Var "csA",Var "csH",Var "nlA",Var "nlH",Var "onBlur",Var "onClick",Var "onDblClick",Var "onEnter",Var "onInput"] (CodeName "csF_618") []))] (BindM [Binding True "showList" (UnitM (Closure [Var "csA",Var "csH",Var "mapWithCount",Var "nlA",Var "nlH",Var "onClick",Var "showItem"] (CodeName "csF_797") []))] (BindM [Binding True "header" (UnitM (Closure [Var "csA",Var "csH",Var "nlA",Var "nlH",Var "onEnter",Var "onInput"] (CodeName "csF_934") []))] (BindM [Binding True "footer" (UnitM (Closure [Var "concat",Var "csA",Var "csH",Closure [] (CodeName "csF_12") [],Var "nlA",Var "nlH",Var "onClick"] (CodeName "csF_1246") []))] (BindM [Binding True "view" (UnitM (Closure [Var "count",Var "csA",Var "csH",Var "filter",Var "footer",Var "header",Var "nlA",Var "nlH",Var "showList"] (CodeName "csF_1378") []))] (BindM [Binding True "isNotDone" (UnitM (Closure [] (CodeName "csF_1381") []))] (BindM [Binding True "isDone" (UnitM (Closure [] (CodeName "csF_1383") []))] (BindM [Binding True "update" (UnitM (Closure [Var "!",Closure [] (CodeName "csF_32") [],Var "delete",Var "filter",Var "isDone",Var "isNotDone",Var "map",Var "mapOnIndex",Var "newContent",Var "toggleEditing",Var "toggleItem"] (CodeName "csF_1836") []))] (BindM [Binding True "serverModel" (BindM [Binding False "csX_1837" (Let [Binding False "cg$jv" (Var "thunk")] (UnitM (Var "cg$jv")))] (Req (Var "csX_1837") (Closure [Closure [] (CodeName "csF_22") []] (CodeName "csF_1845") [])))] (BindM [Binding True "init" (BindM [Binding False "csX_1846" (BindM [Binding False "csX_1848" (App (Closure [] (CodeName "csF_1857") []) (Lit (StrLit "")))] (BindM [Binding False "csX_1849" (BindM [Binding False "csX_1858" (BindM [Binding False "csX_1860" (App (Var "!") (Constr "server" []))] (Let [Binding False "cg$jw" (Var "csX_1860")] (UnitM (Var "cg$jw"))))] (Req (Var "csX_1858") (Var "serverModel")))] (App (Var "csX_1848") (Var "csX_1849"))))] (App (Var "csX_1846") (Var "serverModel")))] (BindM [Binding True "main" (BindM [Binding False "csX_1862" (BindM [Binding False "csX_1864" (BindM [Binding False "csX_1866" (BindM [Binding False "csX_1868" (BindM [Binding False "cg$jy" (BindM [Binding False "cg$jx" (UnitM (Closure [] (CodeName "csF_1879") []))] (UnitM (Var "cg$jx")))] (BindM [Binding False "csX_1870" (Let [Binding False "cg$jz" (Var "cg$jy")] (UnitM (Var "cg$jz")))] (Let [Binding False "cg$ka" (Var "csX_1870")] (UnitM (Var "cg$ka")))))] (App (Var "csX_1868") (Var "init")))] (App (Var "csX_1866") (Closure [Var "view"] (CodeName "csF_1882") [])))] (App (Var "csX_1864") (Closure [Var "update"] (CodeName "csF_1888") [])))] (App (Var "csX_1862") (Lit (StrLit "#body"))))] (UnitM (Var "main"))))))))))))))))))))))))))))))))))))