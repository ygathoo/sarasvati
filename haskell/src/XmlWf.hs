module XmlWf where

import Text.XML.HaXml.Xml2Haskell
import Text.XML.HaXml.OneOfN
import Char (isSpace)


{-Type decls-}

data Workflow = Workflow Workflow_Attrs Start [Node]
              deriving (Eq,Show)
data Workflow_Attrs = Workflow_Attrs
    { workflowId :: String
    , workflowVersion :: String
    } deriving (Eq,Show)
newtype Start = Start [Arc] 		deriving (Eq,Show)
data Node = Node Node_Attrs [Arc]
          deriving (Eq,Show)
data Node_Attrs = Node_Attrs
    { nodeId :: String
    , nodeType :: Node_type
    } deriving (Eq,Show)
data Node_type = Node_type_requireSingle  |  Node_type_requireAll
               deriving (Eq,Show)
data Arc = Arc Arc_Attrs String
         deriving (Eq,Show)
data Arc_Attrs = Arc_Attrs
    { arcTo :: String
    } deriving (Eq,Show)


{-Instance decls-}

instance XmlContent Workflow where
    fromElem (CElem (Elem "workflow" as c0):rest) =
        (\(a,ca)->
           (\(b,cb)->
              (Just (Workflow (fromAttrs as) a b), rest))
           (many fromElem ca))
        (definite fromElem "<start>" "workflow" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Workflow as a b) =
        [CElem (Elem "workflow" (toAttrs as) (toElem a ++
                                              concatMap toElem b))]
instance XmlAttributes Workflow_Attrs where
    fromAttrs as =
        Workflow_Attrs
          { workflowId = definiteA fromAttrToStr "workflow" "id" as
          , workflowVersion = definiteA fromAttrToStr "workflow" "version" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (workflowId v)
        , toAttrFrStr "version" (workflowVersion v)
        ]
instance XmlContent Start where
    fromElem (CElem (Elem "start" [] c0):rest) =
        (\(a,ca)->
           (Just (Start a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Start a) =
        [CElem (Elem "start" [] (concatMap toElem a))]
instance XmlContent Node where
    fromElem (CElem (Elem "node" as c0):rest) =
        (\(a,ca)->
           (Just (Node (fromAttrs as) a), rest))
        (many fromElem c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Node as a) =
        [CElem (Elem "node" (toAttrs as) (concatMap toElem a))]
instance XmlAttributes Node_Attrs where
    fromAttrs as =
        Node_Attrs
          { nodeId = definiteA fromAttrToStr "node" "id" as
          , nodeType = definiteA fromAttrToTyp "node" "type" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "id" (nodeId v)
        , toAttrFrTyp "type" (nodeType v)
        ]
instance XmlAttrType Node_type where
    fromAttrToTyp n (n',v)
        | n==n'     = translate (attr2str v)
        | otherwise = Nothing
      where translate "requireSingle" = Just Node_type_requireSingle
            translate "requireAll" = Just Node_type_requireAll
            translate _ = Nothing
    toAttrFrTyp n Node_type_requireSingle = Just (n, str2attr "requireSingle")
    toAttrFrTyp n Node_type_requireAll = Just (n, str2attr "requireAll")
instance XmlContent Arc where
    fromElem (CElem (Elem "arc" as c0):rest) =
        (\(a,ca)->
           (Just (Arc (fromAttrs as) a), rest))
        (definite fromText "text" "arc" c0)
    fromElem (CMisc _:rest) = fromElem rest
    fromElem (CString _ s:rest) | all isSpace s = fromElem rest
    fromElem rest = (Nothing, rest)
    toElem (Arc as a) =
        [CElem (Elem "arc" (toAttrs as) (toText a))]
instance XmlAttributes Arc_Attrs where
    fromAttrs as =
        Arc_Attrs
          { arcTo = definiteA fromAttrToStr "arc" "to" as
          }
    toAttrs v = catMaybes 
        [ toAttrFrStr "to" (arcTo v)
        ]


{-Done-}
