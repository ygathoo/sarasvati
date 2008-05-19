
> module WorkflowXml where
> import Text.XML.HaXml.Xml2Haskell
> import Text.XML.HaXml.Parse
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types
> import Workflow
> import qualified Data.Map as Map
> import Control.Monad

> data ArcType = InArc | OutArc

> data XmlNode a =
>     XmlNode {
>         wfNode :: Node a,
>         arcs :: [NodeId],
>         externalArcs :: [ExternalArc]
>     }

> data ExternalArc =
>     ExternalArc {
>       targetNodeId   :: String,
>       targetWf       :: String,
>       targetVersion  :: Int,
>       targetInstance :: Int,
>       arcType        :: ArcType
>     }

> getWfNodeId = getNodeId.wfNode

> foldWith :: (a -> b -> a) -> a -> [b] -> a
> foldWith _ a []     = a
> foldWith f a (x:xs) = foldWith f (f a x) xs

> data XmlProc a = XmlProc ([Element]->(a, [Element]))

> instance Monad (XmlProc) where
>     return a = XmlProc (\_->(a,[]))
>
>     XmlProc a >>= f = XmlProc (\elemArray->
>         case (a elemArray) of
>             (currValue, xmlElem) -> case (f currValue) of
>                  XmlProc result -> result xmlElem)

> useDoc (Document _ _ element _ ) = useElement element

> useElement element = XmlProc (\_-> ((), [element]))

> readAttr name = XmlProc (\(x:xs)-> (getElemAttr x name, (x:xs)))

> getChildren = XmlProc (\(x:xs)-> (map (cElemToElem) (childElements (CElem x)), (x:xs)))
>     where childElements = elm `o` children

> getElemAttr (Elem _ attrList _ ) name
>      | null attrs = fail "Attribute not found"
>      | otherwise  = attrVal' (head attrs)
>     where
>         attrs = filter (\(attrName, attrValue) -> attrName == name) attrList
>         attrVal' (_, AttValue atlist) = case (head atlist) of (Left val) -> val

> cElemToElem (CElem element) = element

> readArcs = XmlProc (\(x:xs) -> (readArcsFromElem x, (x:xs)))

> readArcsFromElem element = map (attrVal) $ attributed "to" ((tag "arc") `o` children) (CElem element)
>     where attrVal (v,_) = NodeId (read v::Int)

> readExternalArcs = XmlProc (\(x:xs) -> (readExternalArcsFromElem x, (x:xs)))

> readExternalArcsFromElem element = map (readExternalArcFromElem.cElemToElem) childElem
>     where
>         childElem =  ((tag "externalArc") `o` children) (CElem element)

> readExternalArcFromElem element = unwrapXmlProc $
>     do useElement element
>        workflowId <- readAttr "workflowId"
>        version    <- readAttr "version"
>        instanceId <- readAttr "instanceId"
>        nodeId     <- readAttr "nodeId"
>        arcTypeS   <- readAttr "type"
>        let arcType = case (arcTypeS) of
>                          "in"      -> InArc
>                          otherwise -> OutArc
>        return $ ExternalArc nodeId workflowId (read version::Int) (read instanceId::Int) arcType

> readText name = XmlProc (\(x:xs) -> (readTextElements x name, (x:xs)))

> readTextElements element name = concatMap (stripMaybe.fst) $ filter (onlyJust) labels
>     where
>         onlyJust (Nothing,_)  = False
>         onlyJust ((Just _),_) = True
>         stripMaybe (Just x)   = x
>         labels                = textlabelled ( path [ children, tag name, children, txt ] ) (CElem element)

> unwrapXmlProc (XmlProc a) = case (a []) of (result,_) -> result


loadWfGraphFromFile
  Loads a WfGraph from the given file, using the given map of tag names to functions.

> loadWfGraphFromFile filename elemFuncMap =
>     do xmlStr <- readFile filename
>        case (xmlParse' filename xmlStr) of
>            Left msg -> return $ Left msg
>            Right doc -> return $ Right $ loadWfGraphFromDoc doc elemFuncMap

Given a name and a version number, this function will return the corresponding XML document.

> loadXmlForWorkflow name version =
>     do xmlStr <- readFile filename
>        return $ xmlParse' filename xmlStr
>     where
>         filename = name ++ "." ++ (show version) ++ ".wf.xml"

> loadWfGraph :: String -> Int -> (Map.Map Name (Element->XmlNode a)) -> IO (Either String (WfGraph a))
> loadWfGraph name version elemFuncMap =
>     do maybeDoc <- loadXmlForWorkflow name version
>        case (maybeDoc) of
>            Right doc -> return $ loadWfGraphFromDoc doc elemFuncMap
>            Left  msg -> return $ Left msg

The following functions handle the generation of a WfGraph based on an XML document.
The loadWfGraphFromDoc function takes a map of tag names to function which take
elements of that type and return the appropriate XmlNode.

 loadWfGraphFromDoc :: Document -> (Map.Map Name (Element->XmlNode a)) -> Either String (WfGraph a)

> loadWfGraphFromDoc doc elemFuncMap =
>      do liftM $ useDoc doc
>         children <- getChildren
>         let nodeMap = processChildNodes children elemFuncMap Map.empty
>         maybeInstanceMap<- loadExternalWorkflows (Map.elems nodeMap) elemFuncMap
>         case (maybeInstanceMap) of
>             Left msg -> return msg
>             Right instanceMap -> return $ xmlNodesToWfGraph nodeMap

> processChildNodes []       _           nodeMap = nodeMap
> processChildNodes (e:rest) elemFuncMap nodeMap = processChildNodes rest elemFuncMap newNodeMap
>     where
>         elemName     = case (e) of (Elem name _ _ ) -> name
>         nodeFunction = elemFuncMap Map.! elemName
>         node         = nodeFunction e
>         newNodeMap   = Map.insert (getWfNodeId node) node nodeMap

> loadExternalWorkflows xmlNodes elemFuncMap = foldWith (f) startMap xmlNodes
>     where
>         f wfMap xmlNode = foldWith (f') wfMap (externalArcs xmlNode)
>         f' maybeMapIO extArc = do maybeMap <- maybeMapIO
>                                   case (maybeMap) of
>                                       Right wfMap -> loadExternal wfMap extArc elemFuncMap
>                                       Left  msg   -> return $ Left msg
>         startMap = do return $ Right Map.empty

> loadExternal wfMap extArc elemFuncMap =
>     if (Map.member key wfMap)
>        then do return $ Right wfMap
>        else do maybeGraph <- loadWfGraph (targetWf extArc) (targetVersion extArc) elemFuncMap
>                case (maybeGraph) of
>                    Right graph -> return $ Right $ Map.insert key graph wfMap
>                    Left  msg   -> return $ Left msg
>     where
>         key = targetInstance extArc
>

Function for processing the start element. There should be exactly one of these
per workflow definition. It should contain only arc and externalArc elements. It
has no attributes

> processStartElement element = unwrapXmlProc $ completeXmlNode node element
>     where
>         node = Node StartNodeId RequireAll defaultGuard completeExecution

Function for processing node elements. There can be any number of these in each
workflow. They have no logic associated with them. They have a nodeId, which
should be unique in that workflow and a type, which corresponds to the NodeType
type in Workflow. Nodes should contain only arc and externalArc elements.

> processNodeElement element = unwrapXmlProc $
>     do useElement element
>        id   <- readAttr "nodeId"
>        nodeType <- readAttr "type"
>        completeXmlNode (newNode id nodeType) element
>     where
>         newNode id nodeType = Node (NodeId (read id::Int)) (getType nodeType) defaultGuard completeExecution
>         getType "requireSingle" = RequireSingle
>         getType _               = RequireAll

> completeXmlNode node element =
>    do useElement element
>       arcs <-readArcs
>       externalArcs <- readExternalArcs
>       return $ XmlNode node arcs externalArcs

> defaultElemFunctionMap = Map.fromList [ ("start", processStartElement ),
>                                         ("node",  processNodeElement ) ]

> elemMapWith list = addToMap list defaultElemFunctionMap
>    where
>        addToMap []     map = map
>        addToMap (x:xs) map = addToMap xs $ Map.insert (fst x) (snd x) map

The following function deal with converting a map of XmlNode instances to
a WfGraph. Since XmlNode instances only track outgoing nodes, we need to
infer the incoming nodes.

> xmlNodesToWfGraph = graphFromArcs.xmlNodesToNodeArcs

> xmlNodesToNodeArcs nodeMap = map (xmlNodeToNodeArcs nodeMap) (Map.elems nodeMap)

> xmlNodeToNodeArcs nodeMap xmlNode = NodeArcs (wfNode xmlNode) inputs outputs
>     where
>         inputs    = map (wfNode) $ xmlNodeInputs xmlNode nodeMap
>         outputs   = map (toNode) $ arcs xmlNode
>         mapLookup = (Map.!) nodeMap
>         toNode    = wfNode.mapLookup

> xmlNodeInputs xmlNode nodeMap = filter (isInput) $ Map.elems nodeMap
>     where
>         isInput source = not.null $ filter ((==) targetNodeId) (arcs source)
>         targetNodeId   = getWfNodeId xmlNode