
> module WorkflowXml where
> import Text.XML.HaXml.Xml2Haskell
> import Text.XML.HaXml.Parse
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types
> import Workflow
> import qualified Data.Map as Map
> import Control.Monad
> import XmlUtil
> import Control.Monad.Error

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

> readArcs element =
>     do map (attrVal) $ attributed "to" ((tag "arc") `o` children) (CElem element)
>     where attrVal (v,_) = NodeId (read v::Int)

> readExternalArcs element = forM childElem (readExternalArcFromElem)
>     where
>         childElem = XmlUtil.toElem $ ((tag "externalArc") `o` children) (CElem element)

> readExternalArcFromElem e =
>     do workflowId <- readAttr e "workflowId"
>        version    <- readAttr e "version"
>        instanceId <- readAttr e "instanceId"
>        nodeId     <- readAttr e "nodeId"
>        arcTypeS   <- readAttr e "type"
>        let arcType = case (arcTypeS) of
>                          "in"      -> InArc
>                          otherwise -> OutArc
>        return $ ExternalArc nodeId workflowId (read version::Int) (read instanceId::Int) arcType

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

 loadWfGraph :: String -> Int -> (Map.Map Name (Element->XmlNode a)) -> IO (Either String (WfGraph a))

> loadWfGraph name version elemFuncMap =
>     do maybeDoc <- liftIO $ loadXmlForWorkflow name version
>        case (maybeDoc) of
>            Right doc -> return $ loadWfGraphFromDoc doc elemFuncMap
>            Left  msg -> throwError msg

The following functions handle the generation of a WfGraph based on an XML document.
The loadWfGraphFromDoc function takes a map of tag names to function which take
elements of that type and return the appropriate XmlNode.

> loadWfGraphFromDoc doc elemFuncMap = xmlNodesToWfGraph $ processChildNodes childNodes elemFuncMap Map.empty
>     where
>         childNodes = getChildren (rootElement doc)

> processChildNodes []       _           nodeMap = nodeMap
> processChildNodes (e:rest) elemFuncMap nodeMap = processChildNodes rest elemFuncMap newNodeMap
>     where
>         elemName     = case (e) of (Elem name _ _ ) -> name
>         nodeFunction = elemFuncMap Map.! elemName
>         node         = nodeFunction e
>         newNodeMap   = Map.insert (getWfNodeId node) node nodeMap

--> loadExternalWorkflows xmlNodes elemFuncMap = foldr (f) startMap xmlNodes
-->     where
-->         f xmlNode wfMap = foldr (f') wfMap (externalArcs xmlNode)
-->         f' extArc maybeMapIO = do maybeMap <- maybeMapIO
-->                                   case (maybeMap) of
-->                                       Right wfMap -> loadExternal wfMap extArc elemFuncMap
-->                                       Left  msg   -> return $ Left msg
-->         startMap = do return $ Right Map.empty
--
--> loadExternal wfMap extArc elemFuncMap =
-->     if (Map.member key wfMap)
-->        then do return $ Right wfMap
-->        else do maybeGraph <- loadWfGraph (targetWf extArc) (targetVersion extArc) elemFuncMap
-->                case (maybeGraph) of
-->                    Right graph -> return $ Right $ Map.insert key graph wfMap
-->                    Left  msg   -> return $ Left msg
-->     where
-->         key = targetInstance extArc
-->

Function for processing the start element. There should be exactly one of these
per workflow definition. It should contain only arc and externalArc elements. It
has no attributes

> processStartElement element = completeXmlNode node element
>     where
>         node = Node StartNodeId RequireSingle defaultGuard completeExecution

Function for processing node elements. There can be any number of these in each
workflow. They have no logic associated with them. They have a nodeId, which
should be unique in that workflow and a type, which corresponds to the NodeType
type in Workflow. Nodes should contain only arc and externalArc elements.

> processNodeElement element =
>     do nodeId   <- readAttr element "nodeId"
>        nodeType <- readAttr element "type"
>        completeXmlNode (newNode nodeId nodeType) element
>     where
>         newNode nodeId nodeType = Node (NodeId (read nodeId::Int)) (nodeTypeFromString nodeType) defaultGuard completeExecution

> completeXmlNode node element =
>    do externalArcs <- readExternalArcs element
>       return $ XmlNode node arcs externalArcs
>    where arcs = readArcs element

> defaultElemFunctionMap = Map.fromList [ ("start", processStartElement),
>                                         ("node",  processNodeElement) ]

--> elemMapWith list = addToMap list defaultElemFunctionMap
-->    where
-->        addToMap []     map = map
-->        addToMap (x:xs) map = addToMap xs $ Map.insert (fst x) (snd x) map

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