
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
>   deriving (Show)

> data XmlNode a =
>     XmlNode {
>         wfNode       :: Node a,
>         arcs         :: [Int],
>         arcRefs      :: [String],
>         externalArcs :: [ExternalArc]
>     }
>  deriving (Show)

> data ExternalArc =
>     ExternalArc {
>       targetNodeRef  :: String,
>       targetWf       :: String,
>       targetVersion  :: String,
>       targetInstance :: String,
>       arcType        :: ArcType
>     }
>  deriving (Show)

> wfNodeId = nodeId.wfNode

> readArcs element =
>     do map (attrVal) $ attributed "to" ((tag "arc") `o` children) (CElem element)
>     where attrVal (v,_) = v

> readExternalArcs element = map (readExternalArcFromElem) childElem
>     where
>         childElem = XmlUtil.toElem $ ((tag "externalArc") `o` children) (CElem element)

> readExternalArcFromElem e = ExternalArc nodeId workflowId version instanceId arcType
>     where
>        workflowId = readAttr e "workflow"
>        version    = readAttr e "version"
>        instanceId = readAttr e "instance"
>        nodeId     = readAttr e "nodeId"
>        arcType    = case (readAttr e "type") of
>                         "in"      -> InArc
>                         otherwise -> OutArc

loadWfGraphFromFile
  Loads a WfGraph from the given file, using the given map of tag names to functions.

> loadWfGraphFromFile filename funcMap =
>     do xmlStr <- readFile filename
>        case (xmlParse' filename xmlStr) of
>            Left msg -> return $ Left msg
>            Right doc -> loadWfGraphFromDoc doc source funcMap
>     where
>         source = NodeSource "test" "test" "test" 0

Given a name and a version number, this function will return the corresponding XML document.

> loadXmlForWorkflow name version =
>     do xmlStr <- readFile filename
>        return $ xmlParse' filename xmlStr
>     where
>         filename = wfDir ++ name ++ "." ++ version ++ ".wf.xml"
>         wfDir = "/home/paul/workspace/functional-workflow/test-wf/"

> loadWfGraph source funcMap =
>     do maybeDoc <- loadXmlForWorkflow (wfName source) (wfVersion source)
>        case (maybeDoc) of
>            Right doc -> loadWfGraphFromDoc doc source funcMap
>            Left  msg -> return $ Left msg

The following functions handle the generation of a WfGraph based on an XML document.
The loadWfGraphFromDoc function takes a map of tag names to function which take
elements of that type and return the appropriate XmlNode.

> loadWfGraphFromDoc doc source funcMap =
>     do maybeExternal <- loadExternalWorkflows (Map.elems xmlNodes) funcMap (wfDepth source)
>        case (maybeExternal) of
>            Left msg  -> return $ Left msg
>            Right ext -> do let allXmlNodes = importExternals xmlNodes ext
>                            putStrLn (show allXmlNodes)
>                            let resolvedXmlNodes = resolveAllExternalArcs allXmlNodes
>                            putStrLn (show resolvedXmlNodes)
>                            return $ Right $ xmlNodesToWfGraph resolvedXmlNodes
>     where
>         childNodes = getChildren (rootElement doc)
>         xmlNodes   = findNodeArcs $ processChildNodes childNodes source funcMap Map.empty 1
>

> processChildNodes []       _      _       nodeMap nextId = nodeMap
> processChildNodes (e:rest) source funcMap nodeMap nextId = processChildNodes rest source funcMap newNodeMap (nextId + 1)
>     where
>         elemName     = case (e) of (Elem name _ _ ) -> name
>         nodeFunction = funcMap Map.! elemName
>         node         = fixId $ nodeFunction e source
>         xmlNode      = XmlNode node [] (readArcs e) (readExternalArcs e)
>         newNodeMap   = Map.insert (nodeId node) xmlNode nodeMap
>         fixId  node  = case (nodeId node) of
>                            (-1) -> node
>                            otherwise -> node {nodeId = nextId}

> loadExternalWorkflows xmlNodes elemFuncMap depth = foldr (checkNodes) startMap xmlNodes
>     where
>         checkNodes xmlNode wfMap    = foldr (checkArcs) wfMap (externalArcs xmlNode)
>         checkArcs extArc maybeMapIO = do maybeMap <- maybeMapIO
>                                          case (maybeMap) of
>                                              Right wfMap -> loadExternal wfMap extArc elemFuncMap depth
>                                              Left  msg   -> return $ Left msg
>         startMap = do return $ Right Map.empty

> loadExternal wfMap extArc funcMap depth =
>     if (Map.member key wfMap)
>        then do return $ Right wfMap
>        else do putStrLn $ "Loading " ++ (targetWf extArc) ++ " version: " ++ (targetVersion extArc)
>                maybeGraph <- loadWfGraph source funcMap
>                case (maybeGraph) of
>                    Right graph -> do putStrLn $ "Loaded: " ++ (showGraph graph)
>                                      return $ Right $ Map.insert key graph wfMap
>                    Left  msg   -> return $ Left msg
>     where
>         key = targetInstance extArc
>         source = NodeSource (targetWf extArc) (targetVersion extArc) (targetInstance extArc) (depth + 1)

To import an external workflow into loading workflow, we must take the following steps:
  1. Convert the nodes back to XmlNodes

     Because the node ids will overlap with those in the top level graph, we must assign new ids.
     We can just start the numbering where we left off. Because the ids will all increase by the
     same amount we can fix the incoming/outgoing arcs easily.

  3. Convert external arcs to regular arcs

> importExternals current externals = foldr (importExternal) current (Map.elems externals)

> importExternal graph current = foldr (\node nodeMap-> Map.insert (wfNodeId node) node nodeMap) current xmlNodes
>     where
>         nextId   = Map.size current
>         xmlNodes = map (importXmlNode (nextId - 1)) $ zip [nextId..] (Map.elems graph)

> importXmlNode baseIncr (nextId, nodeArcs) = XmlNode node (map ((+baseIncr).nodeId) (nodeOutputs nodeArcs)) [] []
>     where
>         node = (fixId.arcsNode) nodeArcs
>         fixId node = node { nodeId = nextId }

> findNodeArcs nodeMap = foldr (lookupArcs) nodeMap (Map.elems nodeMap)
>     where
>         lookupArcs node nodeMap = Map.insert (wfNodeId node) (node {arcs=(arcList node)}) nodeMap
>         arcList node            = map (lookupArcRef) (arcRefs node)
>         lookupArcRef ref        = (wfNodeId.head) $ filter (isRefNode ref) (Map.elems nodeMap)
>         isRefNode ref xmlNode   = ref == (nodeRefId.wfNode) xmlNode

> resolveAllExternalArcs nodeMap = foldr (resolveNodeExternals) nodeMap (Map.elems nodeMap)

> resolveNodeExternals node nodeMap = foldr (resolveNodeExternal node) nodeMap (externalArcs node)

> resolveNodeExternal node (ExternalArc nodeRef wf version inst arcType ) nodeMap =
>     Map.insert (wfNodeId newNode) newNode nodeMap
>     where
>         newNode = case (arcType) of
>             OutArc -> node { arcs = (wfNodeId targetNode):(arcs node) }
>             InArc  -> targetNode { arcs = (wfNodeId node):(arcs targetNode) }
>         targetNode = head $ filter (isMatch) (Map.elems nodeMap)
>         isMatch xmlNode = (nodeRefId.wfNode) xmlNode == nodeRef &&
>                           (wfInstance.source.wfNode) xmlNode== inst &&
>                           (wfDepth.source.wfNode) xmlNode == 1

Function for processing the start element. There should be exactly one of these
per workflow definition. It should contain only arc and externalArc elements. It
has no attributes

> processStartElement element source = Node (-1) "start" source RequireSingle defaultGuard completeExecution

Function for processing node elements. There can be any number of these in each
workflow. They have no logic associated with them. They have a nodeId, which
should be unique in that workflow and a type, which corresponds to the NodeType
type in Workflow. Nodes should contain only arc and externalArc elements.

> processNodeElement element source = newNode nodeId nodeType
>     where
>         newNode nodeId nodeType = Node 0 nodeId source nodeType defaultGuard completeExecution
>         nodeId    = readAttr element "nodeId"
>         nodeTypeS = readAttr element "type"
>         nodeType  = nodeTypeFromString nodeTypeS


> defaultElemFunctionMap = Map.fromList [ ("start", processStartElement),
>                                         ("node",  processNodeElement) ]

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
>         targetNodeId   = wfNodeId xmlNode