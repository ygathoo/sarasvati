
> module WorkflowXml where
> import Text.XML.HaXml.Xml2Haskell
> import Text.XML.HaXml.Parse
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types
> import Workflow
> import qualified Data.Map as Map

> data ArcType = InArc | OutArc

> data XmlNode a =
>     XmlNode {
>         wfNode :: Node a,
>         arcs :: [NodeId]
>     }

> data XmlArc =
>     XmlArc {
>       targetNodeId     :: String,
>       targetWorkflow   :: String,
>       targetVersion    :: Int,
>       targetInstanceId :: Int,
>       arcType          :: ArcType
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

> readArcsFromElem element = map (attrVal) $ attributed "nodeId" ((tag "arc") `o` children) (CElem element)
>     where attrVal (v,_) = NodeId (read v::Int)

> readArcFromElem element =
>    do useElement element
>       nodeId <- readAttr "nodeId"
>       return $ XmlArc nodeId "" 1 1 OutArc

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

The following functions handle the generation of a WfGraph based on an XML document.
The loadWfGraphFromDoc function takes a map of tag names to function which take
elements of that type and return the appropriate XmlNode.

> loadWfGraphFromDoc :: Document -> (Map.Map Name (Element->XmlNode a)) -> WfGraph a
> loadWfGraphFromDoc doc elemFuncMap =
>     xmlNodesToWfGraph $ unwrapXmlProc $
>          do useDoc doc
>             children <- getChildren
>             return $ processChildNodes children elemFuncMap Map.empty

> processChildNodes []       _           nodeMap = nodeMap
> processChildNodes (e:rest) elemFuncMap nodeMap = processChildNodes rest elemFuncMap newNodeMap
>     where
>         elemName     = case (e) of (Elem name _ _ ) -> name
>         nodeFunction = elemFuncMap Map.! elemName
>         node         = nodeFunction e
>         newNodeMap   = Map.insert (getWfNodeId node) node nodeMap

Function for processing the <start> element. There should be exactly one of these
per workflow definition.

> processStartElement element = unwrapXmlProc $ completeXmlNode node element
>     where
>         node = Node StartNodeId RequireAll defaultGuard completeExecution

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
>       return $ XmlNode node arcs

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