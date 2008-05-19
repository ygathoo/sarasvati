
> module TaskXml where
> import WorkflowXml
> import Workflow
> import Task

> processTaskElement element = unwrapXmlProc $
>     do useElement element
>        nodeId   <- readAttr "id"
>        arcs <- readArcs
>        nodeType <- readAttr "type"
>        name <- readText "name"
>        desc <- readText "description"
>        return $ XmlNode (createTaskNode nodeId nodeType name desc) arcs

> createTaskNode strNodeId strNodeType name desc = Node nodeId nodeType defaultGuard acceptFunction
>     where
>         acceptFunction          = acceptAndCreateTask (read strNodeId::Int) name desc
>         nodeId                  = NodeId (read strNodeId::Int)
>         nodeType                = nodeTypeFromString strNodeType


