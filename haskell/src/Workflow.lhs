Author: Paul Lorenz

> module Workflow where
> import Data.Map (Map)
> import qualified Data.Map as Map

Node
  Represents a node in a workflow graph.

  Members:
    nodeId - An integer id, which should be unique. Used for testing equality
    accept - function which handles incoming tokens.

  Connections between Nodes are represented by NodeArcs and WFGraph

> data Node = NullNode
>           | Node {
>               nodeId  :: Integer,
>               accept  :: (Token -> WFGraph -> [Token] -> IO [Token])
>             }

> instance Eq Node where
>   NullNode  == NullNode = True
>   NullNode  == _        = False
>   _         == NullNode = False
>   node1     == node2    = (nodeId node1) == (nodeId node2)

> instance Show Node where
>   show NullNode = "[Node: NullNode]"
>   show a        = "[Node id: " ++ (show (nodeId a)) ++ "]"

NodeArcs
  Represents the incoming and outgoing connections to other nodes.

  Members:
    node: The related node
    nodeInputs: The list of incoming node connections
    nodeOutputs: The list of outgoing node connections

> data NodeArcs =
>   NodeArcs {
>     node :: Node,
>     nodeInputs :: [Node],
>     nodeOutputs :: [Node]
>  }

Token
  The set of current tokens gives the current state of the workflow.

  Members:
    tokenId: A list of int, giving a unique id across all tokens
    prevNode: The input node the token came from/is coming from
    currNode: If the token is being processed by a node, this will be set
              to that node. Otherwise it will be set to NullNode
    nextNode: If the token is between nodes, this will be set to the node
              it is going to. Otherwise it will be set to NullNode

> data Token =
>   Token {
>     tokenId::[Int],
>     prevNode::Node,
>     currNode::Node,
>     nextNode::Node
>  }
>  deriving (Show)

> instance Eq Token where
>   t1 == t2 = (tokenId t1) == (tokenId t2)

WFGraph
  This is just a container for NodeArcs, which can be queried
  for the inputs and outputs of a given node

> data WFGraph =
>   WFGraph {
>     nodeMap :: Map Integer NodeArcs
>   }

> instance Show WFGraph where
>  show graph = "[WFGraph " ++ show (Map.size $ nodeMap graph) ++ "]"

inputs
  Returns the Nodes which are inputs to the given node

> inputs graph node = nodeInputs $ (nodeMap graph) Map.! (nodeId node)

outputs
  Returns the Nodes which are outputs of the given node

> outputs graph node = nodeOutputs $ (nodeMap graph) Map.! (nodeId node)

graphFromArcs
  Generates a WFGraph from a list of NodeArcs

> graphFromArcs arcs = WFGraph $ Map.fromList $ zip (map (nodeId.node) arcs) arcs

> startWorkflow :: WFGraph -> Either String (IO [Token])
> startWorkflow graph
>     | null startNodes       = Left "Error: Workflow has no start node"
>     | length startNodes > 1 = Left "Error: Workflow has more than one start node"
>     | otherwise             = Right $ acceptToken (Token [1] NullNode NullNode startNode) graph []
>   where
>     startNodes = filter (\x -> x < 0) $ Map.keys (nodeMap graph)
>     startNode  = node $ (nodeMap graph) Map.! (head startNodes)

removeFirst
  Removes the first instance in a list for which the given predicate
  function returns true

> removeFirst :: (a->Bool) -> [a] -> [a]
> removeFirst predicate [] = []
> removeFirst predicate (x:xs)
>   | predicate x = xs
>   | otherwise = x : (removeFirst predicate xs)

nextForkId
  Generates the token id for the next token for in the case where we have multiple outputs.
  A token id is a list of integers. For each node which has a single output, the output token
  will have the same id as the input token.
  A node with multiple outputs will add a counter to the tail of the id, incremented for
  each child. This guarantees that each token will have a unique id

  For example, a join with 2 outputs might go
    [1] -> [1,0]             or  [1,2,5] -> [1,2,5,0]
        -> [1,1]                         -> [1,2,5,1]

> nextForkId (Token tid _ _ _) counter = tid ++ [counter]

removeInputTokens
  Given a list of input nodes, a target node and a list of tokens,
  for each node removes the first token which has the input node as
  its previous node and the target node as its current node

> removeInputTokens []     _          tokenList = tokenList
> removeInputTokens (x:xs) targetNode tokenList =
>   removeInputTokens xs targetNode $ removeFirst (\tok->prevNode tok == x && nextNode tok == targetNode) tokenList

acceptP
  Accept function for a 'passthrough' node. This type of node is assumed to have one input and one output.
  The function just passes through to the next node in line.

> passthrough :: Token -> WFGraph -> [Token] -> IO [Token]
> passthrough token graph tokenList =
>   do putStrLn $ "Passing through node " ++ show (nextNode token)
>      completeExecution token graph tokenList

completeExecution
  Generates a new token for each output node of the current node of the given token

> completeExecution :: Token -> WFGraph -> [Token] -> IO [Token]
> completeExecution token graph tokenList
>   | hasNoOutputs = do putStrLn $ (show currentNode) ++ " has no outputs. Discarding tokens"
>                       return tokenList
>   | hasOneOutput = do putStrLn $ "Sending token to " ++ show (head outputNodes) ++ " from " ++ show currentNode
>                       acceptToken newToken graph tokenList
>   | otherwise    = split outputNodes tokenList 0
>   where
>     hasNoOutputs                   = null outputNodes
>     hasOneOutput                   = null $ tail outputNodes
>
>     currentNode                    = currNode token
>     outputNodes                    = outputs graph currentNode
>     newToken                       = Token (tokenId token) currentNode NullNode (head outputNodes)
>     newForkToken nextNode counter  = Token (nextForkId token counter) currentNode NullNode nextNode
>     split [] tokenList _           = return tokenList
>     split (x:xs) tokenList counter = do putStrLn $ "Sending token to " ++ show x ++ " from " ++ show currentNode
>                                         newTokenList <- acceptToken (newForkToken x counter) graph tokenList
>                                         split xs newTokenList (counter + 1)

acceptToken
  Requires that a token from every input (counting the currently processed token) exist before
  passing a token to the output node.

> acceptToken :: Token -> WFGraph -> [Token] -> IO [Token]
> acceptToken token graph tokenList
>   | areAllInputsPresent = do putStrLn $ "All inputs received at " ++ show targetNode ++ ". Calling accept function"
>                              (accept targetNode) newToken graph outputTokenList
>   | otherwise           = do putStrLn $ "Join node " ++ show targetNode ++ " doesn't have all inputs yet"
>                              return $ token : tokenList
>   where
>     areAllInputsPresent           = all (inputHasToken (token:tokenList)) inputNodes
>
>     inputHasToken []         node = False
>     inputHasToken (tok:rest) node = (nextNode tok == targetNode && prevNode tok == node) ||
>                                     inputHasToken rest node
>
>     targetNode                    = nextNode token
>     inputNodes                    = inputs graph targetNode
>     outputTokenList               = removeInputTokens inputNodes targetNode tokenList
>
>     newToken                      = Token (tokenId token) (prevNode token) (nextNode token) NullNode