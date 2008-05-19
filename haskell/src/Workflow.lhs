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

> data Node a = NullNode
>             | Node {
>                 nodeId  :: Integer,
>                 accept  :: (Token a -> WfInstance a -> IO (WfInstance a))
>               }

> instance Eq (Node a) where
>   NullNode  == NullNode = True
>   NullNode  == _        = False
>   _         == NullNode = False
>   node1     == node2    = (nodeId node1) == (nodeId node2)

> instance Show (Node a) where
>   show NullNode = "[Node: NullNode]"
>   show a        = "[Node id: " ++ (show (nodeId a)) ++ "]"

NodeArcs
  Represents the incoming and outgoing connections to other nodes.

  Members:
    node: The related node
    nodeInputs: The list of incoming node connections
    nodeOutputs: The list of outgoing node connections

> data NodeArcs a =
>   NodeArcs {
>     node :: Node a,
>     nodeInputs :: [Node a],
>     nodeOutputs :: [Node a]
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

> data Token a =
>   Token {
>     tokenId::[Int],
>     prevNode::Node a,
>     currNode::Node a,
>     nextNode::Node a
>  }
>  deriving (Show)

> instance Eq (Token a) where
>   t1 == t2 = (tokenId t1) == (tokenId t2)

WFGraph
  This is just a container for NodeArcs, which can be queried
  for the inputs and outputs of a given node

> type WfGraph a = Map Integer (NodeArcs a)

> data WfInstance a =
>   WfInstance {
>     wfGraph   :: WfGraph a,
>     tokenList :: [Token a],
>     userData  :: a
>   }

inputs
  Returns the Nodes which are inputs to the given node

> inputs graph node = nodeInputs $ graph Map.! (nodeId node)

outputs
  Returns the Nodes which are outputs of the given node

> outputs graph node = nodeOutputs $ graph Map.! (nodeId node)

graphFromArcs
  Generates a WFGraph from a list of NodeArcs

> graphFromArcs arcs = Map.fromList $ zip (map (nodeId.node) arcs) arcs

> tokenForId tid (WfInstance graph tokenList userData) =
>   head $ filter (\t -> (tokenId t) == tid) tokenList

> startWorkflow :: WfGraph a -> a -> Either String (IO (WfInstance a))
> startWorkflow graph userData
>     | null startNodes       = Left "Error: Workflow has no start node"
>     | length startNodes > 1 = Left "Error: Workflow has more than one start node"
>     | otherwise             = Right $ acceptToken (Token [1] NullNode NullNode startNode) $ WfInstance graph [] userData
>   where
>     startNodes = filter (\x -> x < 0) $ Map.keys graph
>     startNode  = node $ graph Map.! (head startNodes)

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

  passthrough :: Token a -> WfInstance a -> IO (WfInstance a)

> passthrough token wf = do putStrLn $ "Passing through node " ++ show (currNode token)
>                           completeExecution token wf

completeExecution
  Generates a new token for each output node of the current node of the given token

> completeExecution :: Token a -> WfInstance a -> IO (WfInstance a)
> completeExecution token wf@(WfInstance graph tokenList userData)
>   | hasNoOutputs = do putStrLn $ (show currentNode) ++ " has no outputs. Discarding tokens"
>                       return wf
>   | hasOneOutput = do putStrLn $ "Sending token to " ++ show (head outputNodes) ++ " from " ++ show currentNode
>                       acceptToken newToken wf
>   | otherwise    = split outputNodes wf 0
>   where
>     hasNoOutputs                   = null outputNodes
>     hasOneOutput                   = null $ tail outputNodes
>
>     currentNode                    = currNode token
>     outputNodes                    = outputs graph currentNode
>     newToken                       = Token (tokenId token) currentNode NullNode (head outputNodes)
>     newForkToken nextNode counter  = Token (nextForkId token counter) currentNode NullNode nextNode
>     split [] wf _                  = return wf
>     split (x:xs) wf counter        = do putStrLn $ "Sending token to " ++ show x ++ " from " ++ show currentNode
>                                         newWf <- acceptToken (newForkToken x counter) wf
>                                         split xs newWf (counter + 1)

acceptToken
  Requires that a token from every input (counting the currently processed token) exist before
  passing a token to the output node.

> acceptToken :: Token a -> WfInstance a -> IO (WfInstance a)
> acceptToken token wf@(WfInstance graph tokenList userData)
>   | areAllInputsPresent = do putStrLn $ "All inputs received at " ++ show targetNode ++ ". Calling accept function"
>                              (accept targetNode) newToken (WfInstance graph (newToken:outputTokenList) userData)
>   | otherwise           = do putStrLn $ "Join node " ++ show targetNode ++ " doesn't have all inputs yet"
>                              return $ WfInstance graph (token : tokenList) userData
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