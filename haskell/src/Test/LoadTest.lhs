> module Test.LoadTest where
> import qualified XmlWf
> import Text.XML.HaXml.Xml2Haskell
> import Text.XML.HaXml.Parse
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types
> import System.Directory

> loadFile filename =
>   do xmlStr <- readFile filename
>      return $ xmlParse' filename xmlStr

> process filename =
>   do doc <- loadFile filename
>      case (doc) of
>          Left msg -> return $ Left msg
>          Right doc -> return $ unwrapXmlProc $ processDoc doc

> processDoc doc =
>   do useDoc doc
>      wfVersion <- getAttr "version"
>      wfId      <- getAttr "id"
>      return $ Right ( "version: " ++ wfVersion ++ " id: " ++ wfId )

> loadWorkflow filename =
>   do result <- process filename
>      case (result) of
>          Left msg -> putStrLn msg
>          Right msg -> putStrLn msg

> testLoadWf = do fileList <- getDirectoryContents wfDir
>                 mapM (loadWorkflow) $ (useFullPath.filterWfs) fileList
>   where
>     wfDir = "/home/paul/workspace/functional-workflow/test-wf/"
>     filterWfs = (filter (hasExtension ".wf"))
>     useFullPath = (map (\f->wfDir ++ f))

> hasExtension ext name = all (\(x,y) -> x == y) $ zip (reverse ext) (reverse name)

> data XmlProc a = XmlProc ([Element]->(a, [Element]))

> instance Monad (XmlProc) where
>     return a = XmlProc (\_->(a,[]))
>     XmlProc a >>= f = XmlProc (\elemArray->
>         case (a elemArray) of
>             (currValue, xmlElem) -> case (f currValue) of
>                  XmlProc result -> result xmlElem)

> useDoc (Document _ _ elem _ ) = XmlProc (\_-> ((), [elem]))

> getAttr name = XmlProc (\(x:xs)-> (getElemAttr x name, (x:xs)))

> getElemAttr (Elem _ attrList _ ) name
>    | null attrs = ""
>    | otherwise  = attrVal' (head attrs)
>   where
>     attrs = filter (\(attrName, attrValue) -> attrName == name) attrList
>     attrVal' (_, AttValue atlist) = case (head atlist) of (Left val) -> val

> unwrapXmlProc (XmlProc a) = case (a []) of (result,_) -> result