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
>          Right (Document _ _ elem _ ) -> return $ processDoc (CElem elem)

> processDoc doc = Right ( "version: " ++ wfVersion ++ " id: " ++ wfId )
>   where
>     wfElem    = head $ tag "workflow" doc
>     wfVersion = getAttr wfElem "version"
>     wfId      = getAttr wfElem "id"

> getAttr (CElem (Elem _ attrList _ )) name
>    | null attrs = ""
>    | otherwise  = attrVal' (head attrs)
>   where
>     attrs = filter (\(attrName, attrValue) -> attrName == name) attrList
>     attrVal' (_, AttValue atlist) = case (head atlist) of (Left val) -> val

> loadWorkflow filename =
>   do result <- process filename
>      case (result) of
>          Left msg -> putStrLn msg
>          Right msg -> putStrLn msg

> testLoadWf = do fileList <- getDirectoryContents wfDir
>                 mapM (loadWorkflow) $ (useFullPath.filterWfs) fileList
>   where
>     wfDir = "/home/paul/workspace/functional-workflow/test-workflows/"
>     filterWfs = (filter (hasExtension ".wf"))
>     useFullPath = (map (\f->wfDir ++ f))

> hasExtension ext name = all (\(x,y) -> x == y) $ zip (reverse ext) (reverse name)
