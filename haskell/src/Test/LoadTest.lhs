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

> processDoc doc
>     | null wfElems       = Left "No workflow tag found"
>     | length wfElems > 1 = Left "Too many workflow elements found"
>     | otherwise          = Right $ "Found workflow "
>   where
>     wfElems   = tag "workflow" doc
>     wfVersion = attr "version" (head wfElems)
>     wfId      = attr "id"      (head wfElems)

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
