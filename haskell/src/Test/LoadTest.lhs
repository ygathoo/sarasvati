> module Test.LoadTest where
> import qualified XmlWf
> import Text.XML.HaXml.Xml2Haskell
> import Text.XML.HaXml.Parse
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types
> import System.Directory
> import WorkflowXml

> loadFile filename =
>   do xmlStr <- readFile filename
>      return $ xmlParse' filename xmlStr

> process filename f =
>   do doc <- loadFile filename
>      case (doc) of
>          Left msg -> return $ Left msg
>          Right doc -> return $ unwrapXmlProc $ f doc

 loadWorkflow filename =
   do result <- process filename
      case (result) of
          Left msg -> putStrLn msg
          Right msg -> putStrLn msg

 testLoadWf = do fileList <- getDirectoryContents wfDir
                 mapM (loadWorkflow) $ (useFullPath.filterWfs) fileList
   where
     wfDir = "/home/paul/workspace/functional-workflow/test-wf/"
     filterWfs = (filter (hasExtension ".wf"))
     useFullPath = (map (\f->wfDir ++ f))

> hasExtension ext name = all (\(x,y) -> x == y) $ zip (reverse ext) (reverse name)
