> module Test.LoadTest where
> import qualified XmlWf
> import Text.XML.HaXml.Xml2Haskell
> import Text.XML.HaXml.Parse
> import Text.XML.HaXml.Combinators
> import Text.XML.HaXml.Types

> loadFile filename =
>   do xmlStr <- readFile filename
>      return $ xmlParse' filename xmlStr

> process filename =
>   do doc <- loadFile filename
>      case (doc) of
>          Left msg -> return $ Left msg
>          Right (Document _ _ elem _ ) -> return $ Right $ processDoc (CElem elem)

> processDoc doc = tag "workflow" doc