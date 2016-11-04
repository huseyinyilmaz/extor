module Text.Extor
    ( -- Types
      -- Query(..),
      HtmlContent(..),
      -- Functions
      -- extract,
      -- showContent,
      getItems
    ) where

import qualified Data.Text as Text
import qualified Data.List as List
import Text.HTML.TagSoup.Tree(parseTree)
import Text.HTML.TagSoup.Tree(TagTree(..))
import Text.HTML.TagSoup(Tag(..))
import Data.Monoid((<>))
import qualified Control.Monad.State as State
import Control.Monad
import Data.Monoid
import qualified Debug.Trace as Trace
import qualified Data.Map.Strict as Map


type HtmlContent = Text.Text
type AttributeName = Text.Text
type AttributeValue = Text.Text
type Sample = [[Text.Text]]

-- Query for infividual data.
data ItemQuery = ItemQuery {
  parent :: [Element],
  path :: [Element]
} deriving (Show)
-- Query for each row of sample data.
data RowQuery = RowQuery {
  parent :: [Element],
  itemQueries :: [ItemQuery]
} deriving (Show)
-- Query for list of rows.
-- data Query = Query {
--   parent :: [Element],
--   rows :: [RowQuery]
-- } deriving (Show)


-- Represents a node in item's path
data Element = Element {
  name :: Text.Text,
  attributes :: [(AttributeName, AttributeValue)],
  number :: Int
} deriving (Show, Eq)

-- Represents a value in data.
data Item = Item {
  location :: [Element],
  value :: HtmlContent
} deriving (Show, Eq)



sample = readFile "/Users/huseyinyilmaz/Downloads/tagsoup_ Parsing and extracting information from (possibly malformed) HTML_XML documents.htm"

-- Parses given TagSoup tree to list of items.
walkParseTree :: [Element] -> TagTree Text.Text -> State.State (Map.Map Text.Text Int) [Item]
walkParseTree location (TagLeaf (TagText str)) = do
  Trace.traceM "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
  Trace.traceM ("Start of walkTree (Leaf):" <> (show location) <> ", " <> show str)
  Trace.traceM "YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY"
  return $ Trace.traceShowId [Item{location=location, value=str}]
walkParseTree location (TagBranch tag _ children) = do
  Trace.traceM ("Start of walkTree (Branch):" <> (Text.unpack tag))
  idx <- Trace.trace (Text.unpack tag) $ getIndex tag
  let p = Element{
        name=tag,
        attributes=[],
        number=idx}
  results <- (mapM (walkParseTree (p:location)) children)
  return (mconcat results)
  where
    getIndex :: Text.Text -> State.State (Map.Map Text.Text Int) Int
    getIndex t = do
      m <- State.get
      case Map.lookup t m of
        Just idx -> do
          let newState = Trace.traceShowId (Map.insert t (idx+1) m)
          State.put newState
          return idx
        Nothing -> do
          let newState = Trace.trace "YYY" (Map.insert t 0 m)
          State.put newState
          return 0
walkParseTree location e = do
  Trace.traceM "Start of walkTree (Other)"
  return (Trace.traceShow e [])

-- Parses given html content into list of items.
getItems :: HtmlContent -> [Item]
getItems txt = do
  result <- State.evalState (mapM (walkParseTree []) t) Map.empty
  result
  where
    t = parseTree txt

generateQuery :: [Sample] -> [Item] -> [RowQuery]
generateQuery ss is = []
 -- where
    --relevant = List.filter (\Item{value=v} -> List.elem v ss) is




--runQuery :: Query -> [TagTree Text.Text] ->
-- Runs query on given dom structure and returns results that matches to given query

runQuery q [] = 1


-- walkTree :: Query -> [TagTree Text.Text] -> (Bool, [([Text.Text], Text.Text)])
-- walkTree q [] = (False, [])
-- walkTree (q@Query{samples=samples}) ((TagLeaf (TagText str)):rest) =
--   -- Do not include empty lines
--   if s /= "" && List.elem s samples then (False, (([],s):r)) else (False, r)
--   where
--     s = Text.strip str
--     (restCompleted, r) = walkTree q rest
-- walkTree q ((TagBranch tag _ children):rest) = (False, tags)
--   where
--     (childrenCompleted, childrenTags) = walkTree q children
--     -- Add
--     (restCompleted, restTags) = walkTree q rest
--     childrenTags' = fmap (\(path, val)-> (tag:path, val)) childrenTags
--     tags = childrenTags' ++ restTags



-- extract :: Query -> HtmlContent -> [([Text.Text], Text.Text)]
-- extract q c = response
--   where
--     tags = parseTree c
--     (completed, response) = walkTree q tags


-- showContent content = putStrLn $ show $ parseTree content
