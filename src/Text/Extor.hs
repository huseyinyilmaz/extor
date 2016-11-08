module Text.Extor
    ( -- Types
      Item(..),
      Element(..),
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

-----------
-- TYPES --
-----------
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

-- Represents a node in item's path
data Element = Element {
  name :: Text.Text,
  attributes :: [(AttributeName, AttributeValue)],
  number :: Int
} deriving (Show, Eq)
-- Location of an item
-- Serialized version of all items
type Location = [Element]

-- Represents a value in data.
data Item = Item {
  location :: Location,
  value :: HtmlContent
} deriving (Show, Eq)

sample = readFile "/Users/huseyinyilmaz/Downloads/tagsoup_ Parsing and extracting information from (possibly malformed) HTML_XML documents.htm"

walkElement :: Location -> TagTree Text.Text -> State.State (Map.Map Text.Text Int) [Item]
walkElement location (TagLeaf (TagText str)) = return [Item{location=location, value=str}]
walkElement location (TagBranch tag attrs children) = do
  idx <- getIndex tag
  let element = Element{name=tag, attributes=attrs, number=idx}
  return (walkTree (element:location) children)
  where
    getIndex :: Text.Text -> State.State (Map.Map Text.Text Int) Int
    getIndex t = do
      m <- State.get
      case Map.lookup t m of
        Just idx -> do
          let newState = Map.insert t (idx+1) m
          State.put newState
          return idx
        Nothing -> do
          let newState = Map.insert t 1 m
          State.put newState
          return 0

walkElement location e = return []

-- Parses given TagSoup tree to list of items.
walkTree :: Location -> [TagTree Text.Text] -> [Item]
walkTree location nodes = mconcat $ State.evalState (sequence $ fmap (walkElement location) nodes) Map.empty

-- Parses given html content into list of items.
getItems :: HtmlContent -> [Item]
getItems txt = walkTree [] (parseTree txt)
