module Text.Extor
    ( -- Types
      Query(..),
      HtmlContent(..),
      -- Functions
      extract,
      showContent
    ) where

import qualified Data.Text as Text
import qualified Data.List as List
import Text.HTML.TagSoup.Tree(parseTree)
import Text.HTML.TagSoup.Tree(TagTree(..))
import Text.HTML.TagSoup(Tag(..))
import Data.Monoid((<>))
import qualified Debug.Trace as Trace
-- Query for given content
data Query = Query {
  samples :: [Text.Text]
} deriving (Show)

-- Html content as string
type HtmlContent = Text.Text

--runQuery :: Query -> [TagTree Text.Text] ->
-- Runs query on given dom structure and returns results that matches to given query

runQuery q [] = 1


walkTree :: Query -> [TagTree Text.Text] -> (Bool, [([Text.Text], Text.Text)])
walkTree q [] = (False, [])
walkTree (q@Query{samples=samples}) ((TagLeaf (TagText str)):rest) =
  -- Do not include empty lines
  if s /= "" && List.elem s samples then (False, (([],s):r)) else (False, r)
  where
    s = Text.strip str
    (restCompleted, r) = walkTree q rest
walkTree q ((TagBranch tag _ children):rest) = (False, tags)
  where
    (childrenCompleted, childrenTags) = walkTree q children
    -- Add
    (restCompleted, restTags) = walkTree q rest
    childrenTags' = fmap (\(path, val)-> (tag:path, val)) childrenTags
    tags = childrenTags' ++ restTags



extract :: Query -> HtmlContent -> [([Text.Text], Text.Text)]
extract q c = response
  where
    tags = parseTree c
    (completed, response) = walkTree q tags


showContent content = putStrLn $ show $ parseTree content
