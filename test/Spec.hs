module Main where

import Test.HUnit
import qualified Text.Extor as Extor
import Data.Monoid((<>))

import Text.HTML.TagSoup.Tree(parseTree)

testGetItems:: Test
testGetItems = TestCase $ do
  -- let content = ("<html><body><ul>" <>
  --               "<li>one</li>\n" <>
  --               "<li>two</li>" <>
  --               "<li>three</li>\n" <>
  --               "<li>four</li>" <>
  --               "</ul>" <>
  --               "<div> five </div>" <>
  --               "</body></html>")
  let content = ("<li class=\"attr-zero\">zero</li>" <>
                 "<li class=\"attr-one\">one</li>" <>
                 "<li class=\"attr-two\">two</li>"
                 )
      parsed = [Extor.Item{location=[Extor.Element{name = "li",
                                                   attributes = [("class","attr-zero")],
                                                   number = 0}],
                           value="zero"},
                Extor.Item{location=[Extor.Element{name = "li",
                                                   attributes =[("class","attr-one")],
                                                   number = 1}],
                           value="one"},
                Extor.Item{location=[Extor.Element{name="li",
                                                   attributes=[("class","attr-two")],
                                                   number = 2}],
                           value = "two"}]

  (assertEqual
    "Test Get items"
    parsed
    (Extor.getItems content))

-- testExtractOneBranchLeaf:: Test
-- testExtractOneBranchLeaf = TestCase $ do
--   let content = ("<html><body><ul>" <>
--                 "<li>one</li>\n" <>
--                 "<li>two</li>" <>
--                 "<li>three</li>\n" <>
--                 "<li>four</li>" <>
--                 "</ul>" <>
--                 "<div> five </div>" <>
--                 "</body></html>")
--   (assertEqual
--     "Extract one branch leaf"
--     [([], "one"), ([], "two"), ([], "three"), ([], "four")]
--     --["one", "two", "three", "four"]
--     (Extor.extract (Extor.Query {Extor.samples=["one","three"]}) content))


-- testExtractTwoBranchLeaf:: Test
-- testExtractTwoBranchLeaf = TestCase $ do
--   let content = ("<html><body>" <>

--                  "<div><ul>" <>
--                  "<li>one</li>\n" <>
--                  "<li>two  </li>" <>
--                  "</ul></div>" <>

--                  "<div><ul>" <>
--                  "<li>three</li>\n" <>
--                  "<li>four</li>" <>
--                  "</ul></div>" <>

--                  "<div> five </div>" <>

--                  "</body></html>")
--   --Extor.showContent content
--   (assertEqual
--    "Extract two branch leaf"
--    [([], "one"), ([], "two"), ([], "three"), ([], "four")]
--     (Extor.extract (Extor.Query {Extor.samples=["one","three"]}) content))

main :: IO Counts
main =  runTestTT $ TestList [testGetItems]
                              --testExtractOneBranchLeaf ] --,
                              -- testExtractTwoBranchLeaf]
