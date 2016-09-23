module Main where

import Test.HUnit
import qualified Text.Extor as Extor
import Data.Monoid((<>))

import Text.HTML.TagSoup.Tree(parseTree)

testExtractOneBranchLeaf:: Test
testExtractOneBranchLeaf = TestCase $ do
  let content = ("<html><body><ul>" <>
                "<li>one</li>\n" <>
                "<li>two</li>" <>
                "<li>three</li>\n" <>
                "<li>four</li>" <>
                "</ul>" <>
                "<div> five </div>" <>
                "</body></html>")
  (assertEqual
    "Extract one branch leaf"
    [([], "one"), ([], "two"), ([], "three"), ([], "four")]
    --["one", "two", "three", "four"]
    (Extor.extract (Extor.Query {Extor.samples=["one","three"]}) content))


testExtractTwoBranchLeaf:: Test
testExtractTwoBranchLeaf = TestCase $ do
  let content = ("<html><body>" <>

                 "<div><ul>" <>
                 "<li>one</li>\n" <>
                 "<li>two  </li>" <>
                 "</ul></div>" <>

                 "<div><ul>" <>
                 "<li>three</li>\n" <>
                 "<li>four</li>" <>
                 "</ul></div>" <>

                 "<div> five </div>" <>

                 "</body></html>")
  --Extor.showContent content
  (assertEqual
   "Extract two branch leaf"
   [([], "one"), ([

], "two"), ([], "three"), ([], "four")]
    (Extor.extract (Extor.Query {Extor.samples=["one","three"]}) content))

main :: IO Counts
main =  runTestTT $ TestList [testExtractOneBranchLeaf ] --,
                              -- testExtractTwoBranchLeaf]
