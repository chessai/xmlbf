{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative (liftA2, (<|>))
import Control.Monad (mplus)
import qualified Control.Monad.Trans.State as S
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft)
import qualified Data.Text as T
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.Tasty.HUnit as HU
import           Test.Tasty.HUnit ((@?=), (@=?))
import qualified Test.QuickCheck.Instances ()

import qualified Xmlbf as X

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt_main

--------------------------------------------------------------------------------
tt_main :: Tasty.TestTree
tt_main = Tasty.testGroup "main"
  [ tt_text
  , tt_element
  , tt_encoding
  , tt_parsing
  , tt_backtracking
  , tt_fixpoints
  ]

tt_text :: Tasty.TestTree
tt_text = Tasty.testGroup "text'"
  [ QC.testProperty "text: empty or one" $
      QC.forAllShrink QC.arbitrary QC.shrink $ \t ->
         length (X.text t) <= 1

  , QC.testProperty "text: like text'" $
      QC.forAllShrink QC.arbitrary QC.shrink $ \t ->
         case X.text' t of
            Left _  -> X.text t === []
            Right n -> X.text t === [n]
  ]

tt_element :: Tasty.TestTree
tt_element = Tasty.testGroup "element"
  [ HU.testCase "empty name" $ do
      HU.assert (isLeft (X.element' "" [] []))

  , HU.testCase "name with leading whitespace" $ do
      HU.assert (isLeft (X.element' " x" [] []))

  , HU.testCase "name with trailing whitespace" $ do
      HU.assert (isLeft (X.element' "x " [] []))

  , HU.testCase "empty attribute" $ do
      HU.assert (isLeft (X.element' "x" [("","a")] []))

  , HU.testCase "attribute with leading whitespace" $ do
      HU.assert (isLeft (X.element' "x" [(" x","a")] []))

  , HU.testCase "attribute with trailing whitespace" $ do
      HU.assert (isLeft (X.element' "x" [("x ","a")] []))
  ]

tt_encoding :: Tasty.TestTree
tt_encoding = Tasty.testGroup "Encoding"
  [ HU.testCase "empty" $ do
      bsEncode [] @?= ""

  , HU.testCase "xml: <x a=\"y\"></x>" $ do
      bsEncode (X.element "x" [("a","y")] [])
        @?= "<x a=\"y\"></x>"

  , HU.testCase "xml: <x b=\"\" a=\"y\"></x>" $ do
      bsEncode (X.element "x" [("a","y"), ("b","")] [])
        @?= "<x b=\"\" a=\"y\"></x>"

  , HU.testCase "xml: <x b=\"z\" a=\"y\"></x>" $ do
      bsEncode (X.element "x" [("a","y"), ("b","z")] [])
        @?= "<x b=\"z\" a=\"y\"></x>"

  , HU.testCase "xml: <x>foo</x>" $ do
      bsEncode (X.element "x" [] (X.text "foo"))
        @?= "<x>foo</x>"

  , HU.testCase "xml: <x>foobar</x>" $ do
      bsEncode (X.element "x" [] (X.text "foo" <> X.text "bar"))
        @?= "<x>foobar</x>"

  , HU.testCase "xml: <x><y></y></x>" $ do
      bsEncode (X.element "x" [] (X.element "y" [] []))
        @?= "<x><y></y></x>"

  , HU.testCase "xml: <x><y></y><z></z></x>" $ do
      bsEncode (X.element "x" []
                  (X.element "y" [] [] <> X.element "z" [] []))
        @?= "<x><y></y><z></z></x>"

  , HU.testCase "xml: <x></x><y></y>" $ do
      bsEncode (X.element "x" [] [] <> X.element "y" [] [])
        @?= "<x></x><y></y>"
  ]

--------------------------------------------------------------------------------

tt_parsing :: Tasty.TestTree
tt_parsing = Tasty.testGroup "Parsing"
  [ HU.testCase "endOfInput" $ do
      Right () @=? X.runParser X.pEndOfInput []

  , HU.testCase "endOfInput: Not end of input yet" $ do
      Left "Not end of input yet" @=? X.runParser X.pEndOfInput (X.text "&")

  , HU.testCase "text': empty" $ do
      Left "Missing text node" @=? X.runParser X.pText []

  , HU.testCase "text': blank" $ do
      Left "Missing text node" @=? X.runParser X.pText (X.text "")

  , HU.testCase "text': space" $ do
      Right " \t\n" @=?
         X.runParser X.pText (X.text " " <> X.text "\t" <> X.text "\n")

  , HU.testCase "text': missing" $ do
      Left "Missing text node" @=? X.runParser X.pText (X.element "a" [] [])

  , HU.testCase "text'" $ do
      Right "&" @=? X.runParser X.pText (X.text "&")

  , HU.testCase "text': concat" $ do
      Right "&<" @=? X.runParser X.pText
         (X.text "&" <> X.text "" <> X.text "<")

  , HU.testCase "text': twice" $ do
      Left "Missing text node" @=? X.runParser (X.pText >> X.pText)
         (X.text "&" <> X.text "" <> X.text "<")

  , HU.testCase "any element: empty" $ do
      Left "Missing element" @=? X.runParser (X.pAnyElement (pure ())) []

  , HU.testCase "any element: text" $ do
      Left "Missing element"
        @=? X.runParser (X.pAnyElement (pure ())) (X.text "a")

  , HU.testCase "any element: pure" $ do
      Right () @=?  X.runParser (X.pAnyElement (pure ())) (X.element "x" [] [])

  , HU.testCase "any element: name" $ do
      Right "x"
        @=? X.runParser (X.pAnyElement X.pName) (X.element "x" [] [])

  , HU.testCase "element: empty" $ do
      Left "Missing element \"x\"" @=? X.runParser (X.pElement "x" (pure ())) []

  , HU.testCase "element: Missing element" $ do
      Left "Missing element \"x\""
         @=? X.runParser (X.pElement "x" (pure ())) (X.element "y" [] [])

  , HU.testCase "element: pure" $ do
      Right ()
         @=? X.runParser (X.pElement "x" (pure ())) (X.element "x" [] [])

  , HU.testCase "element: name" $ do
      Right "x"
         @=? X.runParser (X.pElement "x" X.pName) (X.element "x" [] [])

  , HU.testCase "element: leading whitespace" $ do
      Right ()
         @=? X.runParser (X.pElement "x" (pure ()))
                         (X.text " \n \t" <> X.element "x" [] [])

  , HU.testCase "element: text'" $ do
      Right "ab"
        @=? X.runParser (X.pElement "x" X.pText)
                (X.element "x" [] (X.text "a" <> X.text "b"))

  , HU.testCase "element: nested" $ do
      Right ([("a","b")], "z")
        @=? X.runParser
                (X.pElement "x" (X.pElement "y" (liftA2 (,) X.pAttrs X.pText)))
                (X.element "x" [] (X.element "y" [("a","b")] (X.text "z")))

  , HU.testCase "element: nested with leading whitespace" $ do
      Right ([("a","b")], "z")
        @=? X.runParser
                (X.pElement "x" (X.pElement "y" (liftA2 (,) X.pAttrs X.pText)))
                (X.text " " <>
                 X.element "x" [] (X.text " " <>
                                    X.element "y" [("a","b")] (X.text "z")))

  , HU.testCase "element: twice" $ do
      Left "Missing element \"x\""
         @=? X.runParser (X.pElement "x" (pure ()) >> X.pElement "x" (pure ()))
                (X.element "x" [] [])

  , HU.testCase "attr" $ do
      Right "a"
         @=? X.runParser (X.pElement "x" (X.pAttr "y"))
                (X.element "x" [("y","a"), ("z","b")] [])

  , HU.testCase "attr: Missing" $ do
      Left "Missing attribute \"y\""
         @=? X.runParser (X.pElement "x" (X.pAttr "y")) (X.element "x" [] [])

  , HU.testCase "attrs: empty" $ do
      Right []
         @=? X.runParser (X.pElement "x" X.pAttrs) (X.element "x" [] [])

  , HU.testCase "attrs" $ do
      Right [("y","a"), ("z","b")]
         @=? X.runParser (X.pElement "x" X.pAttrs)
                (X.element "x" [("z","b"), ("y","a")] [])

  , HU.testCase "attrs: twice" $ do
      Right []
         @=? X.runParser (X.pElement "x" (X.pAttrs >> X.pAttrs))
                (X.element "x" [("z","b"), ("y","a")] [])

  , HU.testCase "fail: empty" $ do
      (Left "x" :: Either String ())
        @=? X.runParser (fail "x") []

  , HU.testCase "fail" $ do
      (Left "x" :: Either String ())
        @=? X.runParser (fail "x") (X.text "y")

  , HU.testCase "children: empty" $ do
      Right []
         @=? X.runParser (X.pElement "x" X.pChildren) (X.element "x" [] [])

  , HU.testCase "children: top empty" $ do
      Right [] @=? X.runParser X.pChildren []

  , HU.testCase "children: top 1 node" $ do
      Right (X.element "x" [] [])
         @=? X.runParser X.pChildren (X.element "x" [] [])

  , HU.testCase "children: top 1 node twice" $ do
      Right []
         @=? X.runParser (X.pChildren >> X.pChildren) (X.element "x" [] [])

  , HU.testCase "children: top 2 nodes" $ do
      Right (X.element "x" [] [] <> X.text "ab" <> X.element "y" [] [])
         @=? X.runParser X.pChildren
                (X.element "x" [] [] <> X.text "a" <> X.text "b" <>
                 X.element "y" [] [])

  , HU.testCase "children: 1 node" $ do
      Right (X.text "foo")
         @=? X.runParser (X.pElement "x" X.pChildren)
                (X.element "x" [] (X.text "foo"))

  , HU.testCase "children: 1 node twice" $ do
      Right []
         @=? X.runParser (X.pElement "x" (X.pChildren >> X.pChildren))
                (X.element "x" [] (X.text "foo"))

  , HU.testCase "children: 2 successive text' nodes" $ do
      Right (X.text "foobar")
         @=? X.runParser (X.pElement "x" X.pChildren)
                (X.element "x" [] (X.text "foo" <> X.text "bar"))

  , HU.testCase "children: 2 text' nodes twice" $ do
      Right []
         @=? X.runParser (X.pElement "x" (X.pChildren >> X.pChildren))
                (X.element "x" [] (X.text "foo" <> X.text "bar"))

  , HU.testCase "children: 3 nodes" $ do
      let ns0 = X.text "foo" <> X.element "a" [] [] <> X.text "bar"
      Right ns0
         @=? X.runParser (X.pElement "x" X.pChildren)
                         (X.element "x" [] ns0)

  , HU.testCase "children: 3 nodes twice" $ do
      Right []
         @=? X.runParser (X.pElement "x" (X.pChildren >> X.pChildren))
                (X.element "x" []
                   (X.text "foo" <> X.element "a" [] [] <> X.text "bar"))

  ]

tt_backtracking :: Tasty.TestTree
tt_backtracking = Tasty.testGroup "Backtracking"
  [ HU.testCase "Alternative" $
      Right "y" @=? X.runParser
        -- The second pText fails because the state is empty after the first
        ((X.pText >> X.pText >> pure "a") <|> X.pText)
        (X.text "y")
  , HU.testCase "MonadPlus" $
      Right "y" @=? X.runParser
        -- The second pText fails because the state is empty after the first
        (mplus (X.pText >> X.pText >> pure "a") X.pText)
        (X.text "y")
  ]


node0 :: X.Node
Right node0 =
  X.element' "a" [] $ mconcat
   [ X.element "b" []
      (X.element "c" [] [] <>
       X.element "d" [] [])
   , X.element "e" []
      (X.element "f" [] [] <>
       X.element "g" [] [])
   ]



fixvisit :: (X.Node -> S.State T.Text [X.Node])
         -> (X.Node -> S.State T.Text [X.Node])
fixvisit _ n@(X.Element t _ _) = do
   S.modify (\ts -> mappend ts t)
   pure [n]

tt_fixpoints :: Tasty.TestTree
tt_fixpoints = Tasty.testGroup "fixpoints"
  [ HU.testCase "dfpos: depth-first post-order?" $ do
      let (ns, ts) = S.runState (X.dfposM fixvisit node0) []
      ns @?= [node0]
      ts @?= "cdbfgea"

  , HU.testCase "dfpos: output single node" $ do
      let Right n0 = X.element' "x" [] (X.text "a" <> X.text "b")
          f = \k -> \case
             X.Text "ab" -> X.text "foo" >>= k
             X.Text "foo" -> X.text "FOO" >>= k
             n -> [n]
      X.element "x" [] (X.text "FOO")
         @?= X.dfpos f n0

  , HU.testCase "dfpos: output multiple nodes" $ do
      let Right n0 = X.element' "x" [] (X.text "a")
          f = \k -> \case
             X.Text "a" -> let ny = X.element "y" [] [] in (ny <> ny) >>= k
             X.Element "y" _ _ -> X.text "b" >>= k
             X.Element "x" as cs  ->
                (X.element "z" as cs <> X.text "a") >>= k
             n -> [n]
      (X.element "z" [] (X.text "bb") <> X.text "bb")
         @?= X.dfpos f n0

  , HU.testCase "dfpre: depth-first pre-order?" $ do
      let (ns, ts) = S.runState (X.dfpreM fixvisit node0) []
      ns @?= [node0]
      ts @?= "abcdefg"

  , HU.testCase "dfpre: output single node" $ do
      let Right n0 = X.element' "x" [] (X.text "a" <> X.text "b")
          f = \k -> \case
             X.Text "ab" -> X.text "foo" >>= k
             X.Text "foo" -> X.text "FOO" >>= k
             n -> [n]
      X.element "x" [] (X.text "FOO")
         @?= X.dfpre f n0

  , HU.testCase "dfpre: output multiple nodes" $ do
      let Right n0 = X.element' "x" [] (X.text "a")
          f = \k -> \case
             X.Text "a" -> let ny = X.element "y" [] [] in (ny <> ny) >>= k
             X.Element "y" _ _ -> X.text "b" >>= k
             X.Element "x" as cs  ->
                (X.element "z" as cs <> X.text "a") >>= k
             n -> [n]
      X.element "z" [] (X.text "bb") <> X.text "bb"
        @?= X.dfpre f n0
  ]

--------------------------------------------------------------------------------

bsEncode :: [X.Node] -> B.ByteString
bsEncode = BL.toStrict . BB.toLazyByteString . X.encode

