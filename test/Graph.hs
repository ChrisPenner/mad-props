{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Graph where

import Test.Hspec
import Grid
import Data.Graph.Inductive as G hiding ((&))
import Data.Graph.Inductive.NodeMap as NM
import qualified Data.Set as S
import qualified Text.RawString.QQ as R
import qualified Data.Text as T
import Data.Foldable
import Data.Functor.Rep

simplest :: T.Text
simplest = T.filter (/= ' ')
  [R.r|123
       456
       789|]

graphOf :: T.Text -> Gr Char Dir
graphOf t =
    let (_rows, _cols, _nm, gr) = graphFromText t
     in gr

simplestPositionsRef :: S.Set Position
simplestPositionsRef =
    S.singleton (Option '1' '2' '3'
                        '4' '5' '6'
                        '7' '8' '9')

nested :: T.Text
nested = T.filter (/= ' ')
  [R.r|.....
       .123.
       .456.
       .789.
       .....|]

nestedPositionsRef :: S.Set Position
nestedPositionsRef =
  S.fromList [Option '.' '.' '.'
                     '.' '1' '2'
                     '.' '4' '5'
            , Option '.' '.' '.'
                     '1' '2' '3'
                     '4' '5' '6'
            , Option '.' '.' '.'
                     '2' '3' '.'
                     '5' '6' '.'
            , Option '.' '1' '2'
                     '.' '4' '5'
                     '.' '7' '8'
            , Option '.' '4' '5'
                     '.' '7' '8'
                     '.' '.' '.'
            , Option '1' '2' '3'
                     '4' '5' '6'
                     '7' '8' '9'
            , Option '2' '3' '.'
                     '5' '6' '.'
                     '8' '9' '.'
            , Option '4' '5' '6'
                     '7' '8' '9'
                     '.' '.' '.'
            , Option '5' '6' '.'
                     '8' '9' '.'
                     '.' '.' '.'
             ]




-- simpleText :: T.Text
-- simpleText = T.filter (/= ' ')
--   [R.r|.......
--        .|-|-|.
--        .-|-|-.
--        .......|]

chosen :: Option Char
chosen = Option '1' '2' '3'
                '4' '5' '6'
                '7' '8' '9'

rightSide :: Option Char
rightSide  = Option '2' '3' '.'
                    '5' '6' '.'
                    '8' '9' '.'


spec :: Spec
spec = do
    describe "collectSuperPositions" $ do
      it "should get the expected options" $ do
        collectSuperPositions (graphOf simplest) `shouldBe` simplestPositionsRef
        collectSuperPositions (graphOf nested) `shouldBe` nestedPositionsRef

    describe "graphFilter" $ do
      it "should propagate match information" $ do
        let chosen = Option '1' '2' '3'
                            '4' '5' '6'
                            '7' '8' '9'
        S.filter (gridFilter chosen E) (collectSuperPositions (graphOf nested))
            `shouldBe` S.singleton rightSide

