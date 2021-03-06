{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Control.Arrow
import Data.Foldable
import qualified Pipes as P
import qualified Pipes.Shaft as PS
import qualified Pipes.Prelude as PP
import Test.Hspec

data1 :: [Int]
data1 = [1, 2..9]

data2 :: [Either Int String]
data2 = [ Left 1
        , Right "a"
        , Left 2
        , Right "b"
        , Left 3
        , Right "c"
        , Left 4
        , Right "d"
        , Left 5
        , Right "e"
        ]

sig1 :: Monad m => P.Producer Int m ()
sig1 = traverse_ P.yield data1

sig2 :: Monad m => P.Producer (Either Int String) m ()
sig2 = traverse_ P.yield data2

duplicate :: a -> (a, a)
duplicate a = (a, a)

main :: IO ()
main =
  hspec $ do
      describe "Arrow" $ do
          it "first" $ do
              let xs = PP.toList
                      (sig1
                       P.>-> PP.map duplicate
                       P.>-> PS.fromShaft (first $ PS.Shaft (PP.map (+ 10))))
              xs `shouldBe` zip ((+ 10) <$> data1) data1

          it "second" $ do
              let xs = PP.toList
                       (sig1
                        P.>-> PP.map duplicate
                        P.>-> PS.fromShaft (second $ PS.Shaft (PP.map (+ 10))))
              xs `shouldBe` zip data1 ((+ 10) <$> data1)

          it "***" $ do
              let xs = PP.toList
                       (sig1
                        P.>-> PP.map duplicate
                        P.>-> PS.fromShaft (PS.Shaft (PP.map (+ 10)) *** PS.Shaft (PP.map (+ 20))))
              xs `shouldBe` zip ((+ 10) <$> data1) ((+ 20)<$> data1)

          it "&&&" $ do
              let xs = PP.toList
                       (sig1
                        P.>-> PS.fromShaft (PS.Shaft (PP.map (+ 10)) &&& PS.Shaft (PP.map (+ 20))))
              xs `shouldBe` zip ((+ 10) <$> data1) ((+ 20)<$> data1)

      describe "ArrowChoice" $ do
          it "left" $ do
              let xs = PP.toList
                       (sig2
                        P.>-> PS.fromShaft (left $ PS.Shaft (PP.map (+ 10))))
              xs `shouldBe` ((\a -> case a of
                                     Left a' -> Left (a' + 10)
                                     Right a' -> Right a') <$> data2)

          it "right" $ do
              let xs = PP.toList
                       (sig2
                        P.>-> PS.fromShaft (right $ PS.Shaft (PP.map (++ "!"))))
              xs `shouldBe` ((\a -> case a of
                                     Left a' -> Left a'
                                     Right a' -> Right (a' ++ "!")) <$> data2)

          it "+++" $ do
              let xs = PP.toList
                       (sig2
                        P.>-> PS.fromShaft (PS.Shaft (PP.map (+ 10)) +++ PS.Shaft (PP.map (++ "!"))))
              xs `shouldBe` ((\a -> case a of
                                     Left a' -> Left (a' + 10)
                                     Right a' -> Right (a' ++ "!")) <$> data2)


          it "|||" $ do
              let xs = PP.toList
                       (sig2
                        P.>-> PS.fromShaft (PS.Shaft (PP.map (show . (+ 10))) ||| PS.Shaft (PP.map (++ "!"))))
              xs `shouldBe` ((\a -> case a of
                                     Left a' -> show (a' + 10)
                                     Right a' -> a' ++ "!") <$> data2)
