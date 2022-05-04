module Main where

import Data.List (intercalate)
import PGF
import Test.Hspec
import UD2GF
import UDAnnotations
import UDConcepts (UDData (UDData))


myUDEnv :: IO UDEnv
myUDEnv = getEnv (path "Test") "Eng" "UDS"
  where path x = "tests/grammars/" ++ x

main :: IO ()
main = do
  env <- myUDEnv
  -- TODO: run "gf --make TestOrderingEng.gf" to generate TestOrderingEng.pgf
  someCats <- readFile "tests/grammars/some_cats.conllu"
  tenHovercrafts <- readFile "tests/grammars/test_distance.conllu"
  portionOfBuildingSeparatedByWalls <- readFile "tests/examples/portion_of_building_separated_by_walls.conllu"
  largePortionOfWalls <- readFile "tests/examples/large_portion_of_walls.conllu"
  policy_acl_nmod <- readFile "tests/examples/policy_called_P_of_the_company.conllu"
  policy_nmod_acl <- readFile "tests/examples/policy_of_the_company_called_P.conllu"
  hspec $ do
    describe "Prefer flat trees" $ do
      it "should pick the flatter tree of the two alternatives" $ do
        bestTree env theCatSleepsAlready `shouldBe` "root_nsubj_obl (UseV sleep_V) (DetCN the_Det (UseN cat_N)) already_Adv"
    describe "Allow matching on LEMMA" $ do
      it "should handle 'LEMMA' as a UD tag" $ do
        -- TODO: It shouldn't be using ImpVP. Fixed by not ignoring startCat (#16).
        -- It might also be worthwhile to prioritize smaller trees of different categories,
        -- instead of the current behaviour of preferring categories that are earlier in alphabetic order.
        bestTrees env someCats `shouldBe`
          ["DetCN anySg_Det (UseN cat_N)"
          ,"DetCN anyPl_Det (UseN cat_N)"
          ,"DetCN someSg_Det (UseN cat_N)"
          ,"DetCN somePl_Det (UseN cat_N)"]
    describe "Parsing for labels" $ do
      it "should allow an escaped comma as a UD tag" $ do
        labelAndMorpho "head[LEMMA=\\,]" `shouldBe` ("head", [UDData "LEMMA" [","]])
    describe "Preserve order of children" $ do
      it "should prefer to start with the first children if there's a choice" $ do
        bestTree env policy_acl_nmod `shouldBe` "DetCN anySg_Det (NmodCN (AclCN (UseN policy_N) (PastPartAP (ComplV2 call_V2 P_NP))) (nmod_ of_Prep (DetCN the_Det (UseN company_N))))"

      it "should prefer to start with the first children if there's a choice (part 2)" $ do
        bestTree env policy_nmod_acl `shouldBe` "DetCN anySg_Det (AclCN (NmodCN (UseN policy_N) (nmod_ of_Prep (DetCN the_Det (UseN company_N)))) (PastPartAP (ComplV2 call_V2 P_NP)))"

      -- portion of building separated by walls
      it "should handle two post-modifiers" $ do
        bestTree env portionOfBuildingSeparatedByWalls `shouldBe` "DetCN each_Det (AdjCN (PastPartAgentAP separate_V2 (DetCN aPl_Det (UseN wall_N))) (AdvCN (UseN portion_N) (PrepNP of_Prep (DetCN a_Det (UseN building_N)))))"
      it "should handle pre- and post-modifier" $ do
        bestTree env largePortionOfWalls `shouldBe` "AdjCN (PositA large_A) (NmodCN (UseN portion_N) (nmod_ of_Prep (DetCN aPl_Det (UseN wall_N))))"

    describe "Match on DISTANCE" $ do
      xit "should handle 'DISTANCE' as keyword, CG-style" $ do
        bestTrees env tenHovercrafts `shouldBe`
          ["ApposNum (UseN hovercraft_N) ten_Num"
          ,"DetCN (num2Det ten_Num) (UseN hovercraft_N)"]


bestTrees :: UDEnv -> String -> [String]
bestTrees env conll = map exprStr exprs
  where
      exprs = getExprs [] env conll
      exprStr expr = case expr of
        (x : _xs) -> showExpr [] x
        _         -> "bestTree: ud2gf failed"


bestTree :: UDEnv -> String -> String
bestTree env conll = exprStr
  where
      exprs = getExprs [] env conll
      exprStr = case exprs of
        (x : _xs) : _xss -> showExpr [] x
        _                -> "bestTree: ud2gf failed"

theCatSleepsAlready :: String
theCatSleepsAlready = unlines
  [ "1\tthe\tthe\tDET\tQuant\tFORM=0\t2\tdet\t_\tFUN=DefArt"
  , "2\tcat\tcat\tNOUN\tNN\tNumber=Sing\t3\tnsubj\t_\tFUN=cat_N"
  , "3\tsleeps\tsleep\tVERB\tVBZ\tMood=Ind|Number=Sing|Person=3|Tense=Pres|VerbForm=Fin\t0\troot\t_\tFUN=sleepVBZ"
  , "4\talready\talready\tADV\tRB\t_\t3\tadvmod\t_\t_"
  ]

{-

portionOfBuildingSeparatedByWalls :: String
portionOfBuildingSeparatedByWalls = unlines $ fmap (intercalate "\t")
  [ ["# newdoc"]
  , ["# newpar"]
  , ["# sent_id = 1"]
  , ["# text = portion of building separated by walls"]
  , ["1",  "portion",    "portion",   "NOUN",  "NN",   "Number=Sing",               "0",  "root",  "_",   "_"]
  , ["2",  "of",         "of",        "ADP",   "IN",   "_",                         "3",  "case",  "_",   "_"]
  , ["3",  "building",   "building",  "NOUN",  "NN",   "Number=Sing",               "1",  "nmod",  "_",   "_"]
  , ["4",  "separated",  "separate",  "VERB",  "VBN",  "Tense=Past|VerbForm=Part",  "1",  "acl",   "_",   "_"]
  , ["5",  "by",         "by",        "ADP",   "IN",   "_",                         "6",  "case",  "_",   "_"]
  , ["6",  "walls",      "wall",      "NOUN",  "NNS",  "Number=Plur",               "4",  "obl",   "_",   "_"]
  ]

largePortionOfWalls :: String
largePortionOfWalls = unlines $ fmap (intercalate "\t")
  [ ["# newdoc"]
  , ["# newpar"]
  , ["# sent_id = 1"]
  , ["# text = large portion of walls"]
  , ["1", "large",   "large",   "ADJ",  "JJ",  "Degree=Pos",  "2", "amod", "_", "_"]
  , ["2", "portion", "portion", "NOUN", "NN",  "Number=Sing", "0", "root", "_", "_"]
  , ["3", "of",      "of",      "ADP",  "IN",  "_",           "4", "case", "_", "_"]
  , ["4", "walls",   "walls",   "NOUN", "NNS", "Number=Plur", "2", "nmod", "_", "_"]
  ]

-- This parse makes zero sense
policyOfTheCompany :: String
policyOfTheCompany = unlines $ fmap (intercalate "\t")
  [ ["# newdoc"]
  , ["# newpar"]
  , ["# sent_id = 1"]
  , ["# text = policy (called in this document \"The Policy\" of the company"]
  , ["1",  "policy",   "policy",   "NOUN",  "NN",    "Number=Sing",               "0",  "root",  "_", "_"]
  , ["2",  "(",        "(",        "PUNCT", "-LRB-", "_",                         "3",  "punct", "_", "SpaceAfter=No"]
  , ["3",  "called",   "call",     "VERB",  "VBN",   "Tense=Past|VerbForm=Part",  "9",  "acl",   "_", "_"]
  , ["4",  "in",       "in",       "ADP",   "IN",    "_",                         "6",  "case",  "_", "_"]
  , ["5",  "this",     "this",     "DET",   "DT",    "Number=Sing|PronType=Dem",  "6",  "det",   "_", "_"]
  , ["6",  "document", "document", "NOUN",  "NN",    "Number=Sing",               "9",  "obl",   "_", "_"]
  , ["7",  "\"",       "\"",       "PUNCT", "``",    "_",                         "9",  "punct", "_", "SpaceAfter=No"]
  , ["8",  "The",      "the",      "DET",   "DT",    "Definite=Def|PronType=Art", "9",  "det",   "_", "_"]
  , ["9",  "Policy",   "policy",   "NOUN",  "NN",    "Number=Sing",               "1",  "obj",   "_", "SpaceAfter=No"]
  , ["10", "\"",       "\"",       "PUNCT", "''",    "_",                         "9",  "punct", "_", "_"]
  , ["11", "of",       "of",       "ADP",   "IN",    "_",                         "13", "case",  "_", "_"]
  , ["12", "the",      "the",      "DET",   "DT",    "Definite=Def|PronType=Art", "13", "det",   "_", "_"]
  , ["13", "company",  "company",  "NOUN",  "NN",    "Number=Sing",               "9",  "nmod",  "_", "_"]
  ]

-}
