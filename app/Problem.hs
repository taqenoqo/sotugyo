{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ExistentialQuantification #-}

module Problem where

import Data.SBV
import Data.Data
import Data.List
import qualified Data.Map as Map

class (SymWord c, Enum c, Bounded c, Show c) => IsCategory c where
  prettyPrint :: c -> String

data Credit c = 
  Credit {
    creditId         :: Integer,
    creditName       :: String,
    creditNum        :: Float,
    creditCategories :: [c]
  }
  deriving (Eq, Ord)

instance IsCategory c => Show (Credit c) where
  show (Credit _ name num cats) = "(" ++ show num ++ ")" ++ name


data CreditVar c = 
  CreditVar { 
    creditVarCredit    :: Credit c,
    creditVarIsAlloced :: SBool,
    creditVarAlloc     :: SBV c
  }

proveGraduation :: IsCategory c => ([CreditVar c] -> Symbolic SBool) -> [Credit c] -> IO String
proveGraduation graduationRule credits = do
  let affectingCredits = filter (not . null . creditCategories) credits
  let problem = makeProblem graduationRule affectingCredits
  solution <- allSat problem
  return $ showResults credits solution

makeProblem :: IsCategory c => ([CreditVar c] -> Symbolic SBool) -> [Credit c] -> Symbolic SBool
makeProblem graduationRule credits = do
  creditVars <- createCreditVars credits
  constrainCreditVars creditVars
  graduationRule creditVars
  where
    createCreditVars credits = do
      isAllocedVars  <- symbolics $ map makeValidFlagVarName credits
      allocationVars <- symbolics $ map makeAllocVarName credits
      return $ zipWith3 CreditVar credits isAllocedVars allocationVars

    constrainCreditVars creditVars = do
      constrain $ bAll fixAllocationWhenNoAlloc creditVars
      constrain $ bAll setDomainOfAllocVar creditVars
      return ()
      where
        fixAllocationWhenNoAlloc (CreditVar credit isAlloced allocation) =
          let categories = creditCategories credit in
          bnot isAlloced ==> allocation .== literal (head categories)

        setDomainOfAllocVar (CreditVar credit _ allocation) =
          let categories = creditCategories credit in
          bOr $ map (\category -> allocation .== literal category) categories

totalUpAllCredits :: IsCategory c => [CreditVar c] -> Symbolic SWord8
totalUpAllCredits creditVars = 
  let countingCondition (CreditVar _ isAlloced _) = isAlloced in
  totalUpCreditsWithPred creditVars totalVarName countingCondition

totalUpCredits :: IsCategory c => [CreditVar c] -> c -> Symbolic SWord8
totalUpCredits creditVars category = 
  let varName = makeSubTotalVarName category in
  let countingCondition (CreditVar _ isAlloced allocation) = isAlloced &&& allocation .== literal category in
  totalUpCreditsWithPred creditVars varName countingCondition

totalUpCreditsWithPred :: IsCategory c => [CreditVar c] -> String -> (CreditVar c -> SBool) -> Symbolic SWord8
totalUpCreditsWithPred creditVars varName pred = do
  eachCreditNumVars <- mkFreeVars $ length creditVars
  total <- symbolic varName
  constrain $ relateCreditVarsAndNumVars creditVars eachCreditNumVars
  constrain $ total .== sum eachCreditNumVars
  return total
  where
    relateCreditVarsAndNumVars creditVars eachCreditNumVars =
      bAll relateCreditVarAndNumVar $ zip creditVars eachCreditNumVars
      where
        relateCreditVarAndNumVar (creditVar, creditNumVar) = 
          let creditNum' = toSWord $ creditNum $ creditVarCredit creditVar in
          (pred creditVar        ==> creditNumVar .== creditNum') &&&
          (bnot (pred creditVar) ==> creditNumVar .== literal 0)

makeAllocVarName :: IsCategory c => Credit c -> String
makeAllocVarName credit = "alloc:" ++ show (creditId credit)

makeValidFlagVarName :: IsCategory c => Credit c -> String
makeValidFlagVarName credit = "valid:" ++ show (creditId credit)

makeSubTotalVarName :: IsCategory c => c -> String
makeSubTotalVarName category = "subtotal:" ++ show category

totalVarName :: String
totalVarName = "total"

toSWord :: Float -> SWord8
toSWord f = literal $ round $ 2 * f

toFloat :: Word8 -> Float
toFloat w = realToFrac w / 2

showResults :: forall c. (Enum c, Bounded c,  IsCategory c) => [Credit c] -> AllSatResult -> String
showResults credits result =
  let solutions = getModelDictionaries result in
  let solutionsStr = unlines $ zipWith showSolution solutions [1..] in
  if null solutions
    then "留年確定です ^p^\n\n科目区分が未定義の単位:\n" ++ showNoRuleCredits credits ++ "\n"
    else solutionsStr ++ "卒業できる組み合わせ: " ++ show (length solutions) ++ " 通り\n\n"
  where
    showNoRuleCredits credits = 
      let noRuleCredits = filter (null . creditCategories) credits in
      unlines $ map creditName noRuleCredits

    showSolution solution index =
      let creditResults = map (constructCreditResult solution) credits in
      let creditResultLines = map show $ sort creditResults in
      let datas::[c] = [minBound .. maxBound] in
      let subtotalLines = map (showSubtotal solution) datas in
      let totalStrLine = showTotal solution in
      let partingLine = replicate 10 '-' in
      let lines = creditResultLines ++ [partingLine] ++ subtotalLines ++ [totalStrLine] in
      "#" ++ show index ++ "\n" ++ unlines (map (indent 2) lines)
      where

        showSubtotal solutions category =
          let varName = makeSubTotalVarName category in
          let subtotal = fromCW $ solutions Map.! varName in
          prettyPrint category ++ ": "  ++ show (toFloat subtotal)

        showTotal solutions =
          let total = fromCW $ solutions Map.! totalVarName in
          "total: " ++ show (toFloat total)

        indent n str = replicate n ' ' ++ str

data CreditResult c =
  NoCategory (Credit c) |
  NoAlloced (Credit c) |
  Alloced (Credit c) c
  deriving (Eq, Ord)

instance IsCategory c => Show (CreditResult c) where
  show (NoCategory credit) = "! " ++ show credit
  show (NoAlloced credit) = "  " ++ show credit
  show (Alloced credit category) = "[" ++ prettyPrint category ++ "] " ++ show credit

constructCreditResult :: IsCategory c => Map.Map String CW -> Credit c -> CreditResult c
constructCreditResult _ credit@(Credit _ _ _ []) = NoCategory credit
constructCreditResult solution credit
  | isAlloced credit =
    let allocVarName = makeAllocVarName credit in
    let allocation = fromCW $ solution Map.! allocVarName in
    Alloced credit allocation
  | otherwise = NoAlloced credit
  where
    isAlloced credit =
      let flagVarName = makeValidFlagVarName credit in
      fromCW $ solution Map.! flagVarName

