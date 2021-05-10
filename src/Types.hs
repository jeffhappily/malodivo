{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Control.Lens (classUnderscoreNoPrefixFields, makeLensesWith)
import Data.Aeson
import Data.Aeson.TH
import Data.Ratio ((%))
import RIO
import qualified RIO.Map as M
import RIO.Process (HasProcessContext (..), ProcessContext)

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})

type CategoryName = String

type BillName = String

type Amount = Integer

data Input = Input
  { bills :: [Bill],
    districts :: [District]
  }
  deriving (Show)

data Bill = Bill
  { _name :: BillName,
    _category :: CategoryName,
    _amount :: Int
  }
  deriving (Show)

data District = District
  { _name :: String,
    _availableFunds :: Int,
    _categoryDefaultFunding :: Map CategoryName Amount,
    _billSpecificFunding :: Map BillName Amount,
    _caps :: Map CategoryName Amount
  }
  deriving (Show)

instance FromJSON District where
  parseJSON = withObject "District" $ \v -> do
    _name <- v .: "name"
    _availableFunds <- v .: "availableFunds"
    categoryDefaultFunding <- v .: "categoryDefaultFunding"
    billSpecificFunding <- v .: "billSpecificFunding"
    caps <- v .: "caps"

    let _categoryDefaultFunding = categoryAmountToMap categoryDefaultFunding
    let _billSpecificFunding = billAmountToMap billSpecificFunding
    let _caps = categoryAmountToMap caps

    return District {..}

data CategoryAmount = CategoryAmount
  { _category :: CategoryName,
    _amount :: Amount
  }
  deriving (Show)

categoryAmountToMap :: [CategoryAmount] -> Map CategoryName Amount
categoryAmountToMap = foldr (\(CategoryAmount c a) z -> M.insert c a z) M.empty

data BillAmount = BillAmount
  { _bill :: BillName,
    _amount :: Amount
  }
  deriving (Show)

billAmountToMap :: [BillAmount] -> Map CategoryName Amount
billAmountToMap = foldr (\(BillAmount b a) z -> M.insert b a z) M.empty

makeLensesWith classUnderscoreNoPrefixFields ''Bill
makeLensesWith classUnderscoreNoPrefixFields ''District

$(deriveJSON defaultOptions 'Input)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} 'Bill)
$(deriveToJSON defaultOptions {fieldLabelModifier = drop 1} 'District)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} 'CategoryAmount)
$(deriveJSON defaultOptions {fieldLabelModifier = drop 1} 'BillAmount)

-- | Constraint is a function that given a value, returns a constrained value
newtype Constraint' a b = Constraint
  { runConstraint ::
      a -> -- Initial value
      b -- New value
  }
  deriving (Category, Functor, Applicative, Monad)

type Constraint a = Constraint' a a

-- | Bill specific fund constraint, will return bsf (bill specific fund) if exists, otherwise
-- returns whatever being passed in
bsfConstraint :: Maybe Integer -> Constraint Integer
bsfConstraint bsf = Constraint $ \x -> fromMaybe x bsf

-- | Cap funding constraint, given a capped amount @cap@, returns a constraint that cap its
-- input at @cap@
capConstraint :: Integer -> Constraint Integer
capConstraint cap = Constraint $ \x -> min x cap

-- | Given a total amount, if the sum of the given integer list is greater than the total
-- amount, return a new list with all the value reduced so that the list adds up to the
-- total amount, otherwise, return the original list
ratioConstraint :: Integer -> Constraint [Integer]
ratioConstraint x = Constraint $ \xs ->
  let total = sum xs
      ratio = x % total
      convertRatio n = round (fromRational $ fromIntegral n * ratio :: Double)
   in if total > x
        then fmap convertRatio xs
        else xs
