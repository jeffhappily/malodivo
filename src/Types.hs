{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell       #-}
module Types where

import           Control.Lens  (classUnderscoreNoPrefixFields, makeLensesWith)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Ratio    ((%))
import           RIO
import           RIO.Map       (fromList)
import           RIO.Process   (HasProcessContext (..), ProcessContext)

data App = App
  { appLogFunc        :: !LogFunc
  , appProcessContext :: !ProcessContext
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

type CategoryName = String
type BillName = String
type Amount = Integer

data Input =
  Input
    { bills     :: [Bill]
    , districts :: [District]
    }
    deriving Show

data Bill =
  Bill
    { _name     :: BillName
    , _category :: CategoryName
    , _amount   :: Int
    }
    deriving Show

data District =
  District
    { _name                   :: String
    , _availableFunds         :: Int
    , _categoryDefaultFunding :: Map CategoryName Amount
    , _billSpecificFunding    :: Map BillName Amount
    , _caps                   :: Map CategoryName Amount
    }
    deriving Show

instance FromJSON District where
  parseJSON = withObject "District" $ \v -> do
    _name                   <- v .: "name"
    _availableFunds         <- v .: "availableFunds"
    categoryDefaultFunding  <- v .: "categoryDefaultFunding"
    billSpecificFunding     <- v .: "billSpecificFunding"
    caps                    <- v .: "caps"

    let _categoryDefaultFunding = categoryAmountToMap categoryDefaultFunding
    let _billSpecificFunding = billAmountToMap billSpecificFunding
    let _caps = categoryAmountToMap caps

    return District{..}

data CategoryAmount = CategoryAmount
  { _category :: CategoryName
  , _amount   :: Amount
  }
  deriving (Show)

categoryAmountToMap :: [CategoryAmount] -> Map CategoryName Amount
categoryAmountToMap xs = fromList $ fmap (\(CategoryAmount c a) -> (c, a)) xs

data BillAmount = BillAmount
  { _bill   :: BillName
  , _amount :: Amount
  }
  deriving (Show)

billAmountToMap :: [BillAmount] -> Map CategoryName Amount
billAmountToMap xs = fromList $ fmap (\(BillAmount b a) -> (b, a)) xs

makeLensesWith classUnderscoreNoPrefixFields ''Bill
makeLensesWith classUnderscoreNoPrefixFields ''District

$(deriveJSON defaultOptions 'Input)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 1 } 'Bill)
$(deriveToJSON defaultOptions { fieldLabelModifier = drop 1 } 'District)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 1 } 'CategoryAmount)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 1 } 'BillAmount)

-- | Constraint is a function that given a value, returns a
-- constrained value
newtype Constraint' a b =
  Constraint
    { runConstraint
        :: a -- ^ Initial value
        -> b -- ^ New value
    }
    deriving Category

type Constraint a = Constraint' a a

instance Functor (Constraint' a) where
  fmap f (Constraint g) = Constraint $ f . g

instance Applicative (Constraint' a) where
  pure = Constraint . const

  Constraint f <*> Constraint g = Constraint $ \x ->
    f x (g x)

instance Monad (Constraint' a) where
  return = pure

  Constraint g >>= f = Constraint $ \x' ->
    let Constraint h = f (g x')
    in h x'

-- | Bill specific fund
bsfConstraint :: Maybe Integer -> Constraint Integer
bsfConstraint num = Constraint $ \x -> fromMaybe x num

-- | Cap funding
capConstraint :: Integer -> Constraint Integer
capConstraint num = Constraint $ \x -> min x num

-- |
ratioConstraint :: Integer -> Constraint [Integer]
ratioConstraint x = Constraint $ \xs ->
  let
    total = sum xs
    ratio = x % total
    convertRatio n = round (fromRational $ fromIntegral n * ratio :: Double) in
  if total > x
    then fmap convertRatio xs
    else xs
