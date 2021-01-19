module Bill where

import           Import
import           RIO.Map (findWithDefault, (!?))

-- |
districtAmountForBill :: District -> Bill -> Constraint Integer
districtAmountForBill d b =  pure defaultAmount >>> bsfConstraint billSpecificAmount
  where
    -- Should always be there so given a default value 0
    defaultAmount :: Integer
    defaultAmount = findWithDefault 0 (b ^. category) (d ^. categoryDefaultFunding)

    billSpecificAmount :: Maybe Integer
    billSpecificAmount = (d ^. billSpecificFunding) !? (b ^. name)

-- |
districtAmountForBillsWithCap :: Integer -> District -> [Bill] -> Constraint' Integer [Integer]
districtAmountForBillsWithCap cap d bs = traverse (districtAmountForBill d) bs >>> ratioConstraint cap

-- |
calculateCategory :: CategoryName -> District -> [Bill] -> Constraint' Integer [Integer]
calculateCategory cat d bs = districtAmountForBillsWithCap cap d bs'
  where
    bs' :: [Bill]
    bs' = filter (\b -> b ^. category == cat) bs

    -- Should always be there, but also given a pretty big default value
    -- as maxBound isn't available for Integer
    cap :: Integer
    cap = findWithDefault 100000000 cat (d ^. caps)

-- | Get all available categories from a list of bills
getCategories :: [Bill] -> [CategoryName]
getCategories = nubOrd . fmap (^. category)
