module Run (run) where

import           Data.Aeson
import           RIO.ByteString.Lazy

import           Bill                (calculateCategory, getCategories)
import           Import

run :: RIO App ()
run = do
  content <- readFileBinary "./input.json"
  case decode (fromStrict content) :: Maybe Input of
    Nothing -> logError "Error parsing"
    Just a -> do
      let cats = getCategories (bills a)

      forM_ (liftA2 (,) cats (districts a)) $ \(c, d) -> do
        let x = calculateCategory c d (bills a)
        let b = runConstraint x 0

        logInfo $ displayShow b


      logInfo "yay"
