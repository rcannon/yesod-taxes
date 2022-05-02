{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}

import Control.Applicative
import Data.Text
import Data.Int
import Yesod
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.Sql
import Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)


--
-- Data Definitions for Tax Calculator
--

-- info from user
data TaxInfo = TaxInfo 
  { incomeInfo :: Double }
  deriving Show

-- key for user to access saved tax data
data TaxKey = TaxKey 
  { getKey :: Int64 }
  deriving Show

-- result to user, which will be 
-- stored in database for later access
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TaxResult
  taxPayed Double
  afterTaxIncome Double
  effectiveTaxPercent Double
  deriving Show
|]


--
-- Tax Calculator Functionality
--

calculateTaxResult :: TaxInfo -> TaxResult
calculateTaxResult taxInfo = 
  TaxResult taxPayed afterTaxIncome effectiveTaxRate   

  where
  income = incomeInfo taxInfo

  taxPayed = calcTaxPayed income
  afterTaxIncome = income - taxPayed
  effectiveTaxRate = taxPayed / income
  
  calcTaxPayed income 
    | income > 523600 = (+ 157804.25) $ 0.37 * (income - 523600)
    | income > 209425 = (+ 47843)     $ 0.35 * (income - 209425)
    | income > 164925 = (+ 33603)     $ 0.32 * (income - 164925)
    | income > 86375  = (+ 14751)     $ 0.24 * (income - 86375)
    | income > 40525  = (+ 4664)      $ 0.22 * (income - 40525)
    | income > 9950   = (+ 995)       $ 0.12 * (income - 9950)
    | otherwise       =                 0.10 * income
    -- source: 
    -- https://www.irs.gov/pub/irs-drop/rp-20-45.pdf 
    -- (Table 3, p. 6-7)


--
-- Yesod and Persistent Types
--

data TaxApp = TaxApp ConnectionPool

mkYesod "TaxApp" [parseRoutes|
/ HomeR GET
/tax TaxInfoR GET
/tax/result TaxResultR POST
/tax/id SavedResultR POST
|]

instance Yesod TaxApp

-- this is necessary for using areq
-- (applicative forms)
instance RenderMessage TaxApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist TaxApp where
    type YesodPersistBackend TaxApp = SqlBackend

    runDB action = do
        TaxApp pool <- getYesod
        runSqlPool action pool

--
-- Forms
--

-- accept tax info from user and return result
taxInfoForm :: Html -> MForm Handler (FormResult TaxResult, Widget)
taxInfoForm 
  = renderTable 
  $ (calculateTaxResult . TaxInfo) 
  <$> areq incomeField "Taxable Income " (Just 0)

  where
  incomeField = checkBool (>= 0) errorMessage doubleField
  errorMessage = "Income must be non-negative." :: Text
  

-- get DB key from user to return saved tax info
savedInfoForm :: Html -> MForm Handler (FormResult TaxKey, Widget)
savedInfoForm 
  = renderTable $ TaxKey <$> areq intField "ID Number: " Nothing


--
-- Yesod Web App Interface
--

-- home page
getHomeR :: Handler Html
getHomeR = defaultLayout 
  [whamlet|
    <a href=@{TaxInfoR}>Tax Calculator!             
  |]


-- start page for tax app;
-- asks user for TaxInfo
getTaxInfoR :: Handler Html
getTaxInfoR = do
  (calcWidget, calcEnctype) <- generateFormPost taxInfoForm 
  (saveWidget, saveEnctype) <- generateFormPost savedInfoForm
  let title = "US Federal Income Tax Calculator"
  defaultLayout $ do
      setTitle title
      toWidget [lucius| appStyle { text-align: center } |]

      [whamlet|
        <appStyle>
          <p>#{title}
          <form method=post action=@{TaxResultR} enctype=#{calcEnctype}>
              ^{calcWidget}
              <p>Click the button below to learn about your taxes!
              <button>Submit
          <p>Or, get your saved tax data: 
          <form method=post action=@{SavedResultR} enctype=#{saveEnctype}>
              ^{saveWidget}
              <button>Submit
          <a href=@{HomeR}>Go Home
      |]
    

-- returns TaxResult to user
-- based on TaxInfo input, and
-- stores info in DB for later access
postTaxResultR :: Handler Html
postTaxResultR = do
  ((result, widget), enctype) <- runFormPost taxInfoForm
  case result of
      FormSuccess (tr@(TaxResult tp ati etr)) -> do
        taxResId <- runDB $ insert tr
        defaultLayout [whamlet|
                        <p>Your Tax Information: 
                        <p>Tax Payed: $#{show tp}
                        <p>After Tax Income: $#{show ati}
                        <p>Effective Tax Rate: #{show etr}
                        <p>
                        <p>Your ID number for later access: #{show $ fromSqlKey taxResId}
                        <a href=@{TaxInfoR}>Again!
                        <p> 
                        <a href=@{HomeR}>Go Home 
                      |]
      _ -> defaultLayout
          [whamlet|
              <p>Invalid input, let's try again.
              <form method=post action=@{TaxResultR} enctype=#{enctype}>
                  ^{widget}
                  <button>Submit
              <a href=@{HomeR}>Go Home
          |]


-- access saved TaxResult based on database entry key
postSavedResultR :: Handler Html
postSavedResultR = do
  ((result, widget), enctype) <- runFormPost savedInfoForm
  case result of
      FormSuccess taxResId -> do
        let key = (toSqlKey $ getKey taxResId) :: Key TaxResult
        (TaxResult tp ati etr) <- runDB $ get404 key 
        defaultLayout [whamlet|
                        <p>Your Tax Information: 
                        <p>Tax Payed: $#{show tp}
                        <p>After Tax Income: $#{show ati}
                        <p>Effective Tax Rate: #{show etr}
                        <p> 
                        <a href=@{HomeR}>Go Home 
                      |]
      _ -> defaultLayout
          [whamlet|
              <p>Invalid ID Number.
              <a href=@{HomeR}>Try Again!
          |]


--
-- Run the Web App
--

dbConnectionStr = "host=localhost dbname=taxapp user=test password=test port=5432"
openConnectionCount = 10

main :: IO () 
main = runStderrLoggingT 
     $ withPostgresqlPool dbConnectionStr openConnectionCount 
     $ \pool -> liftIO $ do
          runResourceT $ flip runSqlPool pool $ do
            runMigration migrateAll
            insert $ calculateTaxResult $ TaxInfo 1.0
          warp 3000 $ TaxApp pool
