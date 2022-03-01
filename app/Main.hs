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
import Yesod
import Database.Persist
import Database.Persist.Postgresql
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
    -- source : 
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
/tax/id/#TaxResultId SavedTaxResultR GET
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
-- Yesod Web App Interface
--

-- applicative form instance for accepting
-- tax info and returning result
taxInfoAForm :: AForm Handler TaxResult
taxInfoAForm = (calculateTaxResult . TaxInfo) <$> areq incomeField "Taxable Income " (Just 0)
  where
  errorMessage = "Income must be non-negative." :: Text
  incomeField = checkBool (>= 0) errorMessage doubleField

-- covert applicative form to mondatic
-- for better composability syntax
taxInfoForm :: Html -> MForm Handler (FormResult TaxResult, Widget)
taxInfoForm = renderTable $ taxInfoAForm

-- home page
getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
                           <a href=@{TaxInfoR}>Tax Calculator!
                         |]

-- start page for tax app;
-- asks user for TaxInfo
getTaxInfoR :: Handler Html
getTaxInfoR = do
  (widget, enctype) <- generateFormPost taxInfoForm 
  defaultLayout $ do
      setTitle title
      app widget enctype
      
  where 
  title = "US Federal Income Tax Calculator"

  app widget enctype = do
    toWidget [lucius| appStyle { text-align: center } |]

    [whamlet|
        <appStyle>
          <p>#{title}
          <form method=post action=@{TaxResultR} enctype=#{enctype}>
              ^{widget}
              <p>Click the button below to learn about your taxes!
              <button>Submit
          <a href=@{HomeR}>Go Home
    |]


-- returns TaxResult to user,
-- based on TaxInfo input
postTaxResultR :: Handler Html
postTaxResultR = do
  ((result, widget), enctype) <- runFormPost taxInfoForm
  case result of
      FormSuccess taxResult -> do
        taxResId <- runDB $ insert taxResult
        defaultLayout [whamlet|
                        <p>"Your tax information:"
                        <p>#{show taxResult}
                        <p>"Your ID number for later access:"
                        <p>#{show $ fromSqlKey taxResId}
                        <a href=@{TaxInfoR}>Again! 
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


-- access saved TaxResult based on database ID
getSavedTaxResultR :: TaxResultId -> Handler Html
getSavedTaxResultR taxResId = do
  taxRes <- runDB $ get404 taxResId
  defaultLayout [whamlet|
                    <p>"Your Saved Tax Data"
                    <p>#{show taxRes}
                    <p>"Your ID number"
                    <p>#{show taxResId}
                    <a href=@{HomeR}>Go Home
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
