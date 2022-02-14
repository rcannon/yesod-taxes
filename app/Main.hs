{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Control.Applicative
import Data.Text
import Yesod

data App = App

mkYesod "App" [parseRoutes|
/ HomeR GET
/tax TaxInfoR GET
/tax/result TaxResultR POST
|]

instance Yesod App

-- this is necessary for using areq
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessag

data TaxInfo = TaxInfo 
  { incomeInfo :: Double }
  deriving Show

data TaxResult = TaxResult 
  { effectiveTaxPercent :: Double
  , afterTaxIncome :: Double
  , taxPayed :: Double
  }
  deriving Show


calculateTaxResult :: TaxInfo -> TaxResult
calculateTaxResult taxInfo = 
  TaxResult <$> effectiveTaxRate 
            <*> afterTaxIncome 
            <*> taxPayed
  where
  income = incomeInfo taxInfo
  taxPayed = calcTaxPayed income
  afterTaxIncome = income - taxPayed
  effectiveTaxRate = taxPayed / income
  
  calcTaxPayed income |
    income > 523600. = (+ 157804.25) $ .37 * (income - 523600.)
    income > 209425. = (+ 47843.)    $ .35 * (income - 209425.)
    income > 164925. = (+ 33603.)    $ .32 * (income - 164925.)
    income > 86375.  = (+ 14751.)    $ .24 * (income - 86375.)
    income > 40525.  = (+ 4664.)     $ .22 * (income - 40525.)
    income > 9950.   = (+ 995.)      $ .12 * (income - 9950.)
    otherwise        =                 .10 * income
    


taxInfoAForm :: AForm Handler TaxResult
taxInfoAForm = (calculateTaxResult . TaxInfo) <$> areq incomeField "Income" (Just 0)
  where
  errorMessage :: Text
  errorMessage = "Income must be non-negative."

  incomeField = checkBool (>= 0) errorMessage doubleField

taxInfoForm :: Html -> MForm Handler (FormResult TaxResult, Widget)
taxInfoForm = renderTable $ taxInfoAForm

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|<a href=@{TaxInfoR}>Tax Calculator!|]


getTaxInfoR :: Handler Html
getTaxInfoR = do
  -- Generate the form to be displayed
  (widget, enctype) <- generateFormPost taxInfoForm
  defaultLayout
      [whamlet|
          <p>
              The widget generated contains only the contents
              of the form, not the form tag itself. So...
          <form method=post action=@{TaxResultR} enctype=#{enctype}>
              ^{widget}
              <p>It also doesn't include the submit button.
              <button>Submit
      |]

postTaxResultR :: Handler Html
postTaxResultR = do
  ((result, widget), enctype) <- runFormPost taxInfoForm
  case result of
      FormSuccess taxInfo -> defaultLayout [whamlet|
                                             <p>#{show taxInfo}
                                             <a href=@{TaxInfoR}>Again!
                                             <a href=@{HomeR}>Go Home! 
                                           |]
      _ -> defaultLayout
          [whamlet|
              <p>Invalid input, let's try again.
              <form method=post action=@{TaxResultR} enctype=#{enctype}>
                  ^{widget}
                  <button>Submit
          |]

main = warp 3000 App
