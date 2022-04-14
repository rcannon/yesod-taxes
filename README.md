
# Yesod Taxes 

A tutorial for building a first web app in Haskell with Yesod, Persistent/Postrgres, and Nix Flakes.

## Quick Start

1. Clone the repo and move into the created directory:
```
$ git clone https://github.com/rcannon/yesod-taxes.git
$ cd yesod-taxes
```

2. Confirm that Nix Flakes are enabled. 
Enter the development environment (necessary for loading Postgres through nix):
```
$ nix develop
```

3. Activate the Postgres Database:
```
$ ./db_start.sh
```
- Note: to stop the database and remove the stored database files, run `./db_stop.sh`.

4. Run the application:
```
$ nix run
```

5. Open your web browser and navigate to `localhost:3000/`.

6. Follow the instructions on the site!

## Tutorial

### Goals and Project Structure 

The goal of this tutorial is to help you get started with developing Haskell web applications with Yesod. While I think the [Yesod Book](https://www.yesodweb.com/book) is well written, it can be quite daunting for people who are new to Haskell or new to Yesod. Here I will give you an introduction to Yesod by building a simple United States federal income tax calclator. I recommend treating this as a suppliment to the Yesod Book and reading this concurrently with it, and I will try to point you to the relevent sections of the book when they come up. 

We will be managing the development environment with Nix through Flakes and with Cabal with the `callCabal2nix`. I'm not going to talk much about setting this up, but you can take a look at the `Flake.nix` file if you are interested. When I started using Nix Flakes, I found [this](https://serokell.io/blog/practical-nix-flakes) post from Serokell immensly helpful (in fact, the `Flake.nix` file for this project based on and nearly identical to the final one shown in the above post). 

To keep things simple, the whole application is kept in a single file: `app/Main.hs`.

### Application Structure (Route Layout, Required Yesod and Persistent Types)

TODO

### Backend Computation

To keep things simple, we are going to base the whole tax calculation on "income." Here is the Haskell code we will use for representing income:

```Haskell
-- lines 33-36 of Main.hs

-- info from user
data TaxInfo = TaxInfo 
  { incomeInfo :: Double }
  deriving Show
```

We are going to return a couple results to the user (and store them in a database, but that comes later): tax payed in USD, after-tax income in USD, and effective tax rate as a decimal between 0 and 1. The effective tax rate is just the tax payed divided by the income before taxes. Here is the code for representing this (Note: this will look a little different than you might expect, since we have to do some extra work to make this data storable in postgres through persistent, which we will talk about later):

```Haskell
-- lines 43 - 51 of Main.hs

-- result to user, which will be 
-- stored in database for later access
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
TaxResult
  taxPayed Double
  afterTaxIncome Double
  effectiveTaxPercent Double
  deriving Show
|]
```

Now on to the computation. There isn't any advanced stuff going on, so I won't spend time explaining this.

```Haskell
-- lines 58-79 of Main.hs

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
```

### Creating a Tax Info Form

Next we will create a simple form for accepting the income info from the user and returning the result. This won't involve designing the webpage at all. I'll present the code, then talk about what it does:

```Haskell
-- lines 114-123 of Main.hs

-- accept tax info from user and return result
taxInfoForm :: Html -> MForm Handler (FormResult TaxResult, Widget)
taxInfoForm 
  = renderTable 
  $ (calculateTaxResult . TaxInfo) 
  <$> areq incomeField "Taxable Income " (Just 0)

  where
  incomeField = checkBool (>= 0) errorMessage doubleField
  errorMessage = "Income must be non-negative." :: Text
```

The just of this code it that it accepts a `Double` from the user using the `areq` function for creating applicative forms (AForms, learn more from the Yesod Book [here](https://www.yesodweb.com/book/forms#forms_create_literal_aform_literal_s)) and since applicative are functors, we `fmap` the composed function `calculateTaxResult . TaxInfo` to create an instance of the `TaxInfo` data and pass it to `calculateTaxResult` to compute the result. The result of this is passed to `renderTable` which takes care of allowing us to use the form monadically and present it on a webpage (we'll get to this later). We define `incomeField` to help us restrict the values the form accepts (see [here](https://www.yesodweb.com/book/forms#forms_validation)); in our case, we require income to be greater than zero.


