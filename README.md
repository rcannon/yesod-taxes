# TODO

remove title from getTaxInfoR (just remove setTitle function, keep it displayed on page)

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

The goal of this tutorial is to help you get started with developing Haskell web applications with Yesod. While I think the [Yesod Book](https://www.yesodweb.com/book) is well written, it can be quite daunting for people who are new to Haskell or new to Yesod. Here I will give you an introduction to Yesod by building a simple United States federal income tax calclator. I recommend treating this as a suppliment to the Yesod Book and reading this concurrently with it, and I will try to point you to the relevent sections of the book when they come up. This is intended to be introductory and practical, we won't go in depth at all on types, template haskell, etc. I want to make this a useful first jumping-off point for working with Yesod.

We will be managing the development environment with Nix through Flakes and with Cabal with the `callCabal2nix`. I'm not going to talk much about setting this up, but you can take a look at the `Flake.nix` file if you are interested. When I started using Nix Flakes, I found [this](https://serokell.io/blog/practical-nix-flakes) post from Serokell immensly helpful (in fact, the `Flake.nix` file for this project based on and nearly identical to the final one shown in the above post). 

To keep things simple, the whole application is kept in a single file: `app/Main.hs`.

### Application Structure (Required Yesod Types, Route Layout)

We'll start with the basic required Yesod types and instances foru our application. I won't talk too much about what things mean in order to avoid getting overly technical. I recommend reading the [basics](https://www.yesodweb.com/book/basics) page concurrently with this section. It will cover things more precisely and with greater detail.

```Haskell
-- lines 86, 95, 99-100 of Main.hs
data TaxApp = TaxApp ConnectionPool

instance Yesod TaxApp

instance RenderMessage TaxApp FormMessage where
    renderMessage _ _ = defaultFormMessage
```

You can change `TaxApp` to whatever is preferred. I won't talk to much about what these do, but they are necessary for Yesod to do it's Template Haskell magic. 

Next is something really useful though; we'll talk about routes. First the code:

```Haskell
-- lines 88-93 of Main.hs

mkYesod "TaxApp" [parseRoutes|
/ HomeR GET
/tax TaxInfoR GET
/tax/result TaxResultR POST
/tax/id SavedResultR POST
|]
```

The `mkYesod "TaxApp" [parseRoutes|` is not important for our purposes, but again feel free to change `"TaxApp"` to whatever, so long as it is consistent with that in the previous code snippet. Here you can see that we have four routes. If this app was running at `website.com`, the routes would be (1) `website.com/`, (2) `website.com/tax`, (3) `website.com/tax/result`, and (4) `website.com/tax/id`. Routes in Yesod require three things to be specified: the route as it would appear as a relative path in the URL, a resource name for this route (we will use it later for specifying the functionality), and the type of request that should we answered at this route. The basics of routing are covered [here](https://www.yesodweb.com/book/basics#basics_routing), and more in depth coverage can be found in the [Routing and Handlers](https://www.yesodweb.com/book/routing-and-handlers) section of the Yesod Book.

### The Home Page

In this section we will concern ourselves with just the first route listed above:
```
-- line 89 of Main.hs

/ HomeR GET
```
Since we have the route already specified, all we need to do now is code up what happens
when a user accesses this page. In our case, we are just going to link them to a different page that houses our tax calculator. Here is the code for that:

```Haskell
-- lines 136-140 of Main.hs

getHomeR :: Handler Html
getHomeR = defaultLayout 
  [whamlet|
    <a href=@{TaxInfoR}>Tax Calculator!             
  |]
```

In general, when we are writing the functionality for our routes, we need to take care to name them in a specific manner. The function for handling a specific route and request should be named starting with the request type in all lower-case, followed contiguously by the resource name. The compiler will complain if you don't do this. As another example, if we specified a route as `/my/route MyRouteR POST`, then the route handler function would need to be named `postMyRouteR`. Read more about the handler functions [here](https://www.yesodweb.com/book/basics#basics_handler_function).

Now on to the functionality of the handler. We will avoid customizing the formatting of the page, so we will just use the `defaultLayout` function to handle that. We use the integrated `whamlet` quasi-quoter to generate the HTML of our webpage from the Hamlet template language. In our case, we are just specifying a hyperlink reference to the `TaxInfoR` handler that will be the start page of the actual application. The link will appear under the text "Tax Calculator!" in the same manner that links appear on this page. You can learn more about using the Hamlet for generating HTML, as will as the other template languages for CSS and JavaScript from the [Shakesperian Templates](https://www.yesodweb.com/book/shakespearean-templates) part of the book.

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

Now on to the computation. There isn't anything relevent to Yesod going on here, so I won't spend much time explaining this. We just accept an arguement of type `TaxInfo` which just holds income information and return a value of type "TaxResult" which contains the values specified in the last code snippet.

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

The just of this code it that it accepts a `Double` from the user using the `areq` function for creating applicative forms (learn more from the Yesod Book's [AForms](https://www.yesodweb.com/book/forms#forms_create_literal_aform_literal_s) section) and since applicative are functors, we `fmap` the composed function `calculateTaxResult . TaxInfo` to create an instance of the `TaxInfo` data and pass it to `calculateTaxResult` to compute the result. The result of this is passed to `renderTable` which takes care of allowing us to use the form monadically and present it on a webpage (we'll get to this later). We define `incomeField` to help us restrict the values the form accepts (see [Form Validation](https://www.yesodweb.com/book/forms#forms_validation)); in our case, we require income to be greater than zero.

### Basic Database Stuff

In order to use a Postgres database with Persistent, we need to add another instance.

```Haskell
-- lines 102-107 of Main.hs

instance YesodPersist TaxApp where
    type YesodPersistBackend TaxApp = SqlBackend

    runDB action = do
        TaxApp pool <- getYesod
        runSqlPool action pool
```

We will use the specified `runDB` function to access stored tax results in our database.
You can learn more about working with Persistent is the [relevent section](https://www.yesodweb.com/book/persistent) of the book. We will specifically utilize the sections on [integrating Persistent with Yesod](https://www.yesodweb.com/book/persistent#persistent_integration_with_yesod) and [working with Postgres](https://www.yesodweb.com/book/persistent#persistent_something_besides_sqlite).

The way that we are going to handle retieve saved tax result data is that we will give each user that submits income data a key. That key will uniquely identify their data in the database. When the user enters that key in our app, we will return their data. We need to create a type that represents the key:

```Haskell
-- lines 39-41 of Main.hs

data TaxKey = TaxKey 
  { getKey :: Int64 }
  deriving Show
```

The postgres portion of Persistent uses 64 bit integers as data base keys.

Nex we create a form to transform user input into a `TaxKey`. We don't apply any function to the key, that will come later. Here we just want to get an integer from the user and create a `TaxKey` value from it. Here is the code:

```Haskell
-- lines 126-128 of Main.hs

savedInfoForm :: Html -> MForm Handler (FormResult TaxKey, Widget)
savedInfoForm 
  = renderTable $ TaxKey <$> areq intField "ID Number: " Nothing
```

In the next sections we will talk more about building the interface between user and these backends.


### The Front Page of the App

Next we will create the webpage that accepts info from the user. 
This corresponds to the `/tax TaxInfoR GET` route and resource we specified above. 
Thus the name of the this handler function will be `getTaxInfoR`. 
We will give the user two options. In the first, the user can enter their income, and 
the application will take that info, calculate their tax results, and redirect them to a page that displays their result as well as their database key for later access. The second option will work if they have already entered tax info before and thus have already recieved a key with which to access that saved info. In this case the user will be able to enter that key, and they will be redirected to a page that displays their saved tax result.
Here is the code that does that, then I will explain it.

```Haskell
-- lines 145-171 of Main.hs

getTaxInfoR :: Handler Html
getTaxInfoR = do
  (calcWidget, calcEnctype) <- generateFormPost taxInfoForm 
  (saveWidget, saveEnctype) <- generateFormPost savedInfoForm
  defaultLayout $ do
      setTitle title
      app calcWidget calcEnctype saveWidget saveEnctype
      
  where 
  title = "US Federal Income Tax Calculator"

  app widget1 enctype1 widget2 enctype2 = do
    toWidget [lucius| appStyle { text-align: center } |]

    [whamlet|
        <appStyle>
          <p>#{title}
          <form method=post action=@{TaxResultR} enctype=#{enctype1}>
              ^{widget1}
              <p>Click the button below to learn about your taxes!
              <button>Submit
          <p>Or, get your saved tax data: 
          <form method=post action=@{SavedResultR} enctype=#{enctype2}>
              ^{widget2}
              <button>Submit
          <a href=@{HomeR}>Go Home
    |]
```

You can see that the `Handler` type is monadic, which allows for easy composition of methods. In the first two lines of `getTaxInfoR`, we generate the forms from the revious section. Each of these returns a pair of two values, the first being the widget that we will use in displaying the form and the second being an Enctype (TODO: check) which we don't need to talk about for our purposes. After gettign these values, we call the `defaultLayout` function to display the page. The `defaultLayout` function takes a widget as input, and we'll demonstrate composibility of Widgets by building a function by that takes the outputs of the `generateFormPost` functions and builds a widget. 

We rely on the `toWidget` function that takes input in the form of the Lucius JavaScript tamplate languages and turns it into a widget for easy modular display on the web (you can learn more about widgets (here, TODO). Here, the Lucius code simply formats our forms to the center of the page. This formatting is applied the HTML generated by the `whamlet` quasiquoter (the "w" standing for widget). Within the whamlet quasiquotes, the `appStyle` line utilizes the formating (centering on page) that we specified directly above. We then display the title of the page.

The next line shows the first form, the one that first calculates the tax results. We'll display it isolated for easier reference:

```Haskell
<form method=post action=@{TaxResultR} enctype=#{enctype1}>
              ^{widget1}
              <p>Click the button below to learn about your taxes!
              <button>Submit

```

### Calculated Tax Results

If the user entered information about their income on the last page, they will be brought to this one. 

### The Saved Tax Results

### Running the Application


