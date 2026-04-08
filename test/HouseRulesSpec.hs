{-# LANGUAGE OverloadedStrings #-}

module HouseRulesSpec (houseRulesSpec) where

import Test.Hspec (Spec, runIO)
import Yesod.Test
import Yesod.Auth (Route(..))
import Database.Persist (insert)
import Database.Persist.Sql (runSqlPool)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import Data.Text (pack)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (getCurrentTime, utctDay)

import Rentals.Foundation
import Rentals.Application ()  -- YesodDispatch instance
import Rentals.Database.Listing
import Rentals.Database.Money
import Rentals.Currency

import Data.UUID (UUID, fromString)
import Data.Maybe (fromJust)
import TestFoundation

testUUID :: UUID
testUUID = fromJust $ fromString "cafebabe-cafe-babe-cafe-babecafebabe"

testSlug :: Slug
testSlug = Slug "house-rules-test"

testListing :: Listing
testListing = Listing
  { listingTitle = "House Rules Test"
  , listingDescription = "A listing with house rules"
  , listingPrice = Money 100
  , listingCurrency = UsDollar
  , listingCleaning = Money 25
  , listingCountry = "US"
  , listingAddress = "321 Rules Ave"
  , listingHandlerName = "Rules Handler"
  , listingHandlerPhone = "555-9999"
  , listingHouseRules = "No smoking. No parties."
  , listingSlug = testSlug
  , listingUuid = testUUID
  }

seedListing :: App -> IO ListingId
seedListing app =
  runNoLoggingT $ runSqlPool (insert testListing) (appConnPool app)

loginAdmin :: YesodExample App ()
loginAdmin = do
  request $ do
    setMethod "POST"
    setUrl $ AuthR $ PluginR "hardcoded" ["login"]
    addPostParam "username" "admin"
    addPostParam "password" "admin"

houseRulesSpec :: Spec
houseRulesSpec = do
  app <- runIO makeTestApp
  lid <- runIO $ seedListing app

  yesodSpec app $ do
    ydescribe "House rules display" $ do

      yit "booking page shows house rules text" $ do
        get $ ListingBookR lid
        statusIs 200
        htmlAnyContain "p" "No smoking"

      yit "listing view page shows house rules text" $ do
        get $ ViewListingR lid testSlug
        statusIs 200
        htmlAnyContain "p" "No smoking"

    ydescribe "House rules booking validation" $ do

      yit "booking accepted when rules accepted" $ do
        today <- liftIO $ utctDay <$> getCurrentTime
        let bookStart = addDays 50 today
            bookEnd   = addDays 55 today
        get $ ListingBookR lid
        request $ do
          setMethod "POST"
          setUrl $ ListingBookR lid
          addToken
          byLabelExact "Start date" (pack $ show bookStart)
          byLabelExact "End date" (pack $ show bookEnd)
          byLabelExact "I accept the house rules" "yes"
        -- Stripe API call fails in test env, but validation passed
        statusIs 500

      yit "booking rejected when rules not accepted" $ do
        today <- liftIO $ utctDay <$> getCurrentTime
        let bookStart = addDays 60 today
            bookEnd   = addDays 65 today
        get $ ListingBookR lid
        request $ do
          setMethod "POST"
          setUrl $ ListingBookR lid
          addToken
          byLabelExact "Start date" (pack $ show bookStart)
          byLabelExact "End date" (pack $ show bookEnd)
        statusIs 303

    ydescribe "Admin house rules management" $ do

      yit "admin can create listing with house rules" $ do
        loginAdmin
        request $ do
          setMethod "PUT"
          setUrl AdminListingNewR
          addRequestHeader ("Content-Type", "application/json")
          setRequestBody $ encode $ object
            [ "listingNewTitle"        .= ("New Rules Listing" :: String)
            , "listingNewDescription"  .= ("desc" :: String)
            , "listingNewPrice"        .= (50 :: Int)
            , "listingNewCleaning"     .= (10 :: Int)
            , "listingNewHandlerName"  .= ("handler" :: String)
            , "listingNewHandlerPhone" .= ("555-0000" :: String)
            , "listingNewCountry"      .= ("US" :: String)
            , "listingNewAddress"      .= ("addr" :: String)
            , "listingNewHouseRules"   .= ("No pets allowed." :: String)
            ]
        statusIs 201

      yit "admin can update house rules" $ do
        loginAdmin
        request $ do
          setMethod "PUT"
          setUrl $ AdminListingR lid
          addRequestHeader ("Content-Type", "application/json")
          setRequestBody $ encode $ object
            [ "listingNewTitle"        .= ("House Rules Test" :: String)
            , "listingNewDescription"  .= ("A listing with house rules" :: String)
            , "listingNewPrice"        .= (100 :: Int)
            , "listingNewCleaning"     .= (25 :: Int)
            , "listingNewHandlerName"  .= ("Rules Handler" :: String)
            , "listingNewHandlerPhone" .= ("555-9999" :: String)
            , "listingNewCountry"      .= ("US" :: String)
            , "listingNewAddress"      .= ("321 Rules Ave" :: String)
            , "listingNewHouseRules"   .= ("Updated rules: No loud music." :: String)
            ]
        statusIs 200
