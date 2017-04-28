{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

-- | make-payments
--
-- Inspects outstanding balances and sends appropriate charge commands to
-- Stripe.
--
module Main where

import Import.NoFoundation

import Control.Lens
import Data.Function (on)
import Data.Maybe
import Data.Ratio
import RunPersist
import System.Environment
import System.IO
import Web.Stripe
import Web.Stripe.Balance
import Web.Stripe.Charge
import Web.Stripe.Customer
import Web.Stripe.Error
import qualified Data.ByteString.Char8 as BS

import qualified Model.Skeleton as Skeleton

-- For doctests:
--
-- $setup
-- >>> import Test.QuickCheck
-- >>> instance Arbitrary Cents where arbitrary = Cents . getPositive <$> arbitrary

-- | Stripe measures charges in cents. Handy!
chargeCents :: Iso' Cents Amount
chargeCents = iso toAmount fromAmount
  where
    toAmount (Cents i) = Amount (fromIntegral i)
    fromAmount (Amount i) = Cents (fromIntegral i)

type StripeResult = Either StripeError Charge

data ChargeResult = ChargeResult
        { _chargeResultPatron :: PatronId
        , _chargeResultFee :: Cents
        , _chargeResultNet :: DonationUnits
        , _chargeResultStripe :: StripeResult
        } deriving (Show)

main :: IO ()
main = do
    conf <- fmap
        (StripeConfig . StripeKey . BS.pack . fromJust)
        (lookupEnv "STRIPE_SECRET_KEY")
    runPersist (makePayments conf)

-- | Calculate Stripe's fee: 2.9% + 30¢
--
-- https://stripe.com/us/pricing
--
-- Stripe uses financial rounding, aka the rounding everyone outside the US
-- learns (apparently). This is the rounding implemented in Prelude, as
-- well. Hooray!
--
-- If we ever have integration testing, we should confirm the following
-- holds true:
--
--      $5.00 charge -> 44.5¢ fee -> Stripe rounded to 44¢
--     $15.00 charge -> 73.5¢ fee -> Stripe rounded to 74¢
--
-- I confirmed these facts when I wrote this function, but tests ftw.
stripeFee :: Cents -> Cents
stripeFee = round . (+ 30) . (* 0.029) . fromIntegral

-- | As of 2016-10-10, the amount a patron pays is increased so that the
-- amount the project receives is equal to the amount they crowdmatched.
--
-- Proving that the rounding always works out was annoying, but I did it
-- with a brute-force program. It's ok up until integer underflows around
-- ~$20M.
--
-- prop> \d -> d < 2*10^9 ==> let {p = payment d; f = stripeFee p} in p-f==d
payment :: Cents -> Cents
payment = round . (/ (1 - 0.029)) . (+ 30) . fromIntegral

-- | A donation is sufficient for processing if the Stripe fee is < 10%.
-- https://tree.taiga.io/project/snowdrift/issue/457
--
-- This function is useful for testing, but we memoize its
-- production-required result below.
--
-- Since we're using the 'payment' function right now, this equation is
-- different from the long term ideal.
sufficientDonation :: DonationUnits -> Bool
sufficientDonation d =
    fee % p < maximumFee
  where
    p = payment (d^.donationCents)
    fee = stripeFee p
    maximumFee = ((%) `on` Cents) 1 10

-- | This is the minimum amount that satisfies 'sufficientDonation'. You can
-- find it for yourself by running:
-- >>> :{
-- >>> let x = head . filter (\x -> all sufficientDonation [x..x+35])
-- >>>              . map DonationUnits
-- >>>              $ [10..]
-- >>> in (x, x == minimumDonation)
-- >>>:}
-- (DonationUnits 3790,True)
--
-- Note that rounding makes the function discontinuous, with a step every
-- 1/0.029 ~ 35 DonationUnits. There's a local optimum at ~3610, but we'll just
-- skip that one, cause that's weird.
--
-- Since we're using the 'payment' function right now, this value is higher
-- than the long term ideal.
minimumDonation :: DonationUnits
minimumDonation = DonationUnits 3790

-- | The projection of a Patron that can, and should, make a donation.
data Donor = Donor
        { _donorPatron :: PatronId
        , _donorCustomer :: CustomerId
        , _donorDonationPayable :: DonationUnits
        } deriving (Show)

-- | Send charge commands to Stripe.
--
-- This holds a lock on the database to ensure consistency. That could kill
-- concurrent performance, but right now the only thing hitting the payment
-- tables is this utility and the crowdmatch utility. None of those should ever
-- be run simultaneously at present, so I'd rather have bad "performance" on
-- operational mistakes, rather than bad/duplicate charges. :)
makePayments :: MonadIO m => StripeConfig -> SqlPersistT m ()
makePayments conf = do
    -- Duplicating sql logic with Haskell logic to get rid of patrons
    -- without a CustomerId :/
    --
    -- #1 (hidden because Esqueleto)
    chargeable <- Skeleton.patronsReceivable minimumDonation
    let donors =
            -- #2
            mapMaybe
                (\p ->
                    Donor
                    <$> Just (entityKey p)
                    <*> _patronStripeCustomer (entityVal p)
                    <*> Just (_patronDonationPayable (entityVal p)))
                chargeable
    chargeResults <- liftIO (traverse (sendCharge conf) donors)
    mapM_ (recordResults conf) chargeResults

-- | Send the charge command to Stripe
--
-- For the Futurama milestone, we tack on a fee that covers the Stripe fee
-- to calculate the 'payment'.
sendCharge
    :: StripeConfig
    -> Donor
    -> IO ChargeResult
sendCharge conf Donor{..} =
    (ChargeResult _donorPatron fee net <$>)
        . stripe conf
        . (-&- _donorCustomer)
        -- Supported upstream as of 2016-10-06, but not in our resolver yet
        -- . (-&- ExpandParams ["balance_transaction"])
        . flip createCharge USD
        . view chargeCents
        $ cents
  where
    cents = payment (_donorDonationPayable ^. donationCents)
    fee = stripeFee cents
    net = (cents - fee) ^. from donationCents

recordResults
    :: MonadIO m
    => StripeConfig
    -> ChargeResult
    -> SqlPersistT m ()
recordResults conf res@ChargeResult {..} =
    either
        (const (liftIO (hPrint stderr res)))
        recordDonation
        _chargeResultStripe
  where
    recordDonation c@Charge {..} = do
        ts <- liftIO (donationTimestamp conf c)
        insert_
            (DonationHistory
                _chargeResultPatron
                ts
                _chargeResultNet
                _chargeResultFee)
        update _chargeResultPatron [PatronDonationPayable -=. _chargeResultNet]

-- | Tries to get the timestamp from the Charge's TransactionBalance
-- sub-item. If that fails, it's cool, we'll just use a local variant of
-- "now".
--
-- I don't want to bail on recording the charge if we can't get the
-- timestamp, since the presence of the Charge itself means Stripe
-- processed it. There was merely a secondary failure getting the
-- TransactionBalance. Ideally we'd retry, with some sort of 'pending'
-- status, but let's slap that together later.
donationTimestamp :: StripeConfig -> Charge -> IO HistoryTime
donationTimestamp conf = fmap HistoryTime . chargeTime
  where
    fallback = getCurrentTime
    chargeTime Charge {..} =
        maybe fallback transactionTime chargeBalanceTransaction
    transactionTime = \case
        Expanded BalanceTransaction {..} -> pure balanceTransactionCreated
        Id balId -> (=<<)
            (either (const fallback) (pure . balanceTransactionCreated))
            (stripe conf (getBalanceTransaction balId))
