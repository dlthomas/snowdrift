-- | crowdmatch
--
-- Calculates and records the monthly donation totals for patrons.
--
module Main where

import Import.NoFoundation

import Data.Ratio
import RunPersist

main :: IO ()
main = runPersist crowdmatch

crowdmatch :: MonadIO m => SqlPersistT m ()
crowdmatch = do
    pledges :: [Pledge] <- map entityVal <$> selectList [] []
    let projectValue = fromIntegral (length pledges)
    today <- utctDay <$> liftIO getCurrentTime
    mapM_
        (recordCrowdmatch (CrowdmatchDay today) (DonationUnit projectValue))
        pledges

recordCrowdmatch
    :: MonadIO m => CrowdmatchDay -> DonationUnit -> Pledge -> SqlPersistT m ()
recordCrowdmatch day amt Pledge{..} = do
    insert_ (CrowdmatchHistory _pledgeUsr day amt)
    void
        (upsert
            (DonationPayable _pledgeUsr amt) [DonationPayableBalance +=. amt])

-- Take the set of pledges, calculate what everyone owes and to whom, write
-- it out.
--
-- But there's only one project, so that's pretty straightforward.
--
-- I guess the tricky part is what to do next. Store the amount owed
-- immediately? Oh, store the transaction and add the amount to a thing?
-- Then the payout mech can just pull from the thing.
