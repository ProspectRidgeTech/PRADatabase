module PRA.Utils
    ( module Export
    , Club(..)
    , ClubFStudent(..)
    , Student(..)
    , ClubMap
    , md5sum
    , updateAL
    , studentsToClubMap
    , setExpiry
    , unsetExpiry
    , expireToken
    , collapse
    , concatName
    , searchStudents
    , awardsToPairs
    , studentsToPairs
    , clubsToPairs
    , showPeak
    , fromEntities
    , toStudent
    , opLst
    , getAward
    , monthPairs
    , monthToName
    , toMonthYear
    , monthlyAwards
    ) where
import Data.IORef as Export
import Text.Blaze as Export
import Data.Maybe as Export (fromJust)
import Data.Text as Export (Text, pack, unpack, append, breakOnAll)
import Data.Tuple as Export (swap)
import Data.Time as Export
import Data.Char as Export
import Crypto.Hash as Export
import Yesod.Static as Export
import Control.Monad as Export
import Data.Time.Clock as Export
import Data.Time.Calendar as Export
import Data.Time.LocalTime as Export
import Database.Persist as Export
import Database.Persist.TH as Export
import Database.Persist.Sqlite as Export
import Control.Monad.Trans.Resource as Export (runResourceT)
import Control.Monad.Logger as Export (runStderrLoggingT)

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import PRA.App
import Yesod

--Funtions
md5sum :: Text -> Text
md5sum = T.pack . map toUpper . show . (hash :: BS.ByteString -> Digest MD5) . BS.pack . T.unpack

updateAL al key val = case lookup key al of
    Nothing -> al
    Just v -> fst spl ++ [(key,val)] ++ drop 1 (snd spl)
      where spl = span (/=(key,v)) al

studentsToClubMap :: [Student] -> ClubMap
studentsToClubMap =
  foldl (\(cMap,unRes) sdnt -> case studentClub sdnt of
                                Nothing -> (cMap,sdnt:unRes)
                                Just club -> if club `elem` map fst cMap
                                               then (updateAL cMap club (sdnt:(fromJust $ lookup club cMap)),unRes)
                                               else ((club,[sdnt]):cMap,unRes)) ([],[])

setExpiry :: Handler ()
setExpiry = setSession "expiry" ""
unsetExpiry :: Handler ()
unsetExpiry = deleteSession "expiry"

expireToken :: MonadHandler m => Text -> m b -> m b -> m b
expireToken token callback passthrough = do
  expiryFlag <- lookupSession "expiry"
  case expiryFlag of
    Nothing -> passthrough
    Just _ -> do
      deleteSession token
      deleteSession "expiry"
      callback

collapse :: Applicative a => [a x] -> a [x]
collapse = foldr (\c acc -> pure (:) <*> c <*> acc) (pure [])

concatName :: Name -> Text
concatName (fn, ln) = fn `append` " " `append` ln

searchStudents :: Text -> [Student] -> [Student]
searchStudents q = filter (not . null . breakOnAll (T.toLower q) . T.toLower . concatName . studentName)

toStudent :: FStudent -> Student
toStudent (FStudent fname lname num gradYear peak) = Student (fname,lname) num gradYear peak [] Nothing [] 0

fromEntities :: [Entity a] -> [a]
fromEntities = map fromEntity
    where fromEntity (Entity _ x) = x

clubsToPairs :: [Club] -> [(Text, Club)]
clubsToPairs clubLst = [(clubName x,x) | x <- clubLst]

--Make a show instance?
showPeak :: Peak -> Text
showPeak p = peakName p `append` " - " `append` peakTeacher p

awardsToPairs :: [Awards] -> [(Text,Text)]
awardsToPairs = map ((\x -> (x,x)) . awardsTitle)

studentsToPairs :: [Student] -> [(Text,Student)]
studentsToPairs sdnts = zip (map (concatName . studentName) sdnts) sdnts

toMonthYear :: FMonth -> MonthYear
toMonthYear (FMonth month year) = (year,month)

monthlyAwards :: Text -> MonthYear -> [Student] -> [Student]
monthlyAwards peak date sdnts = filter (\sdnt -> date `elem` map month (studentAwards sdnt)) peakStudents
    where peakStudents = filter (\sdnt -> peakName (studentPeak sdnt) == peak) sdnts

--Add multi-blurb support. Randomly pick a blurb in the case of duplicate awards.
getAward :: MonthYear -> [Award] -> Award
getAward date = head . filter (\(Award _ _ d) -> d == date)

monthPairs :: [(Text,Int)]
monthPairs = zip ["January","February","March","April","May","June","July","August","September","October","November","December"] [1..12]

monthToName :: Int -> Text
monthToName n = fromJust $ lookup n (map swap monthPairs)

opLst :: [(Text,Bool)]
opLst = [("Encrypt",True),("Decrypt",False)]
