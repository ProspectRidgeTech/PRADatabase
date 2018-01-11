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
    , studentsToPairs
    , clubsToPairs
    , fromEntities
    , toStudent
    , readCSV
    , buildName
    ) where
import Text.Blaze as Export
import Data.Maybe as Export (fromJust)
import Data.Text as Export (Text, pack, unpack, append, breakOnAll)
import Data.Tuple as Export (swap)
import Data.Time as Export
import Data.Char as Export
import Crypto.Hash as Export
import Yesod.Static as Export
import Control.Monad as Export
import Database.Persist as Export
import Database.Persist.TH as Export
import Database.Persist.Sqlite as Export
import Control.Monad.Trans.Resource as Export (runResourceT)
import Control.Monad.Logger as Export (runStderrLoggingT)

import Data.Csv (decodeByName, runParser, Parser, NamedRecord, (.:))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Either (fromRight)
import Yesod hiding ((.:))
import PRA.App

--Funtions
md5sum :: Text -> Text
md5sum = T.pack . map toUpper . show . (hash :: BS.ByteString -> Digest MD5) . BS.pack . T.unpack

readCSV :: BL.ByteString -> [Student]
readCSV csv = case decodeByName csv of
    Left err -> error $ "ERROR WHILE DECODING CSV: " ++ err
    Right (_, v) -> V.toList v

buildName :: NamedRecord -> Name
buildName r = (fromRight "" . runParser $ r .: "student.firstName", fromRight "" . runParser $ r .: "student.lastName")

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
toStudent (FStudent fname lname num gradYear) = Student (fname,lname) num gradYear [] Nothing

fromEntities :: [Entity a] -> [a]
fromEntities = map fromEntity
    where fromEntity (Entity _ x) = x

clubsToPairs :: [Club] -> [(Text, Club)]
clubsToPairs clubLst = [(clubName x,x) | x <- clubLst]

studentsToPairs :: [Student] -> [(Text,Student)]
studentsToPairs sdnts = zip (map (concatName . studentName) sdnts) sdnts
