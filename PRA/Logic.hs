module PRA.Logic where
import PRA.Utils
import Data.List

seniorSort :: [Student] -> [Student]
seniorSort = sortBy sortByGradYear . reverse . nub . reverse
 where sortByGradYear sdnt1 sdnt2
        |studentGradYear sdnt1 > studentGradYear sdnt2 = GT
        |studentGradYear sdnt1 < studentGradYear sdnt2 = LT
        |otherwise                                     = EQ

placeStudents :: [Student] -> ClubMap -> ClubMap
placeStudents [] result = result
placeStudents (x:xs) result@(clubMbrs, unRes)
  |length chx < 1 = placeStudents xs (clubMbrs, unRes ++ [x])
  |length clubMLst < clubMaxSize (head chx) =
      placeStudents xs (updateAL clubMbrs (head chx) (clubMLst ++ [x]), unRes)
  |otherwise =
      placeStudents (x {studentChoices = drop 1 chx}:xs) result
      where chx = studentChoices x
            clubMLst = fromJust $ lookup (head chx) clubMbrs

sortAll sdntLst clubMap = postSort $ placeStudents (seniorSort sdntLst) (zip clubMap (repeat []),[])

postSort :: ClubMap -> ClubMap
postSort dat
  |not (null effectedSdnts) =
      sortAll (seniorSort $ map dropChoice effectedSdnts ++ sdnts `fltrOutLst` effectedSdnts) (map fst clubDat)
  |otherwise = dat
      where clubDat = fst dat
            smallClubs = filter (\(club,mbrs) -> length mbrs < clubMinSize club) clubDat
            sdnts = concatMap snd clubDat ++ snd dat
            effectedSdnts = concatMap snd smallClubs
            fltrOutLst = foldl (\ res x -> filter (/= x) res)
            dropChoice sdnt = sdnt{studentChoices = drop 1 (studentChoices sdnt)}
