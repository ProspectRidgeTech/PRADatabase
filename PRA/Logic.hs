module PRA.Logic where
import PRA.Utils
import Data.List

filterChars :: String -> String
filterChars = unwords . splitOnAny filtered
    where filtered = ['\NUL'..'\US']

splitOnAny :: Eq a => [a] -> [a] -> [[a]]
splitOnAny divLst lst = case break (`elem` divLst) lst of
    (b,[]) -> [b]
    (b,a)  -> splitOnAny divLst b ++ splitOnAny divLst (drop 1 a)

caesarShift sft = map ((drop (sft `mod` length ascii) (cycle ascii) !!) . subtract 32 . ord)
  where ascii = [' '..'~']

seniorSort :: [Student] -> [Student]
seniorSort = sortBy sortByGrade . reverse . nub . reverse
 where sortByGrade sdnt1 sdnt2
        |studentGrade sdnt1 < studentGrade sdnt2 = GT
        |studentGrade sdnt1 > studentGrade sdnt2 = LT
        |otherwise                 = EQ

updateAL al key val = case lookup key al of
    Nothing -> al
    Just v -> fst spl ++ [(key,val)] ++ drop 1 (snd spl)
      where spl = span (/=(key,v)) al

placeStudents :: ClubMap -> [Student] -> Result -> Result
placeStudents _ [] result = result
placeStudents clubMap (x:xs) result@(clubMbrs, unRes)
  |length chx < 1 = placeStudents clubMap xs (clubMbrs, unRes ++ [x])
  |length clubMLst < snd (fromJust $ lookup (clubName $ head chx) clubMap) =
      placeStudents clubMap xs (updateAL clubMbrs (clubName $ head chx) (clubMLst ++ [x]), unRes)
  |otherwise =
      placeStudents clubMap (x {studentChoices = drop 1 chx}:xs) result
      where chx = studentChoices x --Analyse what to do with the Club type, constider replacing the Text club name with the Club type.
            clubMLst = fromJust $ lookup (clubName $ head chx) clubMbrs

sortAll sdntLst clubMap = postSort clubMap $ placeStudents clubMap (seniorSort sdntLst) (zip (map fst clubMap) (repeat []),[])

postSort :: ClubMap -> Result -> Result
postSort clubMap dat
  |not (null effectedSdnts) =
      sortAll (seniorSort $ map dropChoice effectedSdnts ++ fltrOutLst effectedSdnts sdnts) clubMap
  |otherwise = dat
      where clubDat = fst dat
            clubMin c = fst (fromJust $ lookup c clubMap)
            smallClubs = filter (\(club,mbrs) -> length mbrs < clubMin club) clubDat
            sdnts = concatMap snd clubDat
            effectedSdnts = concatMap snd smallClubs
            fltrOutLst xs res = foldl (\ res x -> filter (/= x) res) res xs
            dropChoice sdnt = sdnt{studentChoices = drop 1 (studentChoices sdnt)}
