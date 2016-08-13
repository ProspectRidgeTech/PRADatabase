module PRA.Page.CResult where
import PRA.Utils
import PRA.Logic
import PRA.App
import Yesod

--resultsPage :: [ClubFStudent] -> ClubMap -> WidgetT PRA IO ()
resultsPage sdntDat cMap = do
        let res = sortAll sdntDat cMap --Avoid sorting every call to this page. Display results based on the club field of Student
            clubsl = map fst (fst res)
            unresolved = snd res
            members club = fromJust $ lookup club (fst res)
        [whamlet|
        <div .results>
            <h1>Student Club Placement
            <p>This page shows all current student submissions and the clubs into which they have been sorted. NOTE: These results are subject to change, and are NOT final until all student submissions have been recieved.
            <p>Hint: To find yourself on this page, press Ctrl-f and search your name.
            $forall club <- clubsl
                $if null (members club)
                $else
                    <h2> #{club}:
                    $forall ((fn,ln), g) <- zip (map studentName $ members club) (map studentGrade $ members club)
                        <p> #{fn} #{ln}, #{g}th
            $if null unresolved
            $else
                <h2> Unresolved:
                $forall ((ufn,uln), ug) <- zip (map studentName unresolved) (map studentGrade unresolved)
                    <p> #{ufn} #{uln}, #{ug}th
|]
