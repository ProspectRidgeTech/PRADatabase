module PRA.Page.Club where
import PRA.Utils
import PRA.Logic
import PRA.App
import Yesod

studentClubForm :: [Student] -> [Club] -> Html -> MForm Handler (FormResult ClubFStudent, Widget)
studentClubForm sdnts cMap =
    renderDivs $ ClubFStudent
    <$> areq (selectFieldList $ studentsToPairs sdnts) "Student: " Nothing
    <*> areq intField "Student Number: " Nothing
    <*> collapse [ areq (selectFieldList $ clubsToPairs cMap) "First Choice Club: " Nothing
                 , areq (selectFieldList $ clubsToPairs cMap) "Second Choice Club: " Nothing
                 , areq (selectFieldList $ clubsToPairs cMap) "Third Choice Club: " Nothing ]

pracSubmitSuccess :: WidgetT PRA IO ()
pracSubmitSuccess = do
    [whamlet|
      <div .results>
          <h1> Prospect Ridge Academy Club Signup
          <h3> Form Submitted
          <p> Your submission has been recieved! You're done!
    |]

clubFormWidget :: (ToWidget PRA w,ToMarkup e) => (w, e) -> WidgetT PRA IO ()
clubFormWidget (widget, enctype) = do
    mmsg <- getMessage
    [whamlet|
      <div .formbox>
          <h1> Prospect Ridge Academy Club Signup
          <form method=post action=@{ClubR} enctype=#{enctype}>
              ^{widget}
              $maybe msg <- mmsg
                  <p class="alert">#{msg}
              <button>Submit
    |]

resultsPage :: [Student] -> WidgetT PRA IO ()
resultsPage sdnts = do
        let res = studentsToClubMap sdnts
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
                    <h2> #{clubName club}:
                    $forall ((fn,ln), g) <- zip (map studentName $ members club) (map studentGradYear $ members club)
                        <p> #{fn} #{ln} - #{g}
            $if null unresolved
            $else
                <h2> Unresolved:
                $forall ((ufn,uln), ug) <- zip (map studentName unresolved) (map studentGradYear unresolved)
                    <p> #{ufn} #{uln} - #{ug}
|]
