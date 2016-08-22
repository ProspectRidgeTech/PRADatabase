module PRA.Page.Award where
import Data.List
import PRA.Utils
import PRA.App
import Yesod

awardHomePage :: WidgetT PRA IO()
awardHomePage = [whamlet|
            <div .results>
                <h1> Prospect Ridge Academy Student Database
                <a href=@{AAwardR}>
                    <h4> Award an Award
                <a href=@{ShowAR}>
                    <h4> See Monthly Awards
|]

showAwardsForm :: MonthYear -> Html -> MForm Handler (FormResult FMonth, Widget)
showAwardsForm (year,month) =
    renderDivs $ FMonth
    <$> areq (selectFieldList monthPairs) "Month: " (Just month)
    <*> areq intField "Year: " (Just year)

showAwardsSubmitSuccess :: [Peak] -> [Student] -> MonthYear -> WidgetT PRA IO ()
showAwardsSubmitSuccess peaks sdnts date@(year,month) = do
    [whamlet|
      <div .results>
          <h1> Awards for #{monthToName month}, #{show year}
          $forall peak <- nub $ map peakName peaks
              $with sdntLst <- monthlyAwards peak date sdnts
                  $if null sdntLst
                  $else
                      <hr>
                      <h2> #{peak}
                      $forall (Student name num _ peak _ _ awardLst _) <- sdntLst
                          <a href=@{StudentR num} class=hidden>
                              <h4>#{concatName name} - #{title $ getAward date awardLst}
                          <p><i>"#{blurb $ getAward date awardLst}"</i>
|]

showAwardsFormWidget :: (ToWidget PRA w,ToMarkup e) => (w, e) -> WidgetT PRA IO ()
showAwardsFormWidget (widget, enctype) = do
    [whamlet|
      <div .formbox>
          <h1> Prospect Ridge Academy Student Database
          <p> Enter the month and year of the awards you would like to view.
          <form method=post action=@{ShowAR} enctype=#{enctype}>
              ^{widget}
              <button>Search
    |]

awardForm :: [Awards] -> [Student] -> MonthYear -> Html -> MForm Handler (FormResult FAward, Widget)
awardForm awards sdnts (year,month) =
    renderDivs $ FAward
    <$> areq (selectFieldList $ awardsToPairs awards) "Award: " Nothing
    <*> areq (selectFieldList $ studentsToPairs sdnts) "Student: " Nothing
    <*> (unTextarea <$> areq textareaField "Blurb: " Nothing)
    <*> areq (selectFieldList monthPairs) "Month Awarded: " (Just month)
    <*> areq intField "Year Awarded: " (Just year)

awardSubmitSuccess :: Text -> Text -> WidgetT PRA IO ()
awardSubmitSuccess title sdnt = do
    [whamlet|
      <div .results>
          <h1> Prospect Ridge Academy Student Database
          <h3> Form Submitted
          <p> #{sdnt} has recieved the #{title} character award.
    |]

awardFormWidget :: (ToWidget PRA w,ToMarkup e) => (w, e) -> WidgetT PRA IO ()
awardFormWidget (widget, enctype) = do
    [whamlet|
      <div .formbox>
          <h1> Prospect Ridge Academy Student Database
          <form method=post action=@{AAwardR} enctype=#{enctype}>
              ^{widget}
              <button>Submit
    |]
