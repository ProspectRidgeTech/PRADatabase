module PRA.Page.Club where
import PRA.Utils
import PRA.App
import Yesod

studentClubForm :: [(Text, (Int,Int))] -> Html -> MForm Handler (FormResult ClubFStudent, Widget)
studentClubForm cMap =
    renderDivs $ ClubFStudent
    <$> areq textField "Full Name: " Nothing
    <*> areq (selectFieldList grades) "Grade: " Nothing
    <*> areq (selectFieldList $ clubsToPairs cMap) "First Choice Club: " Nothing
    <*> areq (selectFieldList $ clubsToPairs cMap) "Second Choice Club: " Nothing
    <*> areq (selectFieldList $ clubsToPairs cMap) "Third Choice Club: " Nothing

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
    [whamlet|
      <div .formbox>
          <h1> Prospect Ridge Academy Club Signup
          <form method=post action=@{ClubR} enctype=#{enctype}>
              ^{widget}
              <button>Submit
    |]
