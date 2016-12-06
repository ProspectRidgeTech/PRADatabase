module PRA.Page.Add where
import PRA.Utils
import PRA.App
import Yesod

newStudentForm :: Html -> MForm Handler (FormResult FStudent, Widget)
newStudentForm =
    renderDivs $ FStudent
    <$> areq textField "First Name: " Nothing
    <*> areq textField "Last Name: " Nothing
    <*> areq intField "Student Number: " Nothing
    <*> areq intField "Graduating Year: " Nothing

pradbSubmitSuccess :: WidgetT PRA IO ()
pradbSubmitSuccess = do
    [whamlet|
      <div .results>
          <h1> Prospect Ridge Academy Student Database
          <h3> Form Submitted
          <p> Your submission has been recieved and added to the database.
    |]

dbFormWidget :: (ToWidget PRA w,ToMarkup e) => (w, e) -> WidgetT PRA IO ()
dbFormWidget (widget, enctype) = do
    [whamlet|
      <div .formbox>
          <h1> Prospect Ridge Academy Student Database
          <form method=post action=@{AddR} enctype=#{enctype}>
              ^{widget}
              <button>Submit
    |]
