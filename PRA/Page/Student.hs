module PRA.Page.Student where
import PRA.Utils
import PRA.Logic
import PRA.App
import Yesod

studentPage :: [Student] -> WidgetT PRA IO ()
studentPage sdnt = do
        [whamlet|
        <div .results>
            $if null sdnt
                <h1>Student Not Found
            $else
                $forall (Student name num gradYear _ club) <- sdnt
                    <h1>#{concatName name} - Class of #{gradYear}
                    <h4>#{show num}
                    $maybe c <- club
                        <p><strong>Club:</strong> #{clubName c}
                    $nothing
                        <p><strong>Club:</strong> None
|]

allStudents :: [Student] -> WidgetT PRA IO ()
allStudents res = do
    [whamlet|
      <div .results>
          <h1> Search results
          $forall (Student name num gradYear _ _) <- res
              <a href=@{StudentR num}>
                  <p>#{concatName name} - #{gradYear}
    |]
