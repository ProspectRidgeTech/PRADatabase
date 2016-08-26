module PRA.Page.Search where
import PRA.Utils
import PRA.App
import Yesod

dbSearchForm :: Html -> MForm Handler (FormResult FSearch, Widget)
dbSearchForm = renderDivs $ FSearch <$> areq textField "Search for a student: " Nothing

dbSearchFormWidget :: (ToWidget PRA w,ToMarkup e) => (w, e) -> WidgetT PRA IO ()
dbSearchFormWidget (widget, enctype) = do
    [whamlet|
      <div .formbox>
          <h1> Prospect Ridge Academy Student Database Search
          <p> Enter the name of, or part of the name of, the student you are searching for.
          <form method=post action=@{SearchR} enctype=#{enctype}>
              ^{widget}
              <button>Search
    |]
