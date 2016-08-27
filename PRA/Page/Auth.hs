module PRA.Page.Auth where
import PRA.Logic
import PRA.Utils
import PRA.App
import Yesod

protectedPage expr = do
    admin <- lookupSession "admin"
    case admin of
        Nothing -> do
            setUltDestCurrent
            redirect AuthR
        Just admin -> expr

authPageForm :: [Admin] -> Html -> MForm Handler (FormResult Text, Widget)
authPageForm pass =
    renderDivs $ areq (checkBool ((`elem` map adminPass pass) . md5sum) ("Incorrect password" :: Text) passwordField) "Administrator password: " Nothing

authPageFormWidget :: (ToWidget PRA w,ToMarkup e) => (w, e) -> WidgetT PRA IO ()
authPageFormWidget (widget, enctype) = do
    mmsg <- getMessage
    [whamlet|
      <div .formbox>
          <h1> Protected Page
          <h3> To view this page, please verify your admin status.
          <form method=post action=@{AuthR} enctype=#{enctype}>
              ^{widget}
              $maybe msg <- mmsg
                  <p class="alert">#{msg}
              <button>Login
    |]

authUpdateForm :: Html -> MForm Handler (FormResult Text, Widget)
authUpdateForm = renderDivs $ areq passwordField "New administrator password: " Nothing

authUpdateSubmitSuccess :: WidgetT PRA IO ()
authUpdateSubmitSuccess = do
    [whamlet|
      <div .results>
          <h1> Prospect Ridge Academy Student Database
          <h3> Administrator password updated
    |]

authUpdateFormWidget :: (ToWidget PRA w,ToMarkup e) => (w, e) -> WidgetT PRA IO ()
authUpdateFormWidget (widget, enctype) = do
    mmsg <- getMessage
    [whamlet|
      <div .formbox>
          <h1> Update Password
          <h3> Please enter a new password.
          <form method=post action=@{AuthUR} enctype=#{enctype}>
              ^{widget}
              <button>Login
    |]
