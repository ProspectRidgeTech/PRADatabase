module PRA.Page.Home where
import PRA.Logic
import PRA.Utils
import PRA.App
import Yesod

--Add a random splash to each page load

dbHomePage :: WidgetT PRA IO()
dbHomePage = [whamlet|
                <div .results>
                    <h1> Prospect Ridge Academy Student Database
                    <a href=@{AddR}>
                        <h4> Add Student
                    <a href=@{AwardR}>
                        <h4> Awards
                    <a href=@{SearchR}>
                        <h4> Search Students
                    <h4> Clubs Picker and Community Service Tracker (Coming Soon)
|]
