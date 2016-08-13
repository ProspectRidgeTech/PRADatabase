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
                $forall (Student name num grade peak _ club awards hours) <- sdnt
                    <h1>#{concatName name}, #{grade}th
                    <h4>#{show num}
                    <p><strong>Peak:</strong> #{showPeak peak}
                    $maybe c <- club
                        <p><strong>Club:</strong> #{clubName c}
                    $nothing
                        <p><strong>Club:</strong> None
                    $if null awards
                        <p><strong>Awards:</strong> None
                    $else
                        <p><strong>Awards:</strong>
                        $forall (Award title blurb (year, month)) <- awards
                            <p><strong>#{title} - Awarded #{monthToName month}, #{show year}</strong>
                            <p><i>"#{blurb}"</i>
                    <p><strong>Service Hours:</strong> #{show hours}
|]
