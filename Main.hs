module Main where
import PRA.Logic
import PRA.Utils
import PRA.Page
import PRA.App
import Yesod

--Add /praDB/student show all students page.
mkYesodDispatch "PRA" [parseRoutes|
/ HomeR GET
/auth AuthR GET POST
/auth/update AuthUR GET POST
/student/#Int StudentR GET
/add AddR GET POST
/search SearchR GET POST
/award AwardR GET
/award/show ShowAR GET POST
/award/add AwardSR GET POST
/award/add/#Text AAwardR GET POST
/praClubs ClubR GET POST
/praClubs/results CResultR GET
/src R Static src
|]

getHomeR :: Handler Html
getHomeR = protectedPage $ defaultLayout $ do
        praTheme
        dbHomePage

getAuthR :: Handler Html
getAuthR = do
    admin <- fromEntities <$> (runDB $ selectList [] [])
    f <- generateFormPost $ authPageForm admin
    defaultLayout $ do
        praTheme
        authPageFormWidget f

postAuthR :: Handler ()
postAuthR = do
    admin <- fromEntities <$> (runDB $ selectList [] [])
    ((result, widget), enctype) <- runFormPost $ authPageForm admin
    case result of
        FormSuccess pass -> do
            setSession "admin" "T"
            redirectUltDest HomeR
        FormFailure (err:_) -> do
            setMessage $ toHtml err
            redirect AuthR

getAuthUR :: Handler Html
getAuthUR = do
    f <- generateFormPost authUpdateForm
    protectedPage $ defaultLayout $ do
        praTheme
        authUpdateFormWidget f

postAuthUR :: Handler Html
postAuthUR = do
    ((result, widget), enctype) <- runFormPost authUpdateForm
    case result of
        FormSuccess pass -> do
            runDB $ updateWhere [AdminUser ==. "admin"] [AdminPass =. pass]
            defaultLayout $ do
                praTheme
                authUpdateSubmitSuccess

getAwardR :: Handler Html
getAwardR = protectedPage $ defaultLayout $ do
        praTheme
        awardHomePage

getStudentR :: Int -> Handler Html
getStudentR sn = do
    sdnt <- fromEntities <$> (runDB $ selectList [StudentNumber ==. sn] [])
    protectedPage $ defaultLayout $ do
        praTheme
        studentPage sdnt

getAddR :: Handler Html
getAddR = do
    peaks <- fromEntities <$> (runDB $ selectList [] [])
    f <- generateFormPost $ newStudentForm peaks
    protectedPage $ defaultLayout $ do
        praTheme
        dbFormWidget f

postAddR :: Handler Html
postAddR = do
    peaks <- fromEntities <$> (runDB $ selectList [] [])
    ((result, widget), enctype) <- runFormPost $ newStudentForm peaks
    case result of
        FormSuccess fStudent -> do
            runDB $ insert (toStudent fStudent)
            defaultLayout $ do
                praTheme
                pradbSubmitSuccess

getSearchR :: Handler Html
getSearchR = do
    f <- generateFormPost dbSearchForm
    protectedPage $ defaultLayout $ do
        praTheme
        dbSearchFormWidget f

postSearchR :: Handler Html
postSearchR = do
    sdnts <- fromEntities <$> (runDB $ selectList [] [])
    ((result, widget), enctype) <- runFormPost dbSearchForm
    case result of
        FormSuccess (FSearch query) -> do
            defaultLayout $ do
                praTheme
                dbSearchSubmitSuccess (searchStudents query sdnts)

getShowAR :: Handler Html
getShowAR = do
    now <- liftIO getCurrentTime
    timezone <- liftIO getCurrentTimeZone
    let (y, m, _) = toGregorian $ localDay $ utcToLocalTime timezone now
    f <- generateFormPost $ showAwardsForm (y, m)
    protectedPage $ defaultLayout $ do
        praTheme
        showAwardsFormWidget f

postShowAR :: Handler Html
postShowAR = do
    now <- liftIO getCurrentTime
    timezone <- liftIO getCurrentTimeZone
    let (y, m, _) = toGregorian $ localDay $ utcToLocalTime timezone now
    sdnts <- fromEntities <$> (runDB $ selectList [] [])
    peaks <- fromEntities <$> (runDB $ selectList [] [])
    --Does runFormPost really need all of the form parameters again?
    ((result, widget), enctype) <- runFormPost $ showAwardsForm (y, m)
    case result of
        FormSuccess (FMonth month year) -> do
            defaultLayout $ do
                praTheme
                showAwardsSubmitSuccess peaks sdnts (year,month)

getAwardSR :: Handler Html
getAwardSR = do
    f <- generateFormPost dbSearchForm
    protectedPage $ defaultLayout $ do
        praTheme
        awardSFormWidget f

postAwardSR :: Handler Html
postAwardSR = do
    ((result, widget), enctype) <- runFormPost dbSearchForm
    case result of
        FormSuccess (FSearch query) -> do
            defaultLayout $ do
                praTheme
                redirect (AAwardR query)

getAAwardR :: Text -> Handler Html
getAAwardR query = do
    now <- liftIO getCurrentTime
    timezone <- liftIO getCurrentTimeZone
    let (y, m, _) = toGregorian $ localDay $ utcToLocalTime timezone now
    sdnts <- (searchStudents query . fromEntities) <$> (runDB $ selectList [] [])
    awards <- fromEntities <$> (runDB $ selectList [] [])
    f <- generateFormPost $ awardForm awards sdnts (y, m)
    protectedPage $ defaultLayout $ do
        praTheme
        awardFormWidget f query

postAAwardR :: Text -> Handler Html
postAAwardR query = do
    now <- liftIO getCurrentTime
    timezone <- liftIO getCurrentTimeZone
    let (y, m, _) = toGregorian $ localDay $ utcToLocalTime timezone now
    sdnts <- (searchStudents query . fromEntities) <$> (runDB $ selectList [] [])
    awards <- fromEntities <$> (runDB $ selectList [] [])
    --Does runFormPost really need all of the form parameters again?
    ((result, widget), enctype) <- runFormPost $ awardForm awards sdnts (y, m)
    case result of
        FormSuccess (FAward title sdnt blurb month year) -> do
            let newStudentAwards = (Award title blurb (year,month)) : (studentAwards sdnt)
            runDB $ updateWhere [StudentNumber ==. (studentNumber sdnt)] [StudentAwards =. newStudentAwards]
            defaultLayout $ do
                praTheme
                awardSubmitSuccess title (concatName $ studentName sdnt)

getClubR :: Handler Html
getClubR = do
    clubMap <- (clubsToMap . fromEntities) <$> (runDB $ selectList [] [])
    f <- generateFormPost (studentClubForm clubMap)
    defaultLayout $ do
        praTheme
        clubFormWidget f

postClubR :: Handler Html
postClubR = do
    clubMap <- (clubsToMap . fromEntities) <$> (runDB $ selectList [] [])
    ((result, widget), enctype) <- runFormPost (studentClubForm clubMap)
    case result of
        FormSuccess clubFStudent -> do
            --Add DB update here
            defaultLayout $ do
                praTheme
                pracSubmitSuccess

getCResultR :: Handler Html
getCResultR = do
    sdnts <- fromEntities <$> (runDB $ selectList [] [])
    clubMap <- (clubsToMap . fromEntities) <$> (runDB $ selectList [] [])
    protectedPage $ defaultLayout $ do
        praTheme
        resultsPage sdnts clubMap

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "SdntDB.sqlite3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ runSqlPool (runMigrationSilent migrateAll) pool
    res <- static "Resources/"
    warp 8080 PRA {connPool = pool, src = res}
