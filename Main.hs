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
/student/ StudentSR GET
/student/#Int StudentR GET
/add AddR GET POST
/search SearchR GET POST
/award AwardR GET
/award/show ShowAR GET POST
/award/add AAwardR GET POST
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
            setSession "admin" ""
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
            setSession "search" (pack . show $ searchStudents query sdnts)
            redirectUltDest HomeR

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

getAAwardR :: Handler Html
getAAwardR = expireToken "search" getAAwardR $ do
    now <- liftIO getCurrentTime
    timezone <- liftIO getCurrentTimeZone
    let (y, m, _) = toGregorian $ localDay $ utcToLocalTime timezone now
    search <- lookupSession "search"
    case search of
      Nothing -> do
        setUltDestCurrent
        redirect SearchR
      Just sdnts -> do
        setExpiry
        setUltDestCurrent
        awards <- fromEntities <$> (runDB $ selectList [] [])
        let form = awardForm awards (read $ unpack sdnts) (y, m)
        ((result, widget), enctype) <- runFormPost form
        case result of
          FormSuccess (FAward title sdnt blurb month year) -> do
            let newStudentAwards = (Award title blurb (year,month)) : (studentAwards sdnt)
            runDB $ updateWhere [StudentNumber ==. (studentNumber sdnt)] [StudentAwards =. newStudentAwards]
            defaultLayout $ do
              praTheme
              awardSubmitSuccess title (concatName $ studentName sdnt)
          _ -> do
            protectedPage $ defaultLayout $ do
              praTheme
              awardFormWidget (widget, enctype)

postAAwardR :: Handler Html
postAAwardR = unsetExpiry >> getAAwardR

getStudentSR :: Handler Html
getStudentSR = expireToken "search" getStudentSR $ do
    search <- lookupSession "search"
    case search of
      Nothing -> do
        setUltDestCurrent
        redirect SearchR
      Just sdnts -> do
        setExpiry
        setUltDestCurrent
        protectedPage $ defaultLayout $ do
          praTheme
          allStudents (read $ unpack sdnts)

getClubR :: Handler Html
getClubR = expireToken "search" getAAwardR $ do
    search <- lookupSession "search"
    case search of
      Nothing -> do
        setUltDestCurrent
        redirect SearchR
      Just sdnts -> do
        setExpiry
        setUltDestCurrent
        clubMap <- fromEntities <$> (runDB $ selectList [] [])
        let form = studentClubForm (read $ unpack sdnts) clubMap
        ((result, widget), enctype) <- runFormPost form
        case result of
          FormSuccess (ClubFStudent student num choices) -> do
            if num /= studentNumber student
              then do
                setMessage $ toHtml ("Student ID Mismatch. Try Again." :: Text)
                unsetExpiry
                redirect ClubR
              else do
                runDB $ updateWhere [StudentNumber ==. (studentNumber student)] [StudentChoices =. choices]
                sdnts <- fromEntities <$> (runDB $ selectList [] [])
                clubs <- fromEntities <$> (runDB $ selectList [] [])
                let clubMap = sortAll sdnts clubs
                mapM_ (\(club,members) -> mapM_ (\sdnt -> runDB $ updateWhere [StudentNumber ==. (studentNumber sdnt)] [StudentClub =. Just club]) members) (fst clubMap)
                mapM_ (\sdnt -> runDB $ updateWhere [StudentNumber ==. (studentNumber sdnt)] [StudentClub =. Nothing]) (snd clubMap)
                defaultLayout $ do
                  praTheme
                  pracSubmitSuccess
          _ -> do
            defaultLayout $ do
              praTheme
              clubFormWidget (widget, enctype)

postClubR :: Handler Html
postClubR = unsetExpiry >> getClubR

getCResultR :: Handler Html
getCResultR = do
    sdnts <- fromEntities <$> (runDB $ selectList [] [])
    protectedPage $ defaultLayout $ do
        praTheme
        resultsPage sdnts

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "SdntDB.sqlite3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ runSqlPool (runMigrationSilent migrateAll) pool
    res <- static "Resources/"
    warp 8080 PRA {connPool = pool, src = res}
