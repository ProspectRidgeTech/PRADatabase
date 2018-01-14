module Main where
import qualified Data.ByteString.Lazy as BL
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
            runDB $ updateWhere [AdminUser ==. "admin"] [AdminPass =. (md5sum pass)]
            defaultLayout $ do
                praTheme
                authUpdateSubmitSuccess

getStudentR :: Int -> Handler Html
getStudentR sn = do
    sdnt <- fromEntities <$> (runDB $ selectList [StudentNumber ==. sn] [])
    protectedPage $ defaultLayout $ do
        praTheme
        studentPage sdnt

getAddR :: Handler Html
getAddR = do
    f <- generateFormPost $ newStudentForm
    -- csvData <- liftIO $ BL.readFile "roster.csv"
    -- runDB $ mapM_ insert (readCSV csvData)
    protectedPage $ defaultLayout $ do
        praTheme
        dbFormWidget f

postAddR :: Handler Html
postAddR = do
    ((result, widget), enctype) <- runFormPost $ newStudentForm
    case result of
        FormSuccess fStudent -> do
            runDB $ insert (toStudent fStudent)
            defaultLayout $ do
                praTheme
                pradbSubmitSuccess

getSearchR :: Handler Html
getSearchR = do
    f <- generateFormPost dbSearchForm
    defaultLayout $ do
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

getStudentSR :: Handler Html
getStudentSR = expireToken "search" getStudentSR $ do
    search <- lookupSession "search"
    case search of
      Nothing -> do
        setUltDestCurrent
        redirect SearchR
      Just sdnts -> do
        setExpiry
        protectedPage $ defaultLayout $ do
          praTheme
          allStudents (read $ unpack sdnts)

getClubR :: Handler Html
getClubR = expireToken "search" getClubR $ do
    search <- lookupSession "search"
    case search of
      Nothing -> do
        setUltDestCurrent
        redirect SearchR
      Just sdnts -> do
        setExpiry
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
    hash <- fromEntities <$> (runDB $ selectList [DbColumn ==. "student"] [])
    if (md5sum . pack . show $ sdnts) /= (dbHash . head $ hash)
      then do
        clubs <- fromEntities <$> (runDB $ selectList [] [])
        let clubMap = sortAll sdnts clubs
        runDB $ mapM_ (\(club,members) -> mapM_ (\sdnt -> updateWhere [StudentNumber ==. (studentNumber sdnt)] [StudentClub =. Just club]) members) (fst clubMap)
        runDB $ mapM_ (\sdnt -> updateWhere [StudentNumber ==. (studentNumber sdnt)] [StudentClub =. Nothing]) (snd clubMap)
        sdnts <- fromEntities <$> (runDB $ selectList [] [])
        runDB $ updateWhere [DbColumn ==. "student"] [DbHash =. (md5sum . pack . show $ (sdnts :: [Student]))]
        liftIO $ BL.writeFile "Static/clubs.csv" (writeCSV sdnts)
        redirect CResultR
      else do
        protectedPage $ defaultLayout $ do
          praTheme
          resultsPage sdnts

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "SdntDB.sqlite3" openConnectionCount $ \pool -> liftIO $ do
    runResourceT $ runSqlPool (runMigrationSilent migrateAll) pool
    res <- static "Static/"
    warp 4040 PRA {connPool = pool, src = res}
