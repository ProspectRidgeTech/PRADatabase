module PRA.App where
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.Text (Text)
import Yesod.Static
import Yesod

--Add a search page that returns a session message that is a list of students.
--Add club choosing page that uses session message to display relevent students in a combobox. If results are empty, set a message and redirect to the search page.

--Main Application Type
data PRA = PRA {src :: Static, connPool :: ConnectionPool}

--Routing
staticFiles "Resources/"

mkYesodData "PRA" [parseRoutes|
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

--Constants
openConnectionCount = 4 :: Int
adminCookieTTL = 5 :: Int

--Type synonyms
type Name = (Text,Text)

type MonthYear = (Integer,Int)

--Data Types

data Award = Award {title :: Text, blurb :: Text, month :: MonthYear}
    deriving (Show, Read, Eq)
derivePersistField "Award"

--Make admin pass an Admin ByteString hash rather than plain text.
--Possibly seperate out club choices into their own table. Along with nominations for awards.
--Restructure so that student contains only pertinent information. Just a club name rather than the Club datatype?
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Admin
    user Text
    pass Text
    deriving Show Read Eq
Awards
    title Text
    deriving Show Read Eq
Club
    name Text
    minSize Int
    maxSize Int
    deriving Show Read Ord Eq
Peak
    name Text
    teacher Text
    deriving Show Read Eq
Student
    name Name
    number Int
    gradYear Int
    peak Peak
    choices [Club]
    club Club Maybe
    awards [Award]
    hours Int
    deriving Show Read
|]

type ClubMap = ([(Club,[Student])], [Student])

data ClubFStudent = ClubFStudent {student  :: Student, num :: Int, choices :: [Club]}

data FStudent = FStudent Text Text Int Int Peak

data FAward = FAward Text Student Text Int Integer

--This is seriously stupid, get rid of it.
data FSearch = FSearch Text

data FMonth = FMonth Int Integer

--Instances
instance Yesod PRA where
    makeSessionBackend _ = Just <$> defaultClientSessionBackend adminCookieTTL "client_session_key.aes"

instance Eq Student where
    (==) sdnt1 sdnt2 = studentName sdnt1 == studentName sdnt2

instance RenderMessage PRA FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodPersist PRA where
    type YesodPersistBackend PRA = SqlBackend
    runDB action = do
        PRA {..} <- getYesod
        runSqlPool action connPool
