module PRA.App where
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Data.Text (Text)
import Yesod.Static
import Data.Either
import Data.Csv ((.:), (.=))
import qualified Data.Csv as Cassava
import Yesod hiding ((.:), (.=))
import Data.Text.Encoding

--Add a search page that returns a session message that is a list of students.
--Add club choosing page that uses session message to display relevent students in a combobox. If results are empty, set a message and redirect to the search page.

--Main Application Type
data PRA = PRA {src :: Static, connPool :: ConnectionPool}

--Routing
staticFiles "Static/"

mkYesodData "PRA" [parseRoutes|
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

--Constants
openConnectionCount = 4 :: Int
adminCookieTTL = 5 :: Int

--Type synonyms
type Name = (Text,Text)

type MonthYear = (Integer,Int)

--Data Types

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Db
    column Text
    hash Text
    deriving Show Read Eq
Admin
    user Text
    pass Text
    deriving Show Read Eq
Club
    name Text
    minSize Int
    maxSize Int
    deriving Show Read Eq
Student
    name Name
    number Int
    gradYear Int
    choices [Club]
    club Club Maybe
    deriving Show Read
|]

type ClubMap = ([(Club,[Student])], [Student])

data ClubFStudent = ClubFStudent {student  :: Student, num :: Int, choices :: [Club]}

data FStudent = FStudent Text Text Int Int

--This is seriously stupid, get rid of it.
data FSearch = FSearch Text

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

instance Cassava.FromNamedRecord Student where
    parseNamedRecord r = Student name <$> (r .: "student.studentNumber") <*> (r .: "student.grade") <*> pure [] <*> pure Nothing
        where name = (fromRight "" . Cassava.runParser $ r .: "student.firstName", fromRight "" . Cassava.runParser $ r .: "student.lastName")

instance Cassava.ToNamedRecord Student where
    toNamedRecord Student{..} = Cassava.namedRecord [
      "student.firstName" .= fst studentName,
      "student.lastName" .= snd studentName,
      "student.studentNumber" .= studentNumber,
      "student.grade" .= studentGradYear,
      "student.club" .= studentClub
      ]

instance Cassava.ToField Club where
    toField = encodeUtf8 . clubName
