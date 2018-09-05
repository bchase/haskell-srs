{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib where

import Data.Char      (isAlpha, toLower)
import Data.List      (intercalate, nub)
import Data.Monoid    ((<>), mempty)
import Data.Bifunctor (bimap)
import GHC.Generics   (Generic)

import Data.Aeson                 (ToJSON(..), (.=))
import Database.SQLite.Simple     (NamedParam((:=)))
import Control.Monad.Trans.Class  (lift)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)

import qualified Data.Aeson                 as J
import qualified Data.Text                  as T
import qualified Data.Map                   as Map
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8      as BS
import qualified Data.Time.Clock.POSIX      as Time
import qualified System.Random              as Random
import qualified System.Directory           as Dir
import qualified System.FilePath.Posix      as FP
import qualified Codec.Archive.Zip          as Zip
import qualified Crypto.Hash                as Crypto
import qualified Database.SQLite.Simple     as SQLite

import Lib.Anki.SQLite (NoteId(..), DeckId(..), ModelId(..), Model(..),
                        SQLiteCollection(..), SQLiteNote(..), SQLiteCard(..),
                        SQLiteDeckJSON(..), SQLiteDecksJSON)

-- TODO FP.joinPath instead of `++ "/"` (throughout)

type Dir = FilePath

type Tag = String
type Gloss = String
type DeckName = String

data AudioCard = AudioCard
  { audioCardFrontAudio  :: FilePath
  , audioCardFrontNoHTML :: String
  , audioCardBack        :: String
  , audioCardTags        :: [Tag]
  }

data Card = Card -- ready for APKG
  { cardFront       :: String
  , cardFrontNoHTML :: String
  , cardBack        :: String
  , cardTags        :: [Tag] -- TODO Set Tag
  , cardMedia       :: [FilePath]
  }

class ToCard c where
  mvMediaAndConvertCards :: Dir -> [c] -> IO [Card]

instance ToCard AudioCard where
  mvMediaAndConvertCards dir cs = map mkCard <$> mvMediaFiles cs
    where
      mvMediaFiles = mapM mvAndUpdatePath . zip [0..]

      mvAndUpdatePath (idx, card@AudioCard{audioCardFrontAudio}) = do
        let dest = FP.joinPath [dir, show (idx :: Int)]
        Dir.copyFile audioCardFrontAudio dest
        return $ card { audioCardFrontAudio = dest }

      mkCard AudioCard{..} =
        let front = "[sound:" ++ FP.takeFileName audioCardFrontAudio ++ "]"
         in Card front audioCardFrontNoHTML audioCardBack audioCardTags [ audioCardFrontAudio ]



newtype MediaManifest = MediaManifest { unMediaManifest :: [FilePath] } deriving ( Generic )

manifestPairs :: MediaManifest -> [(String, FilePath)]
manifestPairs = map (bimap show id) . zip ([0..] :: [Int]) . unMediaManifest

instance ToJSON MediaManifest where
  toEncoding = J.pairs . foldl (\json (n,f) -> (n .= f) <> json) mempty . map (bimap T.pack id) . manifestPairs



main :: IO ()
main = do
  let card = AudioCard { audioCardFrontAudio  = "boof.mp3"
                       , audioCardFrontNoHTML = "front-no-html"
                       , audioCardBack        = "back"
                       , audioCardTags        = []
                       }

  -- TODO blows up with trailing slash for source dir
  eAPKG <- toAPKG "NewDeckName" [] "source" [ card ]
  case eAPKG of
    Left err   -> putStrLn $ "APKG failed: "  ++ err
    Right apkg -> putStrLn $ "APKG created: " ++ apkg


-- TODO get rid of (Dir, Dir) by passing absolute paths to media?
-- TODO newtype for `FilePath`s? (accidentally swapped deck name & db path)
toAPKG :: (ToCard c) => DeckName -> [Tag] -> Dir -> [c] -> IO (Either String FilePath)
toAPKG deckName tags mediaDir xs = do
  tmp <- mkTmpDir

  cards <- tagCards tags <$> mvMediaAndConvertCards tmp xs

  -- TODO use `Exception`s in general, but particularly here w/ FS I/O
  ensureDirsExist (mediaDir, tmp) $ do
    let mediaManifest' = mediaManifest cards

    cpMediaFiles (mediaDir, tmp) mediaManifest'
    writeMediaManifest tmp mediaManifest'
    db <- cpTemplateDB tmp

    runExceptT $ do
      col <- ExceptT $ overwriteDeckName deckName db
      ()  <- ExceptT $ writeCollection db col cards
      lift $ mkAPKG tmp deckName

  where
    cpTemplateDB :: Dir -> IO FilePath
    cpTemplateDB dir = do
      let source = "templates/collection.anki2"
          sink   = dir ++ "/collection.anki2"
      Dir.copyFile source sink
      return sink

    overwriteDeckName :: DeckName -> FilePath -> IO (Either String SQLiteCollection)
    overwriteDeckName name dbPath = do
      cols <- getCollections dbPath
      case cols of
        [col@SQLiteCollection{sqlite_col_decks}] -> do
          -- TODO change? -- sqlite_col_decks :: SQLiteDecksJSON
          case renameDeck name sqlite_col_decks of
            Left err     -> return $ Left err
            Right decks' -> do
              let col' = col { sqlite_col_decks = decks' }
              updateDecksOnCollection col' dbPath
              return $ Right col'
        [] -> return $ Left "No collections in DB template"
        _  -> return $ Left "More than one collection in DB template"

        where
          renameDeck :: DeckName -> SQLiteDecksJSON -> Either String SQLiteDecksJSON
          renameDeck n decks =
            case Map.toList decks of
              [(did, deck)] -> Right . Map.fromList $ [(did, deck { sqlite_deck_json_name = n })]
              []            -> Left "No decks"
              _             -> Left "More than 1 deck"

          getCollections :: FilePath -> IO [SQLiteCollection]
          getCollections = do
            flip sqlite3 $ flip SQLite.query_ "SELECT * FROM col"

          updateDecksOnCollection :: SQLiteCollection -> FilePath -> IO ()
          updateDecksOnCollection SQLiteCollection{sqlite_col_id,sqlite_col_decks} = do
            flip sqlite3 $ \conn -> do
              SQLite.executeNamed conn
                "UPDATE col SET decks = :decks WHERE id = :id"
                [ ":id"    := sqlite_col_id
                , ":decks" := sqlite_col_decks
                ]


    writeCollection :: FilePath -> SQLiteCollection -> [Card] -> IO (Either String ())
    writeCollection dbPath col cs = do
      eNotesAndCards <- buildDeck col cs -- TODO mv logic here
      case eNotesAndCards of
        Left  err    -> return $ Left err
        Right (notes, cards) -> do
          let insertNote = "INSERT INTO notes VALUES (?,?,?,?,?,?,?,?,?,?,?)"               :: SQLite.Query
              insertCard = "INSERT INTO cards VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)" :: SQLite.Query

          sqlite3 dbPath $ \conn -> do
            mapM_ (SQLite.execute conn insertNote) notes
            mapM_ (SQLite.execute conn insertCard) cards
          return $ Right ()

    tagCards :: [Tag] -> [Card] -> [Card]
    tagCards ts = map (\card@Card{cardTags} -> card { cardTags = nub $ ts ++ cardTags })

    ensureDirsExist :: (Dir, Dir) -> IO (Either String FilePath) -> IO (Either String FilePath)
    ensureDirsExist (origin, dest) io = do
      let fail' = return . Left . (++) "Directories do not exist: \n" . intercalate "\n"
      originAndDestExist <- (,)
        <$> Dir.doesDirectoryExist origin
        <*> Dir.doesDirectoryExist dest
      case originAndDestExist of
        (False, False) -> fail' [origin, dest]
        (False, True ) -> fail' [origin]
        (True , False) -> fail' [dest]
        (True , True ) -> io

    mediaManifest :: [Card] -> MediaManifest
    mediaManifest = MediaManifest . concat . map cardMedia

    writeMediaManifest :: Dir -> MediaManifest -> IO ()
    writeMediaManifest sink manifest = do
      LBS.writeFile (sink ++ "/media") (J.encode manifest)

    -- TODO copyFile throws if file DNE
    cpMediaFiles :: (Dir, Dir) -> MediaManifest -> IO ()
    cpMediaFiles (source, sink) =
      mapM_ (\(num, fp) -> Dir.copyFile (source ++ "/" ++ fp) (sink ++ "/" ++ num)) . manifestPairs

    mkAPKG :: Dir -> DeckName -> IO FilePath
    mkAPKG dir name = genUniqApkgName name >>= \apkgName -> do
      -- TODO
      --   - (Zip library) MonadThrow if invalid -- http://hackage.haskell.org/package/zip-1.1.0/docs/Codec-Archive-Zip.html#v:mkEntrySelector
      --   - (application) check all necessary files exist (including media)

      _ <- Dir.withCurrentDirectory dir $ do
        let files = [ "collection.anki2", "media" ] -- TODO media files

        files' <- mapM (\f -> (,) <$> BS.readFile f <*> Zip.mkEntrySelector f) files -- TODO handle file DNE case

        Zip.createArchive apkgName $ do
          mapM (uncurry $ Zip.addEntry Zip.Deflate) files'

      return $ dir ++ "/" ++ apkgName


    genUniqApkgName :: String -> IO String
    genUniqApkgName name = uniqSlug >>= \slug -> do
      let normalize = map (\c -> if isAlpha c then c else '-') . map toLower
      return $ normalize name ++ "--" ++ slug ++ ".apkg"




---- GENERIC HELPERS ----

posixSeconds :: Time.POSIXTime -> Int
posixSeconds = fromInteger . round

mkTmpDir :: IO Dir
mkTmpDir = do
  slug <- uniqSlug
  let dirName = "/tmp/anki-hs-" ++ slug
  Dir.createDirectory dirName
  return dirName

uniqSlug :: IO String
uniqSlug = do
  rand <- return . show . abs =<< (Random.randomIO :: IO Int)
  time <- return . show =<< Time.getPOSIXTime

  let time' = filter (not . flip elem ['.','s']) $ time

  return $ time' ++ "--" ++ rand

sqlite3 :: FilePath -> (SQLite.Connection -> IO a) -> IO a
sqlite3 sqlite3File runQueries = do
  conn <- SQLite.open sqlite3File
  x <- runQueries conn
  SQLite.close conn
  return x




---- SQLite ----

type DeckForDB = ([SQLiteNote], [SQLiteCard])

buildDeck :: SQLiteCollection -> [Card] -> IO (Either String DeckForDB)
buildDeck col cs = do
  time <- Time.getPOSIXTime

  case parseColModel col of
    Just [model] -> return . Right $ buildDeck' model time cs
    Just _       -> return $ Left notSingleModel
    Nothing      -> return $ Left invalidJSON

  where
    parseColModel :: SQLiteCollection -> Maybe [Model] -- TODO eitherDecode
    parseColModel = J.decode . LBS.pack . T.unpack . sqlite_col_models

    buildDeck' :: Model -> Time.POSIXTime -> [Card] -> DeckForDB
    buildDeck' Model{modelId, deckId} t cs' =
      let seconds = posixSeconds t
          cardsWithIDs = map (bimap (NoteId . (+seconds)) id) . zip [0..] $ cs'
          notes = map (buildNote modelId t) cardsWithIDs
          cards = map (buildCard deckId  t) cardsWithIDs
       in (notes, cards)

    notSingleModel = "Should be only 1 model in `sqlite_col_models`, were "
    invalidJSON    = "`sqlite_col_models` did not have a valid JSON schema"


buildNote :: ModelId -> Time.POSIXTime -> (NoteId,Card) -> SQLiteNote
buildNote (ModelId mid') t (NoteId nid, Card{..}) = do
  SQLiteNote -- SEE DEF BELOW FOR EXPLANATION
    { sqlite_note_id    = nid
    , sqlite_note_guid  = T.pack . show $ nid
    , sqlite_note_mid   = mid'
    , sqlite_note_mod   = posixSeconds t
    , sqlite_note_usn   = -1
    , sqlite_note_tags  = T.pack $ intercalate "," cardTags
    , sqlite_note_flds  = T.pack $ cardFront ++ "\x1F" ++ cardBack
    , sqlite_note_sfld  = T.pack $ cardFrontNoHTML
    , sqlite_note_csum  = T.pack . show . sha1 $ cardFrontNoHTML
    , sqlite_note_flags = 0
    , sqlite_note_data  = ""
    }

  where
    sha1 :: String -> Crypto.Digest Crypto.SHA1
    sha1 = Crypto.hash . BS.pack


buildCard :: DeckId -> Time.POSIXTime -> (NoteId, Card) -> SQLiteCard
buildCard (DeckId did) time (NoteId nid, _) =
  SQLiteCard
    { sqlite_card_id     = nid
    , sqlite_card_nid    = nid
    , sqlite_card_did    = did
    , sqlite_card_ord    = 0
    , sqlite_card_mod    = posixSeconds time
    , sqlite_card_usn    = -1
    , sqlite_card_type   = 0
    , sqlite_card_queue  = 0
    , sqlite_card_due    = 484332854 -- TODO ?????
    , sqlite_card_ivl    = 0
    , sqlite_card_factor = 0
    , sqlite_card_reps   = 0
    , sqlite_card_lapses = 0
    , sqlite_card_left   = 0
    , sqlite_card_odue   = 0
    , sqlite_card_odid   = 0
    , sqlite_card_flags  = 0
    , sqlite_card_data   = ""
    }
