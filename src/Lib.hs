{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib where

import           Data.Char      (toLower)
import           Data.List      (intercalate)
import           Data.Monoid    ((<>), mempty)
import           Data.Bifunctor (bimap)
import           GHC.Generics   (Generic)

import           Data.Aeson.Types               (Parser)
import           Data.Aeson                     (FromJSON(..), ToJSON(..), Value,
                                                 (.=), (.:), pairs, encode, withObject, decode)
import qualified Data.Text                      as T
import qualified Data.HashMap.Strict            as HashMap
import qualified Data.ByteString.Lazy.Char8     as BSL
import qualified Data.ByteString.Char8          as BS
import           Data.Time.Clock.POSIX          as Time
import           System.Random                  as Random
import           System.Directory               as Dir
import qualified Codec.Archive.Zip              as Zip
import qualified Crypto.Hash                    as Crypto
import qualified Database.SQLite.Simple         as SQLite
import           Database.SQLite.Simple         (ToRow(..))
import           Database.SQLite.Simple.ToField (ToField(..))


type Video = FilePath
type Dir = FilePath

type Tag = String
type Gloss = String
type DeckName = String

data VideoCard = VideoCard
  { videoCardSubs       :: Maybe String
  , videoCardGlosses    :: [Gloss]
  , videoCardTags       :: [Tag]
  , videoCardFrontVideo :: Video
  , videoCardFrontText  :: Maybe String
  , videoCardBackText   :: Maybe String
  }

data Card = Card
  { cardFront       :: String
  , cardFrontNoHTML :: String
  , cardBack        :: String
  , cardTags        :: [Tag]
  , cardMedia       :: [FilePath]
  }



newtype MediaManifest = MediaManifest { unMediaManifest :: [Video] } deriving ( Generic )

manifestPairs :: MediaManifest -> [(String, Video)]
manifestPairs = map (bimap show id) . zip ([0..] :: [Int]) . unMediaManifest

instance ToJSON MediaManifest where
  toEncoding = pairs . foldl (\json (n,f) -> (n .= f) <> json) mempty . map (bimap T.pack id) . manifestPairs



main :: IO ()
main = do
  tmp <- mkTmpDir
  _ <- writeDeck "" (Just "example-tag") ("source/", tmp) []
  return ()

  where
    mkTmpDir :: IO Dir
    mkTmpDir = do
      dirName <- uniqSlug
      Dir.createDirectory dirName
      return dirName


writeDeck :: DeckName -> Maybe Tag -> (Dir, Dir) -> [Card] -> IO (Either String FilePath)
writeDeck deckName mTag dirs origCards = do
  let cards = tagCards origCards mTag

  -- mNotesAndCards <- buildDeck col cards

  ensureDirsExist dirs $ do
    let mediaManifest' = mediaManifest cards
    cpMediaFiles dirs mediaManifest'
    writeMediaManifest dirs mediaManifest'

    writeDeckSqlite3 dirs deckName cards

    mkAPKG dirs deckName >>= return . Right -- TODO Eithers

  where
    writeDeckSqlite3 :: (Dir, Dir) -> DeckName -> [Card] -> IO ()
    writeDeckSqlite3 _ _ _ = do
      -- let sep = "\x1F"
      let createTableCmds = []
      --     insertCollection = ""
      --     insertNotes = ""
      --     insertCards = ""
      --
      conn <- SQLite.open "TODO.db" -- TODO .db
      mapM_ (SQLite.execute_ conn) createTableCmds
      -- SQLite.execute conn insertCollection
      -- mapM_ (SQLite.execute  conn) insertNotes
      -- mapM_ (SQLite.execute  conn) insertCards
      SQLite.close conn


    ----- DONE -----

    fail' = return . Left . (++) "Directories do not exist: \n" . intercalate "\n"

    tagCards :: [Card] -> Maybe Tag -> [Card]
    tagCards cs = maybe cs (flip tagCards' cs)
      where
        tagCards' :: Tag -> [Card] -> [Card]
        tagCards' tag =
          map (\card@Card{cardTags} -> card { cardTags = tag:cardTags })

    ensureDirsExist :: (Dir, Dir) -> IO (Either String FilePath) -> IO (Either String FilePath)
    ensureDirsExist (origin, dest) io = do
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

    writeMediaManifest :: (Dir, Dir) -> MediaManifest -> IO ()
    writeMediaManifest (_, sink) manifest = do
      BSL.writeFile (sink ++ "/media") (encode manifest)

    -- TODO copyFile throws if file DNE
    cpMediaFiles :: (Dir, Dir) -> MediaManifest -> IO ()
    cpMediaFiles (source, sink) =
      mapM_ (\(num, fp) -> Dir.copyFile (source ++ "/" ++ fp) (sink ++ "/" ++ num)) . manifestPairs

    mkAPKG :: (Dir, Dir) -> DeckName -> IO FilePath
    mkAPKG (dir, _) name = do
      slug <- uniqSlug

      let normalize = map (\c -> if c `elem` ['a'..'z'] then c else '-') . map toLower
          apkgPath = flip (++) ("--" ++ slug ++ ".apkg") . normalize $ name

      contents <- Dir.getDirectoryContents dir

      -- TODO
      --   - (Zip library) MonadThrow if invalid -- http://hackage.haskell.org/package/zip-1.1.0/docs/Codec-Archive-Zip.html#v:mkEntrySelector
      --   - (application) check all necessary files exist (including media)
      files <- mapM (\file -> Zip.mkEntrySelector $ dir ++ "/" ++ file) contents

      _ <- Zip.createArchive apkgPath $ do
        mapM (Zip.addEntry Zip.Store (BS.pack "")) files

      return apkgPath




---- GENERIC HELPERS ----

posixSeconds :: Time.POSIXTime -> Int
posixSeconds = fromInteger . round

uniqSlug :: IO String
uniqSlug = do
  rand <- return . show . abs =<< (Random.randomIO :: IO Int)
  time <- return . show =<< Time.getPOSIXTime

  let time' = filter (not . flip elem ['.','s']) $ time

  return $ time' ++ "--" ++ rand



type DeckForDB = ([SQLiteNote], [SQLiteCard])

buildDeck :: SQLiteCollection -> [Card] -> IO (Either String DeckForDB)
buildDeck col cs = do
  time <- getPOSIXTime

  case parseColModel col of
    Just [model] -> return . Right $ buildDeck' model time cs
    Just _       -> return $ Left notSingleModel
    Nothing      -> return $ Left invalidJSON

  where
    parseColModel :: SQLiteCollection -> Maybe [Model]
    parseColModel = decode . BSL.pack . T.unpack . sqlite_col_models

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



newtype NoteId  = NoteId  { unNoteId  :: Int }
newtype DeckId  = DeckId  { unDeckId  :: Int }
newtype ModelId = ModelId { unModelId :: Int }

data Model = Model
  { modelId :: ModelId
  , deckId  :: DeckId
  }

-- https://stackoverflow.com/questions/42578331/aeson-parse-json-with-unknown-key-in-haskell
instance {-# OVERLAPS #-} FromJSON [Model] where -- TODO OVERLAPS
  parseJSON x = parseJSON x >>= mapM parseModel . HashMap.toList
parseModel :: (String, Value) -> Parser Model
parseModel (mid, json) =
  flip (withObject "ModelId") json $ \obj ->
    Model
      <$> (ModelId <$> (return $ read mid)) -- TODO readMay
      <*> (DeckId  <$> obj .: "did")


data SQLiteCollection = SQLiteCollection
  { sqlite_col_id     :: Int
  , sqlite_col_crt    :: Int    -- crt ("created"?)
  , sqlite_col_mod    :: Int    -- mod ("modified"?)
  , sqlite_col_scm    :: Int    -- scm (another UNIX time)
  , sqlite_col_ver    :: Int
  , sqlite_col_dty    :: Int
  , sqlite_col_usn    :: Int
  , sqlite_col_ls     :: Int
  , sqlite_col_conf   :: T.Text -- configuration of the collection
  , sqlite_col_models :: T.Text -- models available in the collection
  , sqlite_col_decks  :: T.Text -- decks of the collection. first is "default", second is target deck
  , sqlite_col_dconf  :: T.Text -- configuration of each deck
  , sqlite_col_tags   :: T.Text
  }

data SQLiteNote = SQLiteNote     -- <<< http://decks.wikia.com/wiki/Anki_APKG_format_documentation >>>
  { sqlite_note_id    :: Int     -- the note id, generate it randomly.
  , sqlite_note_guid  :: T.Text  -- a GUID identifier, generate it randomly.
  , sqlite_note_mid   :: Int     -- identifier of the model, use the one found in the models JSON section.
  , sqlite_note_mod   :: Int     -- replace with current time (seconds since 1970).
  , sqlite_note_usn   :: Int     -- ((default))
  , sqlite_note_tags  :: T.Text  -- tags, visible to the user, which can be used to filter cards (e.g. "verb")
  , sqlite_note_flds  :: T.Text  -- card content, front and back, separated by \x1f char.
  , sqlite_note_sfld  :: T.Text  -- card front content without html (first part of flds, filtered).
  , sqlite_note_csum  :: T.Text  -- a string SHA1 checksum of sfld, limited to 8 digits
  , sqlite_note_flags :: Int     -- ((default))
  , sqlite_note_data  :: T.Text  -- ((empty))
  }

data SQLiteCard = SQLiteCard
  { sqlite_card_id     :: Int -- card ID, generate randomly
  , sqlite_card_nid    :: Int -- note ID
  , sqlite_card_did    :: Int -- deck ID, per `models` JSON on collection (`col` row)
  , sqlite_card_ord    :: Int
  , sqlite_card_mod    :: Int
  , sqlite_card_usn    :: Int
  , sqlite_card_type   :: Int
  , sqlite_card_queue  :: Int
  , sqlite_card_due    :: Int
  , sqlite_card_ivl    :: Int
  , sqlite_card_factor :: Int
  , sqlite_card_reps   :: Int
  , sqlite_card_lapses :: Int
  , sqlite_card_left   :: Int
  , sqlite_card_odue   :: Int
  , sqlite_card_odid   :: Int
  , sqlite_card_flags  :: Int
  , sqlite_card_data   :: T.Text
  }

instance ToRow SQLiteNote where
  toRow SQLiteNote{..} =
    toRow
      [ toField sqlite_note_id
      , toField sqlite_note_guid
      , toField sqlite_note_mid
      , toField sqlite_note_mod
      , toField sqlite_note_usn
      , toField sqlite_note_tags
      , toField sqlite_note_flds
      , toField sqlite_note_sfld
      , toField sqlite_note_csum
      , toField sqlite_note_flags
      , toField sqlite_note_data
      ]

instance ToRow SQLiteCard where
  toRow SQLiteCard{..} =
    toRow
      [ toField sqlite_card_id
      , toField sqlite_card_nid
      , toField sqlite_card_did
      , toField sqlite_card_ord
      , toField sqlite_card_mod
      , toField sqlite_card_usn
      , toField sqlite_card_type
      , toField sqlite_card_queue
      , toField sqlite_card_due
      , toField sqlite_card_ivl
      , toField sqlite_card_factor
      , toField sqlite_card_reps
      , toField sqlite_card_lapses
      , toField sqlite_card_left
      , toField sqlite_card_odue
      , toField sqlite_card_odid
      , toField sqlite_card_flags
      , toField sqlite_card_data
      ]
