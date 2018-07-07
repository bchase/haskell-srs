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

import           Data.Aeson.Types            (Parser)
import           Data.Aeson                  (FromJSON(..), ToJSON(..), Value,
                                              (.=), (.:), pairs, encode, withObject, decode)
import qualified Data.Text                   as T
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.ByteString.Lazy.Char8  as BSL
import qualified Data.ByteString.Char8       as BS
import           Data.Time.Clock.POSIX       as Time
import           System.Random               as Random
import           System.Directory            as Dir
import qualified Codec.Archive.Zip           as Zip
import qualified Database.SQLite.Simple      as SQLite
import qualified Crypto.Hash                 as Crypto


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

uniqSlug :: IO String
uniqSlug = do
  rand <- return . show . abs =<< (Random.randomIO :: IO Int)
  time <- return . show =<< Time.getPOSIXTime

  let time' = filter (not . flip elem ['.','s']) $ time

  return $ time' ++ "--" ++ rand



-- sqlite> SELECT * FROM col;
-- id|crt|mod|scm|ver|dty|usn|ls|conf|models|decks|dconf|tags
-- 1|1530518400|1530820153785|1530820153766|11|0|0|0|{"nextPos": 1, "estTimes": true, "activeDecks": [1], "sortType": "noteFld", "timeLim": 0, "sortBackwards": false, "addToCur": true, "curDeck": 1, "newBury": true, "newSpread": 0, "dueCounts": true, "curModel": "1530820153767", "collapseTime": 1200}|{"1530549901640": {"vers": [], "name": "Basic", "tags": ["export-test-tag"], "did": 1530820024179, "usn": -1, "req": [[0, "all", [0]]], "flds": [{"name": "Front", "media": [], "sticky": false, "rtl": false, "ord": 0, "font": "Arial", "size": 20}, {"name": "Back", "media": [], "sticky": false, "rtl": false, "ord": 1, "font": "Arial", "size": 20}], "sortf": 0, "latexPre": "\\documentclass[12pt]{article}\n\\special{papersize=3in,5in}\n\\usepackage[utf8]{inputenc}\n\\usepackage{amssymb,amsmath}\n\\pagestyle{empty}\n\\setlength{\\parindent}{0in}\n\\begin{document}\n", "tmpls": [{"name": "Card 1", "qfmt": "{{Front}}", "did": null, "bafmt": "", "afmt": "{{FrontSide}}\n\n<hr id=answer>\n\n{{Back}}", "ord": 0, "bqfmt": ""}], "latexPost": "\\end{document}", "type": 0, "id": "1530549901640", "css": ".card {\n font-family: arial;\n font-size: 20px;\n text-align: center;\n color: black;\n background-color: white;\n}\n", "mod": 1530820114}}|{"1": {"desc": "", "name": "Default", "extendRev": 50, "usn": 0, "collapsed": false, "newToday": [0, 0], "timeToday": [0, 0], "dyn": 0, "extendNew": 10, "conf": 1, "revToday": [0, 0], "lrnToday": [0, 0], "id": 1, "mod": 1530820153}, "1530820024179": {"desc": "", "name": "export-test", "extendRev": 50, "usn": -1, "collapsed": false, "browserCollapsed": true, "newToday": [3, 0], "timeToday": [3, 0], "dyn": 0, "extendNew": 10, "conf": 1, "revToday": [3, 0], "lrnToday": [3, 0], "id": 1530820024179, "mod": 1530820114}}|{"1": {"name": "Default", "replayq": true, "lapse": {"leechFails": 8, "minInt": 1, "delays": [10], "leechAction": 0, "mult": 0}, "rev": {"perDay": 100, "fuzz": 0.05, "ivlFct": 1, "maxIvl": 36500, "ease4": 1.3, "bury": true, "minSpace": 1}, "timer": 0, "maxTaken": 60, "usn": 0, "new": {"perDay": 20, "delays": [1, 10], "separate": true, "ints": [1, 4, 7], "initialFactor": 2500, "bury": true, "order": 1}, "mod": 0, "id": 1, "autoplay": true}}|{}


-- sqlite> .schema col
-- CREATE TABLE col (
--     id              integer primary key,
--     crt             integer not null,
--     mod             integer not null,
--     scm             integer not null,
--     ver             integer not null,
--     dty             integer not null,
--     usn             integer not null,
--     ls              integer not null,
--     conf            text not null,
--     models          text not null,
--     decks           text not null,
--     dconf           text not null,
--     tags            text not null
-- );
-- sqlite> .schema notes
-- CREATE TABLE notes (
--     id              integer primary key,   /* 0 */
--     guid            text not null,         /* 1 */
--     mid             integer not null,      /* 2 */
--     mod             integer not null,      /* 3 */
--     usn             integer not null,      /* 4 */
--     tags            text not null,         /* 5 */
--     flds            text not null,         /* 6 */
--     sfld            integer not null,      /* 7 */
--     csum            integer not null,      /* 8 */
--     flags           integer not null,      /* 9 */
--     data            text not null          /* 10 */
-- );
-- CREATE INDEX ix_notes_usn on notes (usn);
-- CREATE INDEX ix_notes_csum on notes (csum);
-- sqlite> .schema cards
-- CREATE TABLE cards (
--     id              integer primary key,   /* 0 */
--     nid             integer not null,      /* 1 */
--     did             integer not null,      /* 2 */
--     ord             integer not null,      /* 3 */
--     mod             integer not null,      /* 4 */
--     usn             integer not null,      /* 5 */
--     type            integer not null,      /* 6 */
--     queue           integer not null,      /* 7 */
--     due             integer not null,      /* 8 */
--     ivl             integer not null,      /* 9 */
--     factor          integer not null,      /* 10 */
--     reps            integer not null,      /* 11 */
--     lapses          integer not null,      /* 12 */
--     left            integer not null,      /* 13 */
--     odue            integer not null,      /* 14 */
--     odid            integer not null,      /* 15 */
--     flags           integer not null,      /* 16 */
--     data            text not null          /* 17 */
-- );
-- CREATE INDEX ix_cards_usn on cards (usn);
-- CREATE INDEX ix_cards_nid on cards (nid);
-- CREATE INDEX ix_cards_sched on cards (did, queue, due);


-- cardFront :: Card -> T.Text
-- cardFront Card{..} =
-- cardBack :: Card -> T.Text
-- cardBack Card{..} =



-- buildNotes :: [Card] -> IO [SQLiteNote]
-- buildNotes cards = do
--   time <- Time.getPOSIXTime
--
--   let cards' = map

type DeckForDB = (SQLiteCollection, [SQLiteNote])

createDeck :: SQLiteCollection -> [Card] -> IO (Either String DeckForDB)
createDeck col cards = do
  time <- getPOSIXTime

  case parseMIDs col of
    Just [mid] -> return $ Right (col, buildNotes time mid cards)
    Just _     -> return $ Left notSingleModel
    Nothing    -> return $ Left invalidJSON

  where
    parseMIDs :: SQLiteCollection -> Maybe [AnkiModelId]
    parseMIDs = fmap (map ankiModelId) . decode . BSL.pack . T.unpack . sqlite_col_models

    notSingleModel = "Should be only 1 model in `sqlite_col_models`, were "
    invalidJSON    = "`sqlite_col_models` did not have a valid JSON schema"


buildNotes :: Time.POSIXTime -> AnkiModelId -> [Card] -> [SQLiteNote]
buildNotes time mid = map (uncurry (note mid time)) . zip [0..]


note :: AnkiModelId -> Time.POSIXTime -> Int -> Card -> SQLiteNote
note (AnkiModelId mid) time idx Card{..} = do
  let mod' = fromInteger . round $ time
      id'  = mod' + idx

  SQLiteNote -- SEE DEF BELOW FOR EXPLANATION
    { sqlite_note_id    = mod' + id'
    , sqlite_note_guid  = T.pack . show $ id'
    , sqlite_note_mid   = mid
    , sqlite_note_mod   = mod'
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









data AnkiModel = AnkiModel
  { ankiModelId :: AnkiModelId
  , ankiDeckId  :: AnkiDeckId
  }
-- data AnkiModel = AnkiModel AnkiModelId AnkiDeckId
newtype AnkiDeckId  = AnkiDeckId  { unAnkiDeckId  :: Int }
newtype AnkiModelId = AnkiModelId { unAnkiModelId :: Int }

-- https://stackoverflow.com/questions/42578331/aeson-parse-json-with-unknown-key-in-haskell
instance {-# OVERLAPS #-} FromJSON [AnkiModel] where -- TODO OVERLAPS
  parseJSON x = parseJSON x >>= mapM parseModel . HashMap.toList
parseModel :: (String, Value) -> Parser AnkiModel
parseModel (mid, json) =
  flip (withObject "AnkiModelId") json $ \obj ->
    AnkiModel
      <$> (AnkiModelId <$> (return $ read mid)) -- TODO readMay
      <*> (AnkiDeckId  <$> obj .: "did")


data SQLiteCollection = SQLiteCollection
  { sqlite_col_id     :: Int
  , sqlite_col_crt    :: Int
  , sqlite_col_mod    :: Int
  , sqlite_col_scm    :: Int
  , sqlite_col_ver    :: Int
  , sqlite_col_dty    :: Int
  , sqlite_col_usn    :: Int
  , sqlite_col_ls     :: Int
  , sqlite_col_conf   :: T.Text
  , sqlite_col_models :: T.Text
  , sqlite_col_decks  :: T.Text
  , sqlite_col_dconf  :: T.Text
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

-- instance SQLite.ToRow SQLiteNote where
--   id_
--   guid
--   mid
--   mod
--   usn
--   tags
--   flds
--   sfld
--   csum
--   flags
--   data_

data SQLiteCard = SQLiteCard
  { sqlite_card_id     :: Int
  , sqlite_card_nid    :: Int
  , sqlite_card_did    :: Int
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
