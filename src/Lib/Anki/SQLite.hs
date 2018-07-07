{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Anki.SQLite where

import GHC.Generics   (Generic)

import Data.Map                         (Map)
import Data.Aeson                       (FromJSON(..), Value, (.:))
import Data.Aeson.TH                    (Options(..))
import Data.Aeson.Types                 (Parser)
import Database.SQLite.Simple           (FromRow(..), ToRow(..), SQLData(SQLText), field)
import Database.SQLite.Simple.Internal  (Field(..))
import Database.SQLite.Simple.ToField   (ToField(..))
import Database.SQLite.Simple.FromField (FromField(..), ResultError(ConversionFailed), returnError)

import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Aeson                 as J
import qualified Data.Aeson.TH              as J
import qualified Data.HashMap.Strict        as HashMap


newtype NoteId  = NoteId  { unNoteId  :: Int }
newtype DeckId  = DeckId  { unDeckId  :: Int }
newtype ModelId = ModelId { unModelId :: Int }

data Model = Model
  { modelId :: ModelId
  , deckId  :: DeckId
  }

-- https://stackoverflow.com/questions/42578331/aeson-parse-json-with-unknown-key-in-haskell
-- TODO INSTEAD? -> https://stackoverflow.com/questions/17278472/arbitrary-json-keys-with-aeson-haskell
instance {-# OVERLAPS #-} FromJSON [Model] where -- TODO OVERLAPS
  parseJSON x = parseJSON x >>= mapM parseModel . HashMap.toList
parseModel :: (String, Value) -> Parser Model
parseModel (mid, json) =
  flip (J.withObject "ModelId") json $ \obj ->
    Model
      <$> (ModelId <$> (return $ read mid)) -- TODO readMay
      <*> (DeckId  <$> obj .: "did")

type SQLiteDecksJSON = Map String SQLiteDeckJSON

data SQLiteCollection = SQLiteCollection
  { sqlite_col_id     :: Int
  , sqlite_col_crt    :: Int -- crt ("created"?)
  , sqlite_col_mod    :: Int -- mod ("modified"?)
  , sqlite_col_scm    :: Int -- scm (another UNIX time)
  , sqlite_col_ver    :: Int
  , sqlite_col_dty    :: Int
  , sqlite_col_usn    :: Int
  , sqlite_col_ls     :: Int
  , sqlite_col_conf   :: T.Text          -- configuration of the collection
  , sqlite_col_models :: T.Text          -- models available in the collection
  , sqlite_col_decks  :: SQLiteDecksJSON -- decks of the collection
  , sqlite_col_dconf  :: T.Text          -- configuration of each deck
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


data SQLiteDeckJSON = SQLiteDeckJSON
  { sqlite_deck_json_desc      :: String
  , sqlite_deck_json_name      :: String
  , sqlite_deck_json_extendRev :: Int
  , sqlite_deck_json_usn       :: Int
  , sqlite_deck_json_collapsed :: Bool
  , sqlite_deck_json_newToday  :: [Int]
  , sqlite_deck_json_timeToday :: [Int]
  , sqlite_deck_json_dyn       :: Int
  , sqlite_deck_json_extendNew :: Int
  , sqlite_deck_json_conf      :: Int
  , sqlite_deck_json_revToday  :: [Int]
  , sqlite_deck_json_lrnToday  :: [Int]
  , sqlite_deck_json_id        :: Int
  , sqlite_deck_json_mod       :: Int
  } deriving ( Generic, Show )
$(J.deriveJSON J.defaultOptions { fieldLabelModifier = drop 17 } ''SQLiteDeckJSON)

instance FromField (Map String SQLiteDeckJSON) where
  fromField f@(Field (SQLText t) _) =
    case J.eitherDecode . LBS.pack . T.unpack $ t of
      Left  err  -> returnError ConversionFailed f $ "SQLiteDeckJSON JSON parse error: " ++ err ++ "\n" ++ T.unpack t
      Right deck -> return deck

  fromField f = returnError ConversionFailed f "expecting SQLText column type"

instance ToField (Map String SQLiteDeckJSON) where
  toField = SQLText . T.pack . LBS.unpack . J.encode


-- -- TODO rm if compiles
-- newtype SQLiteDecksJSON = SQLiteDecksJSON
--   { unDecksJSON :: Map String SQLiteDeckJSON
--   } deriving ( Generic )
-- instance FromJSON SQLiteDecksJSON
-- instance ToJSON   SQLiteDecksJSON


-- http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html#ghc-flag--Wmissing-methods
-- https://stackoverflow.com/questions/13576792/how-to-silence-warnings-of-a-not-fully-implemented-class
-- https://ghc.haskell.org/trac/ghc/ticket/10150
-- https://ghc.haskell.org/trac/ghc/ticket/3820
instance FromRow SQLiteCollection where -- TODO Generic & suppress warning
  fromRow =
    SQLiteCollection
      <$> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field <*> field <*> field
      <*> field <*> field <*> field


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
