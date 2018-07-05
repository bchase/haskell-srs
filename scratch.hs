type Tag = String
type Gloss = String
type Video = FilePath
type Dir = String
type DeckName = String

data Card = Card
  { cardSubs       :: Maybe String
  , cardGlosses    :: [Gloss]
  , cardTags       :: [Tag]
  , cardFrontVideo :: Video
  , cardFrontText  :: Maybe String
  , cardBackText   :: Maybe String
  }


newtype MediaManifest = MediaManifest { unMediaManifest :: [Video] }

writeDeck :: DeckName -> Maybe Tag -> (Dir, Dir) -> [Card] -> IO (Either String FilePath)
writeDeck deckName mTag dirs@(_,sink) cards = do
  let cards' = maybe cards (tagCards tag cards) mTag

  writeMediaManifest dirs cards'
  cpMediaFiles dirs cards'

  writeDeck' sink cards'

  zip sink >>= return . Right -- TODO Eithers

  where
    writeMediaManifest :: (Dir, Dir) -> [Card] -> IO ()
    writeMediaManifest (_, sink) cards = do
      let json = encode . mediaManifest $ cards
      writeFile (sink ++ "/media") json

    mediaManifest :: [Card] -> MediaManifest
    mediaManifest =
      foldl (\m (idx, Card{cardFrontVideo}) -> Map.insert idx cardFrontVideo m) . zip (0..)


    mkdir :: IO ()
    zip :: Dir -> IO FilePath
    zip = undefined
    writeDeck' :: Dir -> [Card] -> IO ()
    writeDeck' = undefined

    ----- DONE -----

    cpMediaFiles :: (Dir, Dir) -> MediaManifest -> IO () -- TODO [Card] instead of MediaManifest
    cpMediaFiles (source, sink) = -- TODO check files exist
      mapM_ ((num, fp) -> cp (source ++ "/" ++ fp) (sink ++ "/" ++ num)) . Map.toList

    tagCards :: Tag -> [Card] -> [Card]
    tagCards tag =
      map (\card@Card{cardTags} -> card { cardTags = tag:cardTags })
