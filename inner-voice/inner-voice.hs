-- 

#include <h>

-- Ascii output
-- Midi output

type Note = (Word8, Word8)

-- pitchclass
type Pc = Word8

-- type Chord = Vector (Maybe Note)
type Chord = Vector (Maybe Pc)

readNoteLtr :: Char -> Word8
readNoteLtr 'C' = 0
readNoteLtr 'D' = 2
readNoteLtr 'E' = 4
readNoteLtr 'F' = 5
readNoteLtr 'G' = 7
readNoteLtr 'A' = 9
readNoteLtr 'B' = 11
readNoteLtr c = error $ "readNoteLtr: " ++ show c

showPc :: Pc -> Text
showPc 0 = "C "
showPc 1 = "C#"
showPc 2 = "D "
showPc 3 = "D#"
showPc 4 = "E "
showPc 5 = "F "
showPc 6 = "F#"
showPc 7 = "G "
showPc 8 = "G#"
showPc 9 = "A "
showPc 10 = "A#"
showPc 11 = "B "

showPcMb :: Maybe Pc -> Text
showPcMb Nothing = ". "
showPcMb (Just 0) = "C "
showPcMb (Just 1) = "C#"
showPcMb (Just 2) = "D "
showPcMb (Just 3) = "D#"
showPcMb (Just 4) = "E "
showPcMb (Just 5) = "F "
showPcMb (Just 6) = "F#"
showPcMb (Just 7) = "G "
showPcMb (Just 8) = "G#"
showPcMb (Just 9) = "A "
showPcMb (Just 10) = "A#"
showPcMb (Just 11) = "B "

readChordNote :: Text -> Maybe Pc
readChordNote t = let
    Just (t1, after1) = T.uncons t
    Just (t2, after2) = T.uncons after1
    Just (t3, after3) = T.uncons after2
    e = error $ "readChordNote: " ++ show t
    finish add octave emptyT =
      if octave >= '0' && octave <= '9' && T.null emptyT
        then Just (readNoteLtr t1 + add)
        else e
  in if
    | T.null t -> e
    | t1 == '.' -> if T.null after1 then Nothing else e
    | t1 < 'A' || t1 > 'G' || T.null after1 -> e
    | t2 == '#' -> finish 1 t3 after3
    | otherwise -> finish 0 t2 after2

readChord :: Text -> Chord
readChord t = V.fromList . map readChordNote $ T.words t

readChords = map readChord . T.lines

--readChords :: Text -> 
--readChords t = fromVector (Z:.6:.V.length v) $ V.concat v
--  where v = V.fromList . map readChord $ T.lines t

allClsSet :: Set Word8
allClsSet = Set.fromList [0..11]

setRandPull :: (MonadRandom m, Ord a) => Set a -> m (a, Set a)
setRandPull s = do
    i <- getRandomR (0, Set.size s - 1)
    return (Set.elemAt i s, Set.deleteAt i s)

fillChord :: RandomGen g => Chord -> Chord -> Rand g Chord
fillChord prevC c = V.fromList <$> go ns0 (V.toList prevC) (V.toList c) where
  ns0 = foldl' (flip Set.delete) allClsSet (catMaybes $ V.toList c)
  go _ [] [] = return []
  go ns (prevN:prevRest) (Nothing:rest) = do
    (n, ns2) <- setRandPull ns
    (Just n:) <$> go ns2 prevRest rest
  go ns (_:prevRest) (n:rest) = (n:) <$> go ns prevRest rest

showChord :: Chord -> Text
showChord = T.intercalate " " . map showPcMb . V.toList

empty6Chord = V.replicate 6 Nothing

fillChords :: RandomGen g => [Chord] -> Rand g [Chord]
fillChords = go empty6Chord where
  go cPrev (c:cs) = liftM2 (:) (fillChord cPrev c) (go c cs)
  go _ [] = return []

main = do
    cs <- readChords <$> T.readFile "ex.txt"
    let ds = evalRand (fillChords cs) (mkStdGen 0)
    mapM_ (T.putStrLn . showChord) ds

{-
readChordNote :: Text -> Maybe Note
readChordNote t = let
    Just (t1, after1) = T.uncons t
    Just (t2, after2) = T.uncons after1
    Just (t3, after3) = T.uncons after2
    e = error $ "readChordNote: " ++ show t
    finish add octave emptyT =
      if octave >= '0' && octave <= '9' && T.null emptyT
        then Just (readNoteLtr t1 + add, fromIntegral $ ord octave - ord '0')
        else e
  in if
    | T.null t -> e
    | t1 == '.' -> if T.null after1 then Nothing else e
    | t1 < 'A' || t1 > 'G' || T.null after1 -> e
    | t2 == '#' -> finish 1 t3 after3
    | otherwise -> finish 0 t2 after2
-}
