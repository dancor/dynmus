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

showPc :: Pc -> String
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

showPcMb :: Maybe Pc -> String
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
readChord t = V.fromList. map readChordNote $ T.words t

readChords = map readChord . T.lines

allClsSet :: Set Word8
allClsSet = Set.fromList [0..11]

setRandPull :: (MonadRandom m, Ord a) => Set a -> m (a, Set a)
setRandPull s = do
    i <- getRandomR (0, Set.size s - 1)
    return (Set.elemAt i s, Set.deleteAt i s)

fillChord :: RandomGen g => Chord -> Rand g Chord
fillChord c = V.fromList <$> go ns0 (V.toList c) where
  ns0 = foldl' (flip Set.delete) allClsSet (catMaybes $ V.toList c)
  go _ [] = return []
  go ns (Nothing:rest) = do
    (n, ns2) <- setRandPull ns
    (Just n:) <$> go ns2 rest
  go ns (n:rest) = (n:) <$> go ns rest

showChord = intercalate " " . map showPcMb . V.toList

main = do
    c1:_ <- readChords <$> T.readFile "ex.txt"
    --mapM_ print c
    putStrLn . showChord $ evalRand (fillChord c1) (mkStdGen 0)

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
