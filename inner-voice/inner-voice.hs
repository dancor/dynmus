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

fillChord :: RandomGen g => Chord -> Rand g Chord
fillChord c = V.fromList <$> go (V.toList c) where
  go [] = return []
  go (Nothing:rest) = liftM2 (:) (Just <$> getRandomR (0, 11)) (go rest)
  go (n:rest) = (n :) <$> go rest

main = do
    c1:_ <- readChords <$> T.readFile "ex.txt"
    --mapM_ print c
    print $ evalRand (fillChord c1) (mkStdGen 0)

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
