module Chapter3 where

import Euterpea
import Codec.SoundFont (Inst(Inst))

-- map example in section 3.2.2

-- generate six-note whole tone scale
-- Note that there are errors in the text: (1) the definition is missing the 10 in the list
-- only creates 5 notes, (2) the octave doesn't switch
wts :: Pitch -> [Music Pitch]
wts p = let f ap = note qn (pitch (absPitch p + ap))
        in map f [0, 2, 4, 6, 8, 10]

-- Exercise 3.1

-- Example: f1 7 [(C,4), (D,4), (E,4)] => [(G,4),(A,4),(B,4)]
f1 :: Int -> [Pitch] -> [Pitch]
f1 n = map (trans n)

-- Example: f2 $ take 4 (repeat (1/4 ::Dur)) => [Prim (Rest (1 % 4)),Prim (Rest (1 % 4)),Prim (Rest (1 % 4)),Prim (Rest (1 % 4))]
f2 :: [Dur] -> [Music a]
f2 = map rest 

-- Example: f3 [c 4 qn, d 4 en, e 4 hn] 
f3 :: [Music Pitch] -> [Music Pitch]
f3 = map halvePlusRest
    where
        halvePlusRest m =
            case m of
                Prim (Note d p) -> note (d/2) p :+: rest (d/2)
                _ -> m

-- Exercise 3.9
fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse durs ns = 
    zipWith (\d n -> n d) durs ns
 
-- Exercise 3.10
maxAbsPitch :: [AbsPitch] -> AbsPitch
maxAbsPitch aps = maximum aps

minAbsPitch :: [AbsPitch] -> AbsPitch
minAbsPitch aps = minimum aps

-- Exercise 3.11
semiToneUp :: Pitch -> Pitch
semiToneUp p = pitch (absPitch p + 1)

semiToneDown :: Pitch -> Pitch 
semiToneDown p = pitch (absPitch p - 1)

-- A scale is a sequence, so use line to turn [Music Pitch] into Music Pitch
chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 = line $ zipWith note (cycle [qn]) pitches
    where
        pitches = if absPitch p1 < absPitch p2
                    then chrom' semiToneUp p1 p2
                    else chrom' semiToneDown p1 p2
        chrom' :: (Pitch -> Pitch) -> Pitch -> Pitch -> [Pitch]
        chrom' stFunc p end 
            | absPitch p == absPitch end = [end]
            | otherwise                  = p : chrom' stFunc (stFunc p) end 
    

-- Exercise 3.12
interval :: Pitch -> Int -> Pitch 
interval p i = pitch (absPitch p + i)

mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p intervals = line $ zipWith note (cycle [qn]) (scanl interval p intervals)

-- Exercise 3.13
genScale :: Mode -> Pitch -> Music Pitch
genScale mode p = mkScale p intervals 
    where
        intervals = 
            case mode of
                Ionian      -> [2,2,1,2,2,2]
                Dorian      -> [2,1,2,2,2,1]
                Phrygian    -> [1,2,2,2,1,2]
                Lydian      -> [2,2,2,1,2,2]
                Mixolydian  -> [2,2,1,2,2,1]
                Aeolian     -> [2,1,2,2,1,2]
                Locrian     -> [1,2,2,1,2,2]

-- Exercise 3.14

-- to repeat: play $ (times n (line melody)
-- rests: (offset dur (line melody))
-- multiple voices: (Modify (Instrument Vibraphone))(times 2 (line melody)) :=: (Modify (Instrument Flute)) (offset (2*wn) (line melody))   
frereJacques = [ c 4 qn, d 4 qn, e 4 qn, c 4 qn, c 4 qn, d 4 qn, e 4 qn, c 4 qn
         , e 4 qn, f 4 qn, g 4 hn, e 4 qn, f 4 qn, g 4 hn
         , g 4 en, a 4 en, g 4 en, f 4 en, e 4 qn, c 4 qn, g 4 en, a 4 en, g 4 en, f 4 en, e 4 qn, c 4 qn,
          c 4 qn, g 3 qn, c 4 hn, c 4 qn, g 3 qn, c 4 hn]

pachabel = [fs 4 hn, e 4 hn, d 4 hn, cs 4 hn, b 3 hn, a 3 hn, b 3 hn, cs 4 hn, d 5 hn, cs 5 hn, b 4 hn, a 4 hn
            , g 4 hn, fs 4 hn, g 4 hn, a 4 hn, d 5 hn, cs 5 hn, b 4 hn, a 4 hn, g 4 hn, fs 4 hn, g 4 hn, a 4 hn ]

mkRound :: Int -> [Music Pitch] -> [InstrumentName] -> Music Pitch
mkRound reps melody voices =
    chord $ map (\(v, o, r) -> (Modify (Instrument v)) (offset (o*wn) (times r (line melody)))) inputs
    where
        offsets = [2 * n | n <- [1 .. fromIntegral (length voices)]]
        inputs = zip3 voices offsets (repeat reps)
