{-# LANGUAGE DeriveAnyClass #-}

import Data.List

import Euterpea

-- Exercise 2.1

data Triad = Triad Pitch Pitch Pitch
  deriving Show

halfSteps :: [PitchClass]
halfSteps = cycle [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]

degrees :: [PitchClass]
degrees = cycle [C, D, E, F, G, A, B]

degree :: Int -> PitchClass -> PitchClass
degree n pc = 
  let
    Just i = elemIndex pc degrees
  in
    degrees !! (i - 1 + n)

octave :: Octave -> Int -> Octave
octave o steps = o + (steps `div` 12)

majorTriad :: Pitch -> Triad
majorTriad (root, o) =
  let
    Just i = elemIndex root halfSteps
    second = i + 4
    third = i + 7
  in 
    Triad (root, o) (halfSteps !! second, octave o second) (halfSteps !! third, octave o third)

minorTriad :: Pitch -> Triad
minorTriad (root, o) =
  let 
    Just i = elemIndex root halfSteps
    second = i + 3
    third = i + 7
  in 
    Triad (root, o) (halfSteps !! second, octave o second) (halfSteps !! third, octave o third)

triadChord :: Triad -> Dur -> Music Pitch
triadChord (Triad p1 p2 p3) d =
  note d p1 :=: note d p2 :=: note d p3

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = 
  let
    (pc, o) = p
    chord1 = triadChord (minorTriad ((degree 2 pc), o)) d
    chord2 = triadChord (majorTriad ((degree 5 pc), o)) d
    chord3 = triadChord (majorTriad ((degree 1 pc), o)) (2*d)
  in 
    chord1 :+: chord2 :+: chord3

-- Exercise 2.2

data BluesPitchClass
  = Ro  -- root == C
  | MT  -- minor third == Ef
  | Fo  -- fourth == F
  | Fi  -- fifh == G
  | MS  -- minor seventh == Bf

type BluesPitch = (BluesPitchClass, Octave)

ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro, o)
mt o d = note d (MT, o)
fo o d = note d (Fo, o)
fi o d = note d (Fi, o)
ms o d = note d (MS, o)

fromBluesPitchClass :: BluesPitchClass -> PitchClass
fromBluesPitchClass Ro = C
fromBluesPitchClass MT = Ef
fromBluesPitchClass Fo = F
fromBluesPitchClass Fi = G
fromBluesPitchClass MS = Bf

fromBluesPitch :: BluesPitch -> Pitch
fromBluesPitch (p, o) = (fromBluesPitchClass p, o)

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d p)) = Prim (Note d (fromBluesPitch p))
fromBlues (Prim (Rest d)) = Prim (Rest d)
fromBlues (m1 :+: m2) = (fromBlues m1) :+: (fromBlues m2)
fromBlues (m1 :=: m2) = (fromBlues m1) :=: (fromBlues m2)
fromBlues (Modify ctl m) = Modify ctl (fromBlues m)


