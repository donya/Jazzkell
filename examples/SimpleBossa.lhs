Simple bossa nova implementation
Donya Quick

Load this file in GHCi and run "play m" to hear some music.
The lead sheet is finite, so the music will stop on its own.

This module is an example of a very simple, largely deterministic 
implementation of some bossa nova behavior using the JazzTypes framework. 
In this case, there is no use of State information in the bass and 
harmony, but the lead makes use of a very simplistic piece of state 
information (the last pitch played).

> module SimpleBossa where
> import Jazzkell
> import Jazzkell.Utils
> import Euterpea
> import System.Random
> import Data.List (sort)

Utility function to cut a piece of music down to the duration 
of a segment:

> trimTo :: Segment a -> Music a -> Music a
> trimTo seg m = removeZeros $ -- necessary because of a bug in Euterpea 2.0.6's cut/remove functions
>     cut (segDur seg / 4) $ 
>     remove ((snd $ segOnset seg) / 4) m

Our state, which is only used by the soloing algorithm:

> data SimpleState = LastPitch AbsPitch | NullState
>     deriving (Eq, Show)

Simple walking bass pattern following the bossa nova rhythm:

> bassFun :: PartFun AbsPitch s
> bassFun s seg1 seg2 hist g = 
>     let p1 = 36 + (head $ scale $ chordCtxt seg1)
>         p2 = p1 + 7 
>         mPat = note dqn p1 :+: note en p2 :+: note dqn p2 :+: note en p1
>         m = trimTo seg1 (forever mPat)
>     in  case seg2 of Nothing -> (g, s, note (segDur seg1 / 4) p1)
>                      Just _ -> (g, s, m)

Some simple chords following the bossa nova rhythm:

> chordFun :: PartFun AbsPitch s
> chordFun s seg1 seg2 hist g = 
>     let ps = map ((scale $ chordCtxt seg1) !!) [0,2,4,6]
>         mkChord d = chord $ map (note d . (+60)) ps
>         mPat = rest qn :+: mkChord qn :+: rest en :+: mkChord en :+: rest qn
>         m = trimTo seg1 (forever mPat)
>     in  case seg2 of Nothing -> (g, s, mkChord $ segDur seg1)
>                      Just _ -> (g, s, m)

Our solo pitch space:

> soloPSpace = [70..84]

The soloing algorithm does a random walk through the pitch space 
above. It uses the state to ensure smooth transitions across segment
boundaries.

> soloFun :: PartFun AbsPitch SimpleState
> soloFun NullState seg1 seg2 hist g = 
>     let (g',p) = choose g soloPSpace
>     in  soloFun (LastPitch p) seg1 seg2 hist g'
> soloFun (LastPitch lp) seg1 seg2 hist g0 = 
>     let sPSpace = filterByScale (scale $ chordCtxt seg1) soloPSpace
>         n = round (2*segDur seg1) 
>         (g1, g2) = split g0
>         ps = take n $ randMelody g0 sPSpace lp 
>         mel = line $ map (note en) ps
>         lastP = last $ pitches mel
>     in  case seg2 of 
>             Nothing -> (g2, LastPitch (head ps), note (segDur seg1) (head ps))
>             Just _ -> (g2, LastPitch (last ps), mel)

> randMelody :: StdGen -> [AbsPitch] -> AbsPitch -> [AbsPitch]
> randMelody g0 pSpace lastP = 
>     let nearPs = filter (/=lastP) $ orderByNearest pSpace lastP
>         (g1, p) = choose g0 $ take 5 nearPs
>     in  p : randMelody g1 pSpace p

Putting it all together:

> myJB :: JazzBand AbsPitch SimpleState
> myJB = [JazzPart Bass AcousticBass bassFun NullState, 
>         JazzPart Bass ElectricGrandPiano chordFun NullState, 
>         JazzPart Bass Marimba soloFun (LastPitch 70)] 

Finally, we'll test it on a lead sheet.
     
> cM7 = ChordCtxt "CM7" [0,2,4,5,7,9,11] -- C major
> dm7 = ChordCtxt "DmM7" [2,4,5,7,9,10,0] -- D minor
> g7 = ChordCtxt "G7" [7,9,11,0,2,4,5] -- G mixolydian
     
> seg1  = Segment dm7  Regular [] (0,0)   4   (TimeSig 4 4)
> seg2  = Segment g7   Regular [] (1,0)   4   (TimeSig 4 4)
> seg3  = Segment cM7  Regular [] (2,0)   4   (TimeSig 4 4)
> seg4  = Segment cM7  Regular [] (3,0)   4   (TimeSig 4 4)
> seg5  = Segment dm7  Regular [] (4,0)   2   (TimeSig 4 4)
> seg6  = Segment g7   Regular  [] (4,2)   2   (TimeSig 4 4)
> seg7  = Segment dm7  Regular [] (5,0)   2   (TimeSig 4 4)
> seg8  = Segment g7   Regular  [] (5,2)   2   (TimeSig 4 4)
> seg9  = Segment cM7  Regular [] (6,0)   4   (TimeSig 4 4)
> seg10 = Segment g7   Regular [] (7,0)   4   (TimeSig 4 4)
> seg11 = Segment cM7  End     [] (8,0)   4   (TimeSig 4 4)

> (g0, s0, m0) = soloFun (LastPitch 70) seg1 (Just seg1) [] (mkStdGen 6)
> ps0 = pitches m0

> (g1, s1, m1) = soloFun s0 seg1 (Just seg1) [] g0
> ps1 = pitches m1

> (g2, s2, m2) = soloFun s1 seg1 (Just seg1) [] g1
> ps2 = pitches m2

> ls = [seg1, seg2, seg3, seg4, seg5, seg6, seg7, seg8, seg9, seg10, seg11]

> m = runBand myJB [] ls (mkStdGen 6)