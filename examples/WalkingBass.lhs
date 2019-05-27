Walking bass with ornaments
Donya Quick

The implementation here is much like in SimpleWalkingBass.lhs, 
but it adds ornaments between some pitches.

> module BassGen where
> import Euterpea
> import Jazzkell
> import Jazzkell.Utils
> import System.Random
> import SimpleWalkingBass hiding (m, myJB)

First, the ornaments function. This works by taking a list of 
pitches, assumed to be one per beat, and adding ornaments. The
result is a Music (AbsPitch, Volume) value.

> bassToMusic :: [AbsPitch] -> StdGen -> Music (AbsPitch, Volume)
> bassToMusic [] g = rest 0
> bassToMusic [x] g = note wn (x,120)
> bassToMusic (x1:x2:xs) g =
>     let (r, g1) = randomR (0.0, 1.0::Double) g
>         m = if r < 0.85 then note qn (x1, 120) else
>             if r < 0.90 then note (3*sn) (x1, 120) :+: note (sn) (x2+1, 80) else
>             if r < 0.95 then note (3*sn) (x1, 120) :+: note (sn) (x1, 80)
>             else note (3*sn) (x1, 120) :+: note (sn) (x2-1, 80)
>     in  m :+: bassToMusic (x2:xs) g1

Now the PartFun, which is basically like the simple walkig bass function.

> wBassFun2 :: PartFun (AbsPitch, Volume) WalkingState
> wBassFun2 NullState seg1 seg2 hist g = 
>     let scale1 = scale $ chordCtxt seg1
>         pSpace = filter (\p -> (elem (mod p 12) scale1)) bassRange
>         roots = filter (\p -> mod p 12 == scale1 !! 0) pSpace
>         (g', r) = choose g roots
>         beats = round (segDur seg1)
>     in  wBassFun2 (NextRoot r) seg1 seg2 hist g
> wBassFun2 (NextRoot r) seg1 Nothing hist g = 
>     (g, NullState, note (segDur seg1) (r, 100))
> wBassFun2 (NextRoot r) seg1 (Just seg2) hist g = 
>     let scale1 = scale $ chordCtxt seg1
>         scale2 = scale $ chordCtxt seg2
>         pSpace1 = filter (\p -> elem (mod p 12) scale1) bassRange 
>         pSpace2 = filter (\p -> elem (mod p 12) scale2) bassRange
>         roots2 = filter (\p -> mod p 12 == scale2 !! 0) pSpace2
>         (g1, nextR) = choose g roots2
>         beats = round (4*segDur seg1)
>         (g2, pitches) = walk beats pSpace1 r nextR g1
>         (g3, g4) = split g2
>         bassLine = bassToMusic pitches g3
>     in  (g4, NextRoot nextR, cut (segDur seg1 / 4) bassLine)


We can also convert the SimpleWalkingBass's chords function to match 
the Music (AbsPitch, Volume) type.

> chordFun2 :: PartFun (AbsPitch, Volume) s
> chordFun2 s seg1 seg2 hist g = 
>     let ps = map ((scale $ chordCtxt seg1) !!) [0,2,4,6]
>         d = segDur seg1 / 4
>         m = chord $ map (\p -> (note d (p+60, 100))) ps
>     in  (g, s, m)

Now we can put both into a band.

> myJB :: JazzBand (AbsPitch, Volume) WalkingState
> myJB = [JazzPart Bass AcousticBass wBassFun2 NullState, 
>         JazzPart Harmony ElectricGrandPiano chordFun2 NullState] 

> m = runBand myJB [] ls (mkStdGen 500)