> module SimpleWalkingBass where
> import JazzTypes
> import Utils
> import Euterpea
> import System.Random

> data WalkingState = NextRoot AbsPitch | NullState
>     deriving (Eq, Show)

> makeStep :: [AbsPitch] -> AbsPitch -> AbsPitch -> StdGen -> (StdGen, AbsPitch)
> makeStep pitchSpace p1 p2 g = 
>     let pH = max p1 p2
>         pL = min p1 p2
>         midPs = filter (\p -> p<pH && p>pL) pitchSpace
>         nearPs = filter (\p -> p<pL+7 && p>pL-7 && p/=pL && p/=pH) pitchSpace
>         ps = if null midPs then nearPs else midPs
>     in  choose g ps

> nSteps :: Int -> [AbsPitch] -> AbsPitch -> AbsPitch -> StdGen -> (StdGen, [AbsPitch])
> nSteps 0 pSpace p1 p2 g = (g, [])
> nSteps i pSpace p1 p2 g = 
>     let (g2, pMid) = makeStep pSpace p1 p2 g
>         (g3, ps) = nSteps (i-1) pSpace pMid p2 g2
>     in  (g3, p1 : ps)

> bassRange = [36..50] :: [AbsPitch]

> wBassFun :: PartFun AbsPitch WalkingState
> wBassFun NullState seg1 seg2 hist g = 
>     let scale1 = scale $ chordCtxt seg1
>         pSpace = filter (\p -> (elem (mod p 12) scale1)) bassRange
>         roots = filter (\p -> mod p 12 == scale1 !! 0) pSpace
>         (g', r) = choose g roots
>         beats = round (segDur seg1)
>     in  wBassFun (NextRoot r) seg1 seg2 hist g
> wBassFun (NextRoot r) seg1 Nothing hist g = 
>     (g, NullState, note (segDur seg1) r)
> wBassFun (NextRoot r) seg1 (Just seg2) hist g = 
>     let scale1 = scale $ chordCtxt seg1
>         scale2 = scale $ chordCtxt seg2
>         pSpace1 = filter (\p -> elem (mod p 12) scale1) bassRange 
>         pSpace2 = filter (\p -> elem (mod p 12) scale2) bassRange
>         roots2 = filter (\p -> mod p 12 == scale2 !! 0) pSpace2
>         (g1, nextR) = choose g roots2
>         beats = round (4*segDur seg1)
>         (g2, pitches) = nSteps beats pSpace1 r nextR g1
>         bassLine = line $ map (note qn) pitches 
>     in  (g2, NextRoot nextR, cut (segDur seg1/4) bassLine)

> chordFun :: PartFun AbsPitch s
> chordFun s seg1 seg2 hist g = 
>     let ps = map ((scale $ chordCtxt seg1) !!) [0,2,4,6]
>         d = segDur seg1 / 4
>         m = chord $ map (note d . (+60)) ps
>     in  (g, s, m)


> myJB = [JazzPart Bass AcousticBass wBassFun NullState, 
>         JazzPart Bass ElectricGrandPiano chordFun NullState] 

Finally, we'll test it on a simple lead sheet.
     
> cM7 = ChordCtxt "CM7" [0,2,4,5,7,9,11]
> dmM7 = ChordCtxt "DmM7" [2,4,5,7,9,11,0] -- dorian
> g7 = ChordCtxt "G7" [7,9,11,0,2,4,5]
     
> seg1  = Segment dmM7 Regular [] (0,0)   4   (TimeSig 4 4)
> seg2  = Segment g7   Regular [] (1,0)   4   (TimeSig 4 4)
> seg3  = Segment cM7  Regular [] (2,0)   4   (TimeSig 4 4)
> seg4  = Segment cM7  Regular [] (3,0)   4   (TimeSig 4 4)

> ls = concat $ repeat [seg1, seg2, seg3, seg4]

> m = runBand myJB [] ls (mkStdGen 5)