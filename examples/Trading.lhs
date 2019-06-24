Minimal Solo Trading Example
Donya Quick

> module Trading where
> import Jazzkell
> import Euterpea
> import Jazzkell.Utils
> import System.Random

This is an extremely simple example of how history can be
used to send information from one soloist to another. For
the sake of isolating just that behavior, the lead sheet
segments will all be assumed to be C-major. The system is
also stateless. Soloist A will play something and then
soloist B will respond by recombining fragments of what
it just heard.

The purpose of this example is NOT to make amazing music.
It is just to show how solo trading and history usage can
be modeled in an extremely basic way where the behavior
of each algorithm can is easy to assess by ear.

Soloist A is simply going to play one of three possible
patterns repeated for the length of the segment.

> aPats :: [[AbsPitch]]
> aPats = [
>     [60,62,64,65,67,69,71,72],
>     [60,64,67,64],
>     [60,62,64,65,67,65,64,62]
>     ]

> soloFunA :: PartFun AbsPitch s
> soloFunA s seg1 seg2 hist g =
>     let (g2,pat) = choose g aPats
>         m = line $ map (note en) $ concat $ repeat pat
>     in  if category seg1 == CustomSeg "A"
>         then (g2, s, cut (segDur seg1 / 4) m)
>         else (g, s, rest (segDur seg1 / 4))


Soloist B will do recombinance in chunks of size 4 on
what it heard from Soloist A.

> recombine :: StdGen -> [a] -> [a]
> recombine g [] = []
> recombine g xs =
>     let (i,g2) = randomR (0, max 0 $ length xs - 1 - 4) g
>         chunk = take 4 $ drop i xs
>     in  chunk ++ recombine g2 xs

> getSoloHist :: History a -> Music a
> getSoloHist ((Solo, m):h) = m
> getSoloHist (x:h) = getSoloHist h
> getSoloHist [] = error "No history!"

> soloFunB :: PartFun AbsPitch s
> soloFunB s seg1 seg2 hist g =
>     let (g2, g3) = split g
>         histSolo = getSoloHist hist
>         ps = pitches histSolo
>         m = line $ map (note en) $ recombine g2 ps
>     in  if category seg1 == CustomSeg "B"
>         then (g3, s, cut (segDur seg1 / 4) m)
>         else (g, s, rest (segDur seg1 / 4))


Now we create a lead sheet that specifies which performer
plays on which segment. We put together the jazz band and
run it. Use "play m" to hear the result.

> cM7 = ChordCtxt "CM7" [0,2,4,5,7,9,11] -- C major
> cSeg1  = Segment cM7 (CustomSeg "A") [] (1,0)   4   (TimeSig 4 4)
> cSeg2  = Segment cM7 (CustomSeg "B") [] (2,0)   4   (TimeSig 4 4)
> cLS = concat $ repeat [cSeg1, cSeg2]

> myJB :: JazzBand AbsPitch ()
> myJB = [JazzPart Solo Marimba soloFunA (),
>         JazzPart Solo Vibraphone soloFunB ()]

> m = runBand myJB [] cLS (mkStdGen 0)
