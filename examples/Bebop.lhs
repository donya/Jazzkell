A Bebop Band
Donya Quick

This file demonstrates 2 things:
- Generating drums
- Converting between Music types within the generative system.

Part functions are given for drums, bass, chords (piano), and solo.

Use "play m" to hear some music. Use Ctrl+C to stop.

> module Bebop where
> import Jazzkell
> import Jazzkell.Utils
> import Euterpea
> import System.Random
> import qualified SimpleWalkingBass as SWB
> import qualified SimpleBossa as SB

This implementation is going to use an combined state type that holds 
all the information any part would need. Each part will only use/access 
its own information.

> data BebopState = BebopState{nextRoot :: AbsPitch, lastSoloPitch :: AbsPitch}

> defState :: BebopState
> defState = BebopState (-1) (-1) -- using -1 to indicate a "null" value

====== Drums ======

First, some constants:

> hh = 46 :: AbsPitch -- open hi hat
> hc = 44 :: AbsPitch -- close hi hat
> k = 36 :: AbsPitch -- kick
> kv = 100 :: Volume -- kick volume
> s = 40 :: AbsPitch -- snare
> sv = 85 :: Volume -- snare volume
> crash = 49 :: AbsPitch -- cymbal crash

The following functions are from an older implementation that created
infinite Music values for jazz drums. We can reuse it here to make a PartFun.
The functions are simple stochastic choice over patterns for hi-hats, snare,
and kick. 

Volume is very important for these to sound reasonable, so we must use 
the Music (AbsPitch, Volume) type.

> genHiHats :: StdGen -> Music (AbsPitch, Volume)
> genHiHats g =  
>     let (v, g1) = randomR (0,100) g
>         (g2, x) = choose g1 [note en (hh,v), note en (hh,v), note en (hh,v), note en (hc,v)] 
>                              -- note (2*en/3) (hh,v) :+: note (en/3) (hh,v) -- include this option if you want syncopation
>     in  (if v<30 then rest en else x) :+: genHiHats g2

> genSnare :: StdGen -> Music (AbsPitch, Volume)
> genSnare g = 
>     let (g1, x) = choose g [rest qn :+: note en (s, sv) :+: rest (en + hn),
>                             rest qn :+: note en (s, sv) :+: rest (en) :+: rest qn :+: note en (s, sv) :+: rest (en),
>                             rest dhn :+: note en (s, sv) :+: rest (en)]
>     in  x :+: genSnare g1 

> genKick :: StdGen -> Music (AbsPitch, Volume)
> genKick g = 
>     let (g1, x) = choose g [note en (k,kv) :+: rest dqn :+: note en (k,kv) :+: rest dqn,
>                             note en (k,kv) :+: rest (hn+en) :+: note en (k,kv) :+: rest en]
>     in  x :+: genKick g1 

> bebopDrumsFun :: PartFun (AbsPitch, Volume) s
> bebopDrumsFun s seg1 Nothing hist g = 
>     let d = segDur seg1
>     in  (g, s, note d (k, 120) :=: note d (crash, 120))
> bebopDrumsFun s seg1 (Just seg2) hist g = 
>     let d = segDur seg1
>         gs = splitN g
>         hats = genHiHats (gs !! 0)
>         snare = genSnare (gs !! 1)
>         kicks = genKick (gs !! 2)
>     in  (gs !! 3, s, SB.trimTo seg1 (hats :=: snare :=: kicks))

====== Chords ======

We'll create chords by placing the root, third, fifth, and 
seventh of the chord randomly within the pitch space. 

We'll start by defining some rhythms for the chords.
Negative values will intigate rests of that duration.
So, -en is an eighth rest, while en will indicate a 
note.

> chordRhythms = [[-en, qn, -en, -hn], 
>                 [-en, en, -dqn, en, -qn],
>                 [en,-dqn,en,-dqn], 
>                 [en,-en,-hn,en,-en], 
>                 [-qn, qn, -en, en,-qn]]

Now, a function for selecting pitches for a collection 
of pitch classes. They will be randomly assigned within 
the pitch space.

> choosePitches :: StdGen -> [PCNum] -> [AbsPitch] -> (StdGen, [AbsPitch])
> choosePitches g [] pSpace = (g, [])
> choosePitches g (pc:pcs) pSpace = 
>     let (g1, p) = choose g $ filter (\p -> p `mod` 12 == pc) pSpace
>         (g2, ps) = choosePitches g1 pcs pSpace
>     in  (g2, p:ps)

And finally the part function for generating chords.

> chordFunV :: PartFun (AbsPitch, Volume) s
> chordFunV s seg1 seg2 hist g = 
>     let pcs = map ((scale $ chordCtxt seg1) !!) [0,2,4,6]
>         (g1, ps) = choosePitches g pcs [55..69]
>         measures = ceiling (segDur seg1 / 4)
>         (g2, rhyths) = chooseN g1 measures chordRhythms
>         rhyth = concat rhyths
>         nFun d = chord $ map (note d) $ zip ps $ repeat 80
>         m = line $ map (\d -> if d<0 then rest (-d) else nFun d) rhyth
>     in  case seg2 of Nothing -> (g2, s, nFun (segDur seg1 / 4))
>                      Just _ -> (g2, s, SB.trimTo seg1 m)


====== Walking Bass with Volume ======

Since the drums are using volume, we can lift the bass function
from SimpleWalkingBass into the new Music (AbsPitch,Volume) type.

> wBassFunV :: PartFun (AbsPitch,Volume) BebopState
> wBassFunV s seg1 seg2 hist g = 
>     let seg1' = segMap fst seg1
>         seg2' = maybe Nothing (Just . segMap snd) seg2
>         hist' = map (\(pt,m) -> (pt, mMap fst m)) hist
>         s' = if nextRoot s <=0 then SWB.NullState 
>              else SWB.NextRoot (nextRoot s)
>         (g', SWB.NextRoot nextRP, m) = SWB.wBassFun s' seg1' seg2' hist' g
>     in  (g', s{nextRoot = nextRP}, mMap (\p -> (p,100)) m)

===== Solo Algorithm with Volume =====

We're going to alter the soloPSpace function a bit more invasively,
so rather than lifting it as above we'll redefine it. Here, we 
use stochastic volumes and create a rest if a volume is below 
a certain level.

> soloPSpace = [70..84]

> soloFunV :: PartFun (AbsPitch, Volume) BebopState
> soloFunV s seg1 seg2 hist g0 = 
>     let sPSpace = filterByScale (scale $ chordCtxt seg1) soloPSpace
>         n = ceiling (2*segDur seg1) 
>         (g1, lp) = if lastSoloPitch s < 0 then choose g0 sPSpace
>                      else (g0, lastSoloPitch s)
>         gs = splitN g1
>         ps = take n $ SB.randMelody (gs !! 0) sPSpace lp 
>         vols = randomRs (0,127) (gs !! 1)
>         boostV v = min 127 (v + 40)
>         nFun p v = if v < 40 then rest en else note en (p,boostV v)
>         mel = line $ zipWith nFun ps vols
>         lastP = last ps
>         s' = s{lastSoloPitch = lastP}
>     in  case seg2 of 
>             Nothing -> (gs !! 2, s', note (segDur seg1 / 4) (head ps, 100))
>             Just _ -> (gs !! 2, s', cut (segDur seg1 / 4) mel)


====== Jazz Band Definition ======

> myJB :: JazzBand (AbsPitch, Volume) BebopState
> myJB = [JazzPart Drums Percussion bebopDrumsFun defState, 
>         JazzPart Bass AcousticBass wBassFunV defState,
>         JazzPart Bass ElectricGrandPiano chordFunV defState,
>         JazzPart Bass Marimba soloFunV defState] 

Random lead sheet of major/minor 7th chords:

> randomLeadSheet :: StdGen -> Onset -> LeadSheet (AbsPitch, Volume)
> randomLeadSheet g0 (m,b) = 
>     let (g1, mode) = choose g0 ["M", "m"]
>         (g2, root) = choose g1 [0..11]
>         scale = map ((`mod` 12).(+root)) $ 
>                 if mode=="M" then [0,2,4,5,7,9,11] else [0,2,3,5,7,8,10]
>         pcStr = ["C", "Db", "D", "Eb", "E", "F", "F#", "G", "Ab", "A", "Bb", "B"] !! root
>         chordStr = pcStr ++ mode ++ "7"
>         ctxt = ChordCtxt chordStr scale
>     in  Segment ctxt Regular [] (m,b) 4 (TimeSig 4 4) : randomLeadSheet g2 (m+1, b)

> ls = randomLeadSheet (mkStdGen 600) (0,0)

> m = tempo 2 $ runBand myJB [] ls (mkStdGen 700)

> main = writeMidi "bebop.mid" $ cut 30 m


====== Utility Functions ======

> segMap :: (a -> b) -> Segment a -> Segment b
> segMap f (Segment ctxt cat styles o d ts) = 
>     let styles' = map (fsty f) styles
>     in  Segment ctxt cat styles' o d ts where
>     fsty f (pt, FixedMusic m) = (pt, FixedMusic $ mMap f m)
>     fsty f (pt, Free) = (pt, Free)
>     fsty f (pt, FixedPitch fp) = (pt, FixedPitch fp)

> splitN :: StdGen -> [StdGen]
> splitN g = let (g1, g2) = split g in g1 : splitN g2