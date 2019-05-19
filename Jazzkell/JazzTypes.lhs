A Functional Model for Jazz Improvisation: Implementation
Donya Quick

> module Jazzkell.JazzTypes where
> import Euterpea
> import Data.List (sort)
> import System.Random

A part name refers to a particular roll in improvisational jazz. We'll define
four and allow an additional custom constructor for generality.

> data PartType = Solo | Harmony | Bass | Drums | PartType String
>     deriving (Eq, Show, Ord)

A ChordCtxt, or chord context, is information given by notations like "C7"
above a staff on a lead sheet. It has a string symbol that implies a particular
scale. We will represent Scales as a list of pitch class numbers, or Ints.
We will assume that the Scale's members are within the range [0,11] and that
the list is organized in order of the scale's pitch class cycle. In other words,
G-major would be [7,9,11,0,2,4,6].

Note that we are not using Euterpea's PitchClass type here. This is to avoid
issues of enharmonic equivalence, since Euterpea's PitchClass type has multiple
constructors that map to the same numerical pitch class (for example, Cs for
C sharp and Df for D flat).

> type PCNum = Int
> type Scale = [PCNum]
> data ChordCtxt = ChordCtxt{sym::String, scale::Scale} -- TODO: maybe include pitchSpace?
>     deriving (Eq, Show)

A Segment is a portion of a lead sheet having a homogenous chord context.
Segments may have fixed features for some parts, which typically chracterizes
the lead sheet as a composition rather than as simply a chord progression.

> data SegStyle a = Free | FixedPitch [AbsPitch] | FixedMusic (Music a)
>     deriving (Eq, Show)

> data SegCat = Intro | Regular | Bridge | Ending | End | CustomSeg String
>     deriving (Eq, Show, Ord)

> type Measure = Int
> type Beat = Rational
> type Onset = (Measure, Beat) 
> data TimeSig = TimeSig Int Int -- TimeSig 3 4 is 3/4, TimeSig 4 4 is 4/4, etc.
>     deriving (Eq, Show)

> data Segment a = Segment{
>     chordCtxt :: ChordCtxt, -- harmonic context
>     category  :: SegCat, -- intro, bridge etc.
>     styles    :: [(PartType, SegStyle a)], -- improv style (free, fixed sections, etc.)
>     segOnset  :: Onset, -- when does the segment start?
>     segDur    :: Beat, -- when does the segment end?
>     timeSig   :: TimeSig} -- what's the time signature in this segment?
>     deriving (Eq, Show)

Finally, a lead sheet is simply a list of segments.

> type LeadSheet a = [Segment a]

A state is a collection of features that are tracked between generative iterations.
This is left completely polymorphic. We denote is as the type variable s in the
following definitions.

On the performance side, a PartFun is a function from a part's State, the current
Segment, the next Segment (if one exists), and what the band just played, to an
updated State and newly emitted Music for the part.

> type History a = [(PartType, Music a)]
> type PartFun a s = s -> Segment a -> Maybe (Segment a) -> History a -> StdGen -> (StdGen, s, Music a)

We will then define JazzPart to represent one performer in a group. It will
have a PartType, an instrument, a PartFun defining its behavior, and a current
State. A JazzPart is specific to a style an instrument.

> data JazzPart a s = JazzPart{
>     partType   :: PartType,
>     instr      :: InstrumentName,
>     partFun    :: PartFun a s,
>     state      :: s}

A JazzBand, then, is a simply a list of JazzParts.

> type JazzBand a s = [JazzPart a s]

When we run the JazzBand, we need only supply a LeadSheet. Note that LeadSheet
may potentially be infinite!

> runBand :: JazzBand a s -> History a -> LeadSheet a -> StdGen -> Music a
> runBand [] h segs g = rest 0 -- no band to play!
> runBand jb h [] g = rest 0
> runBand jb h (seg1:segs) g =
>     let seg2 = if null segs then Nothing else Just (head segs)
>         result = runSegment jb h seg1 seg2 g
>         (gs, states, ms) = unzip3 result
>         jb' = zipWith (\st jp -> jp{state=st}) states jb
>         h' = zip (map partType jb) ms
>     in  foldr1 (:=:) ms :+: runBand jb' h' segs (last gs)

> runSegment :: JazzBand a s -> History a -> Segment a -> Maybe (Segment a) -> StdGen -> [(StdGen, s, Music a)]
> runSegment [] h seg1 seg2 g = []
> runSegment (jp:jps) h seg1 seg2 g =
>     let (g', st, m) = partFun jp (state jp) seg1 seg2 h g
>         m' = instrument (instr jp) m
>     in  (g', st, m') : runSegment jps h seg1 seg2 g'
