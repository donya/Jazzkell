> module Utils where
> import Euterpea
> import JazzTypes
> import Data.List 
> import System.Random


> type PitchSpace = [AbsPitch]

> derivePitchSpace :: Scale -> AbsPitch -> AbsPitch -> PitchSpace
> derivePitchSpace [] lower upper = []
> derivePitchSpace s lower upper = 
>     let s' = sort s
>         psRaw = concatMap (\o -> map (+o) s') [0..10]
>     in  filter (\p -> p<=upper && p>=lower) psRaw

> filterByScale :: Scale -> PitchSpace -> PitchSpace
> filterByScale s = filter (\p -> (p `mod` 12) `elem` s)

> orderByNearest :: PitchSpace -> AbsPitch -> [AbsPitch]
> orderByNearest ps p = 
>     let dists = map (abs . subtract p) ps
>     in  map snd $ sort $ zip dists ps

> nearest :: PitchSpace -> AbsPitch -> AbsPitch
> nearest [] p = error "(nearest) empty pitch space."
> nearest ps p = head $ orderByNearest ps p

> pitches :: Music a -> [a]
> pitches = mFold pFun (++) (++) (\c l -> l) where
>     pFun (Note d p) = [p]
>     pFun (Rest d) = []

> durs :: Music a -> [Dur]
> durs = mFold pFun (++) (++) (\c l -> l) where
>     pFun (Note d p) = [d]
>     pFun (Rest d) = [d]


> infSplit :: StdGen -> [StdGen] -- necessary to get many generators from just one
> infSplit g = let (g1, g2) = split g in g1 : infSplit g2

choose: select uniformly at random from a list

> choose :: StdGen -> [a] -> (StdGen, a) 
> choose g [] = error "Nothing to choose from!"
> choose g xs = 
>     let (r, g') = next g
>     in  (g', xs !! (r `mod` length xs))

> chooseN :: StdGen -> Int -> [a] -> (StdGen, [a])
> chooseN g0 i xs = if i <= 0 then (g0, []) else
>     let (g1, ys) = chooseN g0 (i-1) xs
>         (g2, y) = choose g1 xs
>     in  (g2, y:ys)

chooseDist: select an item accoring to its probability (a Double)

> chooseDist :: StdGen -> [(a,Double)] -> (StdGen, a)
> chooseDist g ps =
>     let (r, g1) = randomR (0.0, 1.0::Double) g
>     in  (g1, chooseRec r ps) where
>     chooseRec v [(x,p)] = x
>     chooseRec v [] = error "Nothing to choose from!"
>     chooseRec v ((x,p):ps) = if v <= p && p > 0 then x else chooseRec (v-p) ps

chooseDistNorm: select an tem according to its probability after normalization
(input probabilities need not be normalized)

> chooseDistNorm ::  StdGen -> [(a,Double)] -> (StdGen, a)
> chooseDistNorm g xs = 
>     let (vals,probs) = unzip xs
>     in  chooseDist g $ zip vals (map (/sum probs) probs)
