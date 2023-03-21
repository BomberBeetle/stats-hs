import Data.Fixed

arithAvg set = (foldl (\x -> \y -> x+y) 0 set) / fromIntegral (length set)

geoAvg set = (foldl (\x -> \y -> x*y) 1 set)**(1/fromIntegral (length set))

geoAvgPond set = (foldl (\x -> \(y,z) -> x*(y**z)) 1 set)**(1/(foldl (\x -> \(_,z) -> x+z) 0 set))

harmonicAvg set = fromIntegral (length set) / (foldl (\x -> \y -> x+(1/y)) 0 set)

quadraticAvg set = sqrt ((foldl (\x -> \y -> x + y**2) 0 set)/fromIntegral (length set))

genAvg :: (Floating a, Foldable t) => t a -> a -> a
genAvg set p = ((foldl (\x -> \y -> x + y**p) 0 set)/fromIntegral (length set))**(1/p)

prunedAvg::[Float] -> Float -> Float
prunedAvg set perc
    | i `mod'` 1 == 0 = prunedAvgPos set (round (i))
    | otherwise = ((prunedAvgPos set (ceiling (i)))*(1-(fromIntegral (ceiling i)-i)) + (prunedAvgPos set (floor (i)))*(1-(i-fromIntegral (floor i)))) -- TODO: see if this is a simple midpoint or a weighted avg 
    where i = (fromIntegral (length set))*(perc/100)

prunedAvgPos set n = (sum prunedSet)/(fromIntegral (length prunedSet))
    where prunedSet = drop n $ reverse $ drop n set