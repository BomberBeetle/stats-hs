import Data.List


quantile p set = (tail (take xf rol) !! 0)*(1-(rank-fromIntegral xf)) + (tail (take xc rol) !!0 )*(1-(rank - fromIntegral xc))  
    where rol = sort set
          rank = 1 + (fromIntegral $ length set)*p
          xf = floor rank
          xc = ceiling rank

                        