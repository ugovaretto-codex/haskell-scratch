import Data.List
count xs = let sxs =  sort xs 
               (_,_,_,c) = foldl (\(last, prev,acc,m) x -> 
                                  if (x-last) <= 1 && (x-prev) <= 1 
                                  then (x,prev,acc+1,m) 
                                  else let p | x - prev > 1 = x
                                             | otherwise = last 
                                       in (x,p,1,max m acc))
                        (head sxs, head sxs, 0,0) sxs
           in c--
main = interact $ show . count . map (read :: String->Int) . tail . words