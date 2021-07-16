isBigGang :: Int -> (Bool, String) 
isBigGang x =  (x > 9, "Compared gang size to 9")


applyLog :: (a,String) -> (a -> (b, String)) -> (b, String)
applyLog (x,log) f = let (y,newlog) = f x in (y,log ++ newlog)


