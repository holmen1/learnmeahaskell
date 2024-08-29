import Control.Monad
{- 
Because mapping a function that returns an I/O action over a list and then
sequencing it is so common, the utility functions mapM.
forM (located in Control.Monad) is like mapM, only that it has its parameters
switched around. The first parameter is the list and the second one is the
function to map over that list, which is then sequenced. -}
  
main = do   
    colors <- forM [1,2,3,4] (\a -> do
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        getLine)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors

-- $ runhaskell for.hs 
-- Which color do you associate with the number 1?
-- blue
-- Which color do you associate with the number 2?
-- red
-- Which color do you associate with the number 3?
-- green
-- Which color do you associate with the number 4?
-- white
-- The colors that you associate with 1, 2, 3 and 4 are: 
-- blue
-- red
-- green
-- white

