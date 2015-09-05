{-
 - Lets implement the UNIX echo command
 - The program arguments are simply printed to the standard output.
 - If the first argument is -n, this argument is not printed, and no trailing newline is printed
 -}
import System.Environment
import System.Random
import Data.List


main :: IO ()
main = do
  (command:args) <- getArgs
  if command == "-n"
    then putStr $ unwords args
    else putStrLn $ unwords (command:args)

{- Write a lottery number picker
 - This function should take a StdGen instance, and produce a list of six unique numbers between 1 and 49, in numerical order
 -}
lottery :: StdGen -> [Int]
lottery = sort . take 6 . randomRs (1,49)

-- Example:
--lottery (mkStdGen 11)
