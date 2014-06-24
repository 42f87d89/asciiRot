import System.Environment
import System.IO

asciiRotC :: Int -> Char -> Char
asciiRotC 0 msg = msg
asciiRotC n msg = toEnum (33 + (n + (fromEnum msg) - 33) `mod` 93) :: Char

asciiRot :: Int -> String -> String
asciiRot n msg = map (asciiRotC n) msg

asciiRotWith :: (Int->Int) -> Int -> String -> String
asciiRotWith _ _ "" = ""
asciiRotWith f acc (x:xs) = (asciiRotC acc x):(asciiRotWith f (f acc) xs)

main = do
    args <- getArgs
    putStrLn $ asciiRotWith (+1) 0 (head args)
