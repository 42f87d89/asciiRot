asciiRot :: Int -> String -> String
asciiRot _ "" = ""
asciiRot 0 msg = msg
asciiRot n msg = map (\x -> toEnum (33 + (n + (fromEnum x) - 33) `mod` 93) :: Char) msg
