main :: IO()

main = do
    contents <- getContents
    let valors = words contents
    let res = foldl (+) 0 (map (read) valors::[Int])
    putStr $ (show $ res) ++ "\n"
