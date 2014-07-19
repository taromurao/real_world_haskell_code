import FoldDir (foldTree, atMostThreePictures)

main :: IO [()]
main = do
    files <- foldTree atMostThreePictures [] "."
    mapM print files
