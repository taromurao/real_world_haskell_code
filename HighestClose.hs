import qualified Data.ByteString.Lazy.Char8 as L

highestClose :: L.ByteString -> Maybe Int
highestClose = maximum . (Nothing:) . map closing . L.lines

highestCloseFrom :: FilePath -> IO()
highestCloseFrom path = do
    contents <- L.readFile path
    print (highestClose contents)


-- private

closing :: L.ByteString -> Maybe Int
closing = readPrice . (!!4) . L.split ','

readPrice :: L.ByteString -> Maybe Int
readPrice str = do
    (dollers, rest) <- L.readInt str
    (cents, more) <- L.readInt (L.tail rest)
    return (dollers * 100 + cents)

