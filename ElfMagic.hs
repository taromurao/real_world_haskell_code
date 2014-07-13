import qualified Data.ByteString.Lazy as L

isElfFile :: FilePath -> IO Bool
isElfFile path = do
    content <- L.readFile path
    return (hasElfMagic content)

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [207,250,237,254]
