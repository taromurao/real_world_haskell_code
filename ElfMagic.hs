import qualified Data.ByteString.Lazy as L

isElfFile :: FilePath -> IO Bool
isElfFile path = do
    content <- L.readFile path
    return (hasElfMagic content)

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    -- In the original text, the first bytes are [0x7f, 0x45, 0x4c, 0x46]
    -- Under OSX, it looks that first four bytes of executable files are as follows:
    where elfMagic = L.pack [207,250,237,254]
