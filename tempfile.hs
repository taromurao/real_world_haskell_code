import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception(finally, catch)

main :: IO()
main = withTempFile "mytmp.txt" myAction

myAction :: FilePath -> Handle -> IO()
myAction tempname temph =
    do
        putStrLn "Welcome to tempfile.hs"
        putStrLn $ "I have a temporary file at " ++ tempname

        pos <- hTell temph
        putStrLn $ "My initial position is " ++ show pos

        let tempdata = show [1..10]
        putStrLn $ "Writing one line containing " ++ show (length tempdata) ++ tempdata
        hPutStrLn temph tempdata

        pos <- hTell temph
        putStrLn $ "After writing, my new position is " ++ show pos

        putStrLn $ "The file content is "
        hSeek temph AbsoluteSeek 0
        c <- hGetContents temph
        putStrLn c

        putStrLn $ "Which could be expressed as this Haskell literal:"
        print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func =
    do
        -- If the type in the lambda function is omitted, GHC compalains with
        -- No instance for (GHC.Exception.Exception e0)
        -- arising from a use of `catch'
        -- The type variable `e0' is ambiguous
        -- Possible fix: add a type signature that fixes these type variable(s)
        tempdir <- catch (getTemporaryDirectory) (\(_ :: IOError) -> return ".")
        (tempfile, temph) <- openTempFile tempdir pattern
        finally
            (func tempfile temph)
            (do hClose temph
                removeFile tempfile)

