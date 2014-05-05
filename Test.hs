import Tox.Core

main = do
        let tox = toxNew False
        putStrLn $ show $ toxIsconnected tox
