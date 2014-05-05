import Tox.Core
import Control.Concurrent

main = do
        let tox = toxNew False
        address <- toxGetAddress tox
        putStrLn address
        putStrLn $ show $ toxIsconnected tox
        toxBootstrapFromAddress tox "192.254.75.98" False 33445 "951C88B7E75C867418ACDB5D273821372BB5BD652740BCDF623A4FA293E75D2F"
        threadDelay 1000000
        putStrLn $ show $ toxIsconnected tox
