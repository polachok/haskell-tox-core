import Tox.Core
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Base16 as BS16

main = do
        tox <- toxNew False
        address <- toxGetAddress tox
        putStrLn address
        toxBootstrapFromAddress tox "54.199.139.199" False 33445 "7F9C31FE850E97CEFD4C4591DF93FC757C7C12549DDD55F8EEAECC34FE76C029"
        forever $ do
            toxDo tox
            connected <- toxIsconnected tox
            if connected then putStrLn "Connected" else return ()
            threadDelay 1000
