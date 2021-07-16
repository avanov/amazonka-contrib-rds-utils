module  Network.AWS.RDS.Utils
    (   generateDbAuthToken
    )
where

import Network.AWS.Presign
import Network.AWS.Sign.V4 as V4


generateDbAuthToken :: IO ()
generateDbAuthToken = putStrLn "someFunc"
