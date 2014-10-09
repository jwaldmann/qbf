import Parse
import Solve

import System.Environment

main = do
    [ f ] <- getArgs
    q <- qdimacs f
    r <- Solve.completeIO q
    print r

