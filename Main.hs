import Parse
import Solve

import System.Environment

main = do
    [ f ] <- getArgs
    q <- read_qdimacs f
    r <- Solve.completeIO q
    print r

