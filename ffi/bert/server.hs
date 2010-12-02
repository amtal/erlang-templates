import Data.BERT
import Network.BERT.Server
import Network.BERT.Transport


main = do
    t <- fromHostPort "" 8080
    serve t dispatch 


dispatch "calc" "add" [IntTerm a, IntTerm b] =
    return $ Success $ IntTerm (a+b)
dispatch "calc" _ _ =
    return NoSuchFunction
dispatch _ _ _ =
    return NoSuchModule
