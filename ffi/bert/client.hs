import Data.BERT
import Network.BERT.Client
import Network.BERT.Transport

main = do
    t <- fromURI "bert://localhost:8080"
    r <- call t "calc" "add" ([123,3000]::[Int])
    case r of
        Right res -> print (res::Int)
        Left _    -> putStrLn "error"
