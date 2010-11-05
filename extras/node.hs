import Foreign.Erlang
import Control.Monad

-- Needs the following line added to /etc/services:
--      epmd        4369/tcp        # Erlang Port Mapper Daemon
---
-- After that, it fails with message:
--      too few bytes. Failed reading at byte position 2
---
-- The message isn't in the three source files I checked. One of the imported
-- libraries then? Feh, it seems close.
-- Same message occurs on gen_server:call. Methinkd the error-handling in
-- this library can use work...
--
-- Okay, tweaking the node names, I've managed to get something that works.
-- I wonder if I can contact the original author and get permission to update
-- the code (and probably the license - GPL is too restrictive for something
-- as crucial as FFI between two complementary languages!)
main = do
    -- initialize a node and a mail box
    self <- createSelf "hs@niflheim"
    box <- createMBox self
    putStrLn "Initialized."
    -- tell a gen_server on node 'erl' the mail box pid
    let node = "erl"
        pid = Right "node"
        msg = (ErlAtom "register", mboxSelf box)
    genCast box node pid msg
    putStrLn "Register message sent."
    -- listen for "calls" from erl and reply
    forever $ do
        req <- mboxRecv box
        let resp = handle req
        mboxSend box node pid resp
        putStrLn $ "Received "++show req++", responded with "++show resp

handle (ErlAtom "ping") = ErlAtom "pong"
