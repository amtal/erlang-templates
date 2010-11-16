-- #!/usr/bin/runghc 
-- (uncomment to make file executable)
--
-- "X-module function Reference grapher"
--
-- Reads a bunch of Erlang code and produces a module dependency graph.
-- A simple but powerful tool for static analysis of source code.
--
-- FEATURES:
--  - unknown modules shown as boxes, known as ellipses
--  - there's an ignore list for "pure" modules with no interesting side
--    effects
--  - by using core erlang precompilation, eunit/debug code is properly
--    taken into account (I think!)
--
-- DEPENDENCIES:
--  - GHC, a recent Haskell Platform should do. You'll need
-- to pull the following packages via cabal-install:
--      amtal@kos:~$ cabal update
--      amtal@kos:~$ cabal install filemanip
--      amtal@kos:~$ cabal install coreerlang
--  - GraphViz, see your package manager of choice
--  - Won't clean up after itself on Windows due to using `rm`
--
-- CAUTION: hacky work in progress: lists, strings, IO everywhere
--
-- TODO:
--
-- remove duplicate module declaration spam
-- pure ignore list as a runtime option, rather than hardcode
-- modify ignoring *test.erl via cmd line switch
-- have option to not delete .core files (debug)
-- compress multiple call lines into just one, but with a number label
-- compress multiple calls from same function into one with label?
--      (this could be interesting for refactoring between files)
-- 
-- something to avoid constant recompile cost...
-- graph colouring to trace flow through multiple modules?
-- have a "focus" mode that displays major neighbors of node only
--      (to conform to 7+/-2... could auto-grouping also do this?)
--
-- watch HRL file dependencies (and that's a regex thing)
-- detect bad record sharing? ...looks like that'd be a whole separate
--  thing for static analysis of HRL files
--
-- on a similar trend, are supervisor dependencies worth tracking?
-- are they even trackable, due to potential dynamic child adding?
--
--
-- AUTHOR: Amtal <alex.kropivny@gmail.com>
-- LICENSE: give credit if using significant chunks. If you find a way
-- to make money from it, tell me how. Otherwise, do whatever yo.
import Control.Monad (when)
-- File Wrangling
import System.FilePath.Find
import System.Process (runCommand, waitForProcess)
import System.Exit (ExitCode(..))
-- Parsing and Filtering
import Language.CoreErlang.Parser
import Language.CoreErlang.Syntax
-- GraphViz Generation
import Data.List hiding (find)

-- CONFIG: (command line arguments are a sign of weakness)
-- The point of these graphs is to identify important dependencies.
-- Leaf modules with no side effects rarely count as such.
ignoredModules = [ "dict","sets","gb_sets","lists", "proplists",
                   "string", "io_lib", "re", "eunit",
                   -- there's plenty of side effects in erlang, but
                   -- it's also so full of pure funcs it clutters
                   -- graphs (TODO: have a function list for this,
                   -- so spawn and whatnot gets watched but not
                   -- is_atom and whatnot)
                   "erlang"]
-- Things like dict and lists are good examples, while erlang and
-- calendar bad examples due to introducing new processes and the notion
-- of time.


-- * File Wrangling (looks kinda like a Make script, hmmm...)

main = do
    putStrLn "Compiling into intermediate format..."
    find always code "." >>= mapM erl2core
    putStrLn "Parsing files..."
    mods <- find always core "." >>= mapM process 
    putStrLn "Building graph..."
    graph $ concat mods
    return ()
        where
    code = extension ==? ".erl"
        &&? fileName /~? "*tests.erl"
    core = extension ==? ".core"

erl2core :: FilePath -> IO ()
erl2core fname = do
    ret <- exec $ "erlc +to_core "++fname
    when (ret/=ExitSuccess) (putStrLn $ "\tError compiling "++fname)

exec cmd = runCommand cmd >>= waitForProcess

-- * Common Data Structures

data Call = Call CallType String String Int
          | Unimplemented String
          deriving (Show,Read,Eq)
data CallType = Static | DynMod | DynFun | DynAll 
              deriving (Show,Read,Eq)

-- * Parsing and Filtering

process :: FilePath -> IO [(String, [Call])]
process fname = do
    s <- readFile fname
    exec $ "rm "++fname
    case parseModule s of
        Left err -> do
            putStrLn $ "Error parsing "++fname++": "++show err
            return []
        Right tree -> 
            
            return [extractCrossRefs tree]

-- recursively walk tree via pattern matching, looking for
-- ModCalls (todo: generalize this via something like a Functor)
extractCrossRefs :: Ann Module -> (String,[Call])
extractCrossRefs tree = walkM (stripA tree) where
    -- annotations show up everywhere and we don't care
    -- (what are they even for, -spec stuff?)
    stripA (Ann m _) = m
    stripA (Constr m) = m
    -- root of AST
    walkM (Module (Atom name) _ _ funs) = (name,calls) where 
        calls = concatMap walkFD funs
    walkFD (FunDef _ a) = walk (stripA a)
    --walk :: Exp -> [a]
    -- Exp forms the bulk of the AST, recursively referring to itself
    -- via Exps (which are just annoted Exp?)
    walk (App ex exs) = walkE ex ++ concatMap walkE exs
    walk (Lambda _ ex) = walkE ex
    walk (Seq ex ex') = walkE ex ++ walkE ex'
    walk (Let (_,ex) ex') = walkE ex ++ walkE ex'
    walk (LetRec fs ex) = concatMap walkFD fs ++ walkE ex
    walk (Case ex as) = walkE ex ++ concatMap (walkAlt . stripA) as
    walk (Rec as _) = concatMap (walkAlt . stripA) as
    walk (Tuple exs) = concatMap walkE exs
    walk (List l) = walkL l where
        walkL (L es) = concatMap walkE es
        walkL (LL es e) = walkE e ++ concatMap walkE es
    walk (Binary bs) = concatMap walkB bs where
        walkB (BitString _ es) = concatMap walkE es
    walk (Op _ es) = concatMap walkE es
    walk (Try es (_, es') (_, es'')) = concatMap walkE (es:es':es'':[])
    walk (Catch es) = walkE es
    -- collect extramodular calls
    walk (ModCall (m,f) args) = concatMap walkE args 
                             ++ [(get m f (length args))] where
        get (Exp(Constr(Lit(LAtom(Atom m))))) 
            (Exp(Constr(Lit(LAtom(Atom f))))) 
            a = Call Static m f a
        get (Exp(Constr(Var m)))
            (Exp(Constr(Lit(LAtom(Atom f))))) 
            a = Call DynMod m f a
        get (Exp(Constr(Lit(LAtom(Atom m))))) 
            (Exp(Constr(Var f)))
            a = Call DynFun m f a
        get (Exp(Constr(Var m)))
            (Exp(Constr(Var f)))
            a = Call DynAll m f a
        get foo bar arity = Unimplemented (show (foo,bar,arity))
    -- everything else can't contain side effects, and can be ignored
    -- (probably)
    walk _ = []
    --walkE :: Exps -> [a]
    walkE (Exp a) = walk (stripA a)
    walkE (Exps as) = concatMap (\a->walk .stripA $ a) $ stripA as
    --walkAlt :: Alt -> [a]
    -- fairly sure guards have no side effects, and thus aren't
    -- worth walking...
    walkAlt (Alt _ _ ex) = walkE ex

-- * GraphViz Generation 
--
-- Note: combining Data.Graph.Inductive and Data.GraphViz may produce
-- cromulent results. Then again, it might just be a waste of time.
-- 
-- Would this allow better control of graph attributes than raw output?


graph :: [(String,[Call])] -> IO ()
graph ms = do
    writeFile "xr.dot" (mkGraph ms)
    exec "dot -Tpng xr.dot > xr.png"
    return ()
     
mkGraph :: [(String,[Call])] -> String
mkGraph ms = concat . intersperse "\n" 
           $ ["digraph xr {"]++mods++calls++["\n}"]
        where
    -- set up module styles before drawing calls
    mods :: [String]
    mods = edge internal "ellipse" ++ edge external "box"
            where
        internal = map fst ms
        external = filter (not . flip elem internal) called
        called = filter bad 
               . concat 
               . map (map getTarg . snd) 
               $ ms
        getTarg (Call _ to _ _) = to
        getTarg _ = "this edge case should no longer occur"
        bad "-" = False -- I don't understand how this happens
        bad _ = True -- nor care enough to figure it out now
        edge ss shp = concat . map ppMod $ ss where
            ppMod s = ["\t"++esc s++" [shape="++shp++"]"]
    -- call edges
    calls :: [String]
    calls = concat . map ppModCalls $ ms 
    ppModCalls (mName,mCalls) = map ppCall mCalls where
        ppCall (Call DynFun m _f _a)
            | m `elem` ignoredModules = comm "ignored call to" m
            | otherwise = arr mName m++" [arrowhead=dot]"
        ppCall (Call DynAll m _f _a) = arr mName m++" [arrowhead=dot]"
        ppCall (Call _ m _f _a)
            | m `elem` ignoredModules = comm "ignored call to" m
            | otherwise = arr mName m
        ppCall (Unimplemented err) = comm "unhandled call" err
        arr m m' = esc m++"->"++esc m'
        comm s m = "/* "++s++" "++m++" */"
    -- utility
    esc s = '\"':s++"\""
