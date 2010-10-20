{-# OPTIONS_GHC -Wall -Werror -O2 #-}

import Graphics.Vty
import Control.Exception(bracket)
import System.Exit(ExitCode(..), exitWith)
import System.IO(Handle, withFile, IOMode(ReadMode,WriteMode,AppendMode), hGetLine, hPutStrLn, hPutStr)
import System.Environment(getArgs)
import System.Directory(doesFileExist,getHomeDirectory)
import Data.List(group, nubBy, sort, sortBy, isPrefixOf, delete, find, intersect)
import Data.Ord(comparing)
import Data.Maybe(fromJust)
import Data.Function(on)
import System.Console.GetOpt
import Control.Monad(foldM, when)

import qualified System.IO.Strict as Strict

version :: String
version = "0.0.3"

defOutputFile, defHistFileName, defFavFileName, rcFileName :: FilePath
rcFileName      = "~/.hhrc"
defFavFileName  = "~/.hh_favorites"
defHistFileName = "~/.bash_history"
defOutputFile   = "~/.hh.last"

data Config = Config {
    currMode            :: Mode,
    dataFileName        :: FilePath,
    favoritesFileName   :: FilePath,
    outputFileName      :: FilePath,
    saveOnExit          :: Bool
} deriving (Show,Read)

atHome :: FilePath -> IO FilePath
atHome what = fmap (++ what) getHomeDirectory

expandFileName :: FilePath -> IO FilePath
expandFileName ('~':rest) = atHome rest
expandFileName path       = return path

withFile' :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile' path iom f = expandFileName path >>= \n -> withFile n iom f

readFile' :: FilePath -> IO String
readFile' path = expandFileName path >>= \n -> Strict.readFile n

writeFile' :: FilePath -> String -> IO ()
writeFile' path what = expandFileName path >>= flip writeFile what

doesFileExist' :: FilePath -> IO Bool
doesFileExist' path = expandFileName path >>= doesFileExist

loadConfig :: IO Config
loadConfig = catch load def
        where
            load = fmap read (withFile' rcFileName ReadMode hGetLine) :: IO Config
            def _ = return $ Config Freq defHistFileName defFavFileName defOutputFile False

saveConfig :: Config -> IO ()
saveConfig cfg = withFile' rcFileName WriteMode $ flip hPutStrLn (show cfg)

type Item = (String, Int)

type FilterFunc = [String] -> [Item]
type ShowFunc = Bool -> Bool -> Item -> Image
type IsFavFunc = String -> Bool

data Mode = Favorites | Freq | Recent deriving (Eq, Show, Read, Bounded, Enum)

boundedPred, boundedSucc :: (Bounded a, Enum a, Eq a) => a -> a
boundedSucc m
    | m == maxBound = m
    | otherwise     = succ m
boundedPred m
    | m == minBound = m
    | otherwise     = pred m

data Behaviour = Behaviour {
    mbTitle     :: String,
    mbFn        :: Config -> String,
    mbFunc      :: FilterFunc,
    mbShow      :: ShowFunc
}

modes :: [(Mode,Behaviour)]
modes = [(Freq,      Behaviour "freq"      dataFileName      (reverse . freqSort) onlyStr  ),
         (Recent,    Behaviour "recent"    dataFileName      (reverse . same)     onlyStr  ),
         (Favorites, Behaviour "favs"      favoritesFileName (reverse . same)     onlyStr  )]
    where
        same                  = flip zip [1..]

        sel_attr              = def_attr `with_style` standout
        fav_attr              = def_attr `with_style` bold
        fav_sel_attr          = sel_attr `with_style` bold

        onlyStr  False False = string def_attr      . fst
        onlyStr  True  False = string fav_attr      . fst
        onlyStr  False True  = string sel_attr      . fst
        onlyStr  True  True  = string fav_sel_attr  . fst

withVty :: (Vty -> IO a) -> IO a
withVty = bracket mkVty shutdown

fromInt :: Num a => Int -> a
fromInt = fromInteger.toInteger

data Result = Chosen String | Aborted | PrevMode String | NextMode String | Refresh SelectState

maxPrefix :: Eq a => [[a]] -> [a] -> [a]
maxPrefix [] p = p
maxPrefix ps p
    | length p > minimum (map length ps) = p
    | all (isPrefixOf next) ps = maxPrefix ps next
    | otherwise = p
    where
        next = p ++ [head ps !! length p]

type SelectState = (Int, Int, String)

helpScreen :: Vty -> (Int, Int) -> IO ()
helpScreen vty bounds@(_, height) = do
    update vty $ pic_for_image . vert_cat $ map (string def_attr) $ text ++ replicate (height - length text - 1) " " ++ ["hh " ++ version ++ ", q/Esc/Enter to resume" ]
    next_event vty >>= \e -> case e of
        EvKey (KASCII 'q') []   -> return ()
        EvKey KEsc       []     -> return ()
        EvKey KEnter     []     -> return ()
        _                       -> helpScreen vty bounds
    where
        text = [ "Down              - next line",
                 "Up                - previous line",
                 "Enter             - select line",
                 "C-Down            - next favorited line",
                 "C-Up              - previous favorited line",
                 "Home              - first line",
                 "End               - last line",
                 "Left              - previous mode",
                 "Right             - next mode",
                 "C-u               - clear line",
                 "C-a               - add to favorites",
                 "C-r               - remove from favorites",
                 "C-c, C-d, q, Esc  - abort",
                 "Tab               - complete" ]

select :: Vty -> Config -> (Int, Int) ->      [Item] -> IsFavFunc -> ShowFunc -> String -> SelectState -> IO Result
select    vty    cfg       bounds@(width, height) ls'       isFav        showFunc    prefix    state@(top, curr, word) = do
    update vty (pic_for_image . vert_cat $ status ++ items ++ fin) { pic_cursor = cursor }
    e <- next_event vty
    case e of
        EvKey (KASCII 'h')  [MCtrl] -> help
        EvKey KEsc []               -> quit
        EvKey (KASCII 'c')  [MCtrl] -> quit
        EvKey (KASCII 'C')  [MCtrl] -> quit
        EvKey (KASCII 'd')  [MCtrl] -> quit
        EvKey (KASCII 'D')  [MCtrl] -> quit
        EvKey (KASCII 'u')  [MCtrl] -> again (top, curr, "")
        EvKey (KASCII 'U')  [MCtrl] -> again (top, curr, "")
        EvKey (KASCII '\t') []      -> complete
        EvKey (KASCII 'a')  [MCtrl] -> favAdd
        EvKey (KASCII 'r')  [MCtrl] -> favRem
        EvKey KRight        []      -> return $ NextMode word
        EvKey KLeft         []      -> return $ PrevMode word
        EvKey KDown         []      -> down
        EvKey KUp           []      -> up
        EvKey KUp           [MCtrl] -> prevFav
        EvKey KDown         [MCtrl] -> nextFav
        EvKey KHome         []      -> home
        EvKey KEnd          []      -> end
        EvKey KPageUp       []      -> pgup
        EvKey KPageDown     []      -> pgdn
        EvKey (KASCII ch)   []      -> reduce ch
        EvKey KBS           []      -> enhance
        EvKey KEnter        []      -> choose
        EvResize _ _                -> reload
        _                           -> same
    where

        again                                   = select vty cfg bounds ls' isFav showFunc prefix

        same                                    = again state

        reload                                  = return $ Refresh state

        quit                                    = return Aborted

        help                                    = helpScreen vty bounds >> same

        ls                                      = nubBy ((==) `on` fst) $ filter (isPrefixOf word . fst) ls'

        cursor                                  = Cursor (fromInt $ length word + length prefix + 1) 0

        status                                  = [string def_attr (prefix ++ ">") <|> string (def_attr `with_style` bold) word <|> hint ]
            where
                hint                            = string def_attr $ replicate (width - 1 - sum (map length [text, word, prefix]) ) ' ' ++ text
                text                            = "C-H: Help"

        fin                                     = [string def_attr " "]

        items                                   = map mkLine [top..min (top + height - 2) (length ls - 1)]

        complete                                = let word' = maxPrefix (map fst ls) word
                                                  in  again (0, 0, word')

        reduce ch                               = again (0, 0, word ++ [ch])

        enhance
          | length word > 0                     = again (0, 0, init word)
          | otherwise                           = same

        choose
          | curr < length ls                    = return . Chosen $ fst (ls !! curr)
          | otherwise                           = same

        favAdd
          | null ls                             = same
          | otherwise                           = do
                _ <- favRem
                withFile' favFn AppendMode $ flip hPutStrLn (fst $ ls !! curr)
                reload

        favRem
          | null ls                             = same
          | otherwise                           = do
                ll <- readFileLines favFn
                withFile' favFn WriteMode $ \h ->
                   let ll' = delete (fst $ ls !! curr) ll
                   in  hPutStr h $ unlines ll'
                return $ Refresh (top, min curr (max 0 $ length ls - 2), word)

        favFn                                   = favoritesFileName cfg

        home                                    = again (0, 0, word)

        end
          | null ls                             = same
          | otherwise                           = let y' = max (length ls - height + 1) 0
                                                  in  again (y', length ls - 1, word)
        up
          | top == 0 && curr == 0               = same
          | top == curr                         = again (top - 1, curr - 1, word)
          | otherwise                           = again (top,     curr - 1, word)

        down
          | (curr + 1) >= length ls             = same
          | (top + height - 1) == (curr + 1 )   = again (top + 1, curr + 1, word)
          | otherwise                           = again (top,     curr + 1, word)

        pgup                                    = let y' = max (top - height) 0 -- can't use jumpTo, always stick to the top
                                                  in  again (y',  y', word)

        pgdn                                    = jumpTo $ top + height

        indexedFavs                             = filter (isFav . snd) (zip [0..] $ map fst ls)
        gotoFav Nothing                         = same
        gotoFav (Just (curr',_))                = jumpTo curr'
        nextFav                                 = gotoFav $ find ((curr <) . fst) indexedFavs
        prevFav                                 = gotoFav $ find ((curr >) . fst) (reverse indexedFavs)

        mkLine n                                = let which = ls !! n
                                                  in  showFunc (isFav $ fst which) (n == curr) which

        jumpTo curr'
            | curr'' < top                      = next $ max 0 (curr'' - height + 2)
            | curr'' > top + height - 2         = next $ min (length ls - height + 1) curr''
            | otherwise                         = next top
            where
                curr''                          = min (max curr' 0) (length ls - 1)
                next top'                       = again (top', curr'', word)

readFileLines :: FilePath -> IO [String]
readFileLines fn = do
    e <- doesFileExist' fn
    if e
      then fmap lines $ readFile' fn
      else return []

histogram :: FilterFunc
histogram as = let mk a = (head a, length a)
               in  map mk (group $ sort as)

freqSort :: FilterFunc
freqSort = sortBy (comparing snd) . histogram

vtyBounds :: Vty -> IO (Int, Int)
vtyBounds vty = do
    bounds <- display_bounds $ terminal vty
    return (fromInteger( toInteger $ region_width bounds), fromInteger( toInteger $ region_height bounds )) :: IO (Int, Int)

play :: SelectState -> Config -> IO Config
play state cfg = do
    favs <- readFileLines (favoritesFileName cfg)
    ls <- readFileLines fn
    r <- withVty $ \vty -> do
        bounds <- vtyBounds vty
        select vty cfg bounds (func ls) (`elem` favs) showFunc title state
    case r of
        Aborted                     -> exitWith $ ExitFailure 1
        Chosen   cmd                -> writeFile' (outputFileName cfg) (cmd ++ "\n") >> return cfg
        NextMode word'              -> play (0, 0, word') $ cfg { currMode = next }
        PrevMode word'              -> play (0, 0, word') $ cfg { currMode = prev }
        Refresh  state'             -> play state' cfg
    where
        b                            = fromJust $ lookup (currMode cfg) modes
        next                         = boundedSucc $ currMode cfg
        prev                         = boundedPred $ currMode cfg
        title                        = mbTitle b
        fn                           = mbFn b cfg
        showFunc                     = mbShow b
        func                         = mbFunc b . filter (not . null)

data OptFlag = Version | Help | SaveConfig | Init | FromFile FilePath deriving (Eq)

options :: [OptDescr OptFlag]
options = [ Option "v"     ["version"]          (NoArg Version)             "show version number",
            Option "h?"    ["help"]             (NoArg Help)                "show help",
            Option "s"     ["save-cfg"]         (NoArg SaveConfig)          "save configuration on exit",
            Option "i"     ["init"]             (NoArg Init)                "create default rc file if non exists",
            Option "f"     ["from","from-file"] (OptArg fromFile "FILE")    "read history content from FILE" ]
    where
        fromFile Nothing   = FromFile defHistFileName
        fromFile (Just fn) = FromFile fn

header :: String
header  = "Usage: hh [opts...] words"

parseArgs :: [String] -> IO (String, [OptFlag])
parseArgs args = case getOpt Permute options args of
        (flags ,n, [])      -> return ( unwords n, flags )
        (_,     _, errs)    -> ioError $ userError (concat errs ++ usageInfo header options)

handleOpt :: Config -> OptFlag -> IO Config
handleOpt cfg Version        = putStrLn version >> return cfg
handleOpt cfg Help           = putStrLn (usageInfo header options) >> return cfg
handleOpt cfg Init           = loadConfig >>= saveConfig >> return cfg
handleOpt cfg (FromFile fn)  = return $ cfg { dataFileName = fn }
handleOpt cfg SaveConfig     = return $ cfg { saveOnExit = True }

main :: IO ()
main = let shouldRun opts = null $ intersect [Version, Help, Init] opts
       in do
            ( word, opts ) <- getArgs >>= parseArgs
            cfg <- (loadConfig >>= flip (foldM handleOpt) opts)
            cfg' <- if shouldRun opts
                        then play (0, 0, word) cfg
                        else return cfg
            when (saveOnExit cfg') $ saveConfig cfg'

