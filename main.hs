import Data.Bits
import Data.Array
import Data.Char
import Control.Monad

data TreeNode = Branch TreeNode Bool TreeNode | Singleton Bool

instance Show TreeNode where
    show (Branch l t r) = "(" ++ show l ++ " " ++ (if t then "X" else ".") ++ " " ++ show r ++ ")"
    show (Singleton t) = if t then "X" else "."

this :: TreeNode -> Bool
this (Branch _ x _) = x
this (Singleton x) = x

getElem :: String -> TreeNode -> String
getElem ('<':ps) (Branch l _ _) = getElem ps l
getElem ('>':ps) (Branch _ _ r) = getElem ps r
getElem "" node = if this node then "X" else "."

buildTree :: String -> TreeNode
buildTree inp = let (l, t) = readTree (filter (/= ' ') inp)
                in if null l then t else error $ "Parse error: left " ++ l

readTree :: String -> (String, TreeNode)
readTree ('X':xs) = (xs, Singleton True)
readTree ('.':xs) = (xs, Singleton False)
readTree ('(':xs) = (xs'', Branch lSub (x == 'X') rSub)
    where
        (x:xs', lSub) = readTree xs
        (')':xs'', rSub) = readTree xs'

ruleArr :: Int -> Array (Bool, Bool, Bool, Bool) Bool
ruleArr rule = arr
    where
        sstate = (False, False, False, False)
        estate = (True, True, True, True)
        ss = [False, True]
        arr = array (sstate, estate) $
            zip [ (b1, b2, b3, b4) | b1 <- ss, b2 <- ss, b3 <- ss, b4 <- ss ] $
            map (testBit rule) [0..15]

calc :: Array (Bool, Bool, Bool, Bool) Bool -> TreeNode -> [(Int, String)] -> [String]
calc rules tree qs = map (\(i, q) -> getElem q (trace!i)) $ zip queryInd querys
    where
        (deltaIs, querys) = unzip qs
        queryInd :: [Int]
        queryInd = tail . scanl (+) 0 $ deltaIs
        maxInd :: Int
        maxInd = foldl1 max queryInd
        trace = array (0, maxInd) $ scanl (\(_, last) i -> (i, nextTree False last)) (0, tree) [1..maxInd]
        nextTree :: Bool -> TreeNode -> TreeNode
        nextTree p (Branch l t r) = let l' = nextTree t l
                                        r' = nextTree t r
                                    in Branch l' (rules!(p, this l, t, this r)) r'
        nextTree p (Singleton t)  = Singleton (rules!(p, False, t, False))

main = do
    rule <- readLn
    let rules = ruleArr rule
    tStr <- getLine
    let tree = buildTree tStr
    t <- readLn
    qs <- forM [1..t] $ \_ -> do
        ql <- getLine
        return . (\(n, ' ':'[':xs) -> (read n :: Int, init xs)) $ span (\x -> isDigit x || x == '-') ql
    putStr . unlines $ calc rules tree qs
