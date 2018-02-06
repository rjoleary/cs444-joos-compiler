import Data.List
import Data.Maybe
import Data.Tree

type ParseTree = Tree String

singleNode = flip Node []
lhs (Node x _) = x
rhs (Node _ x) = x

-- Parse a single production rule into a tree with a single parent node.
parseProduction :: String -> ParseTree
parseProduction line = Node rhs (map singleNode lhs)
        where rhs:lhs = words line

-- Convert the bottom-up parse to a Haskell datatype.
--treeify :: String -> ParseTree
treeify x = case foldl treeify' [] (map parseProduction . lines $ x) of
                [t] -> t
                _   -> error "Could not create tree"

-- Run this for each rule added to the tree.
treeify' :: [ParseTree] -> ParseTree -> [ParseTree]
treeify' forest rule = rule':forest'
    where
        lhsEq x y = lhs x == lhs y
        -- TODO: The following line fails on rules with multiple of the same
        -- token on the RHS. It is not an issue for our current grammar.
        rule'     = Node (lhs rule) [fromMaybe x $ find (lhsEq x) forest | x <- rhs rule]
        forest'   = deleteFirstsBy lhsEq forest (rhs rule)

main = do
    contents <- readFile "test/joos_tree.txt"
    let tree = treeify contents
    putStrLn (drawTree tree)
    --putStrLn $ case weed tree of
    --    Just err -> "Error: " ++ err
    --    Nothing  -> "OK"
