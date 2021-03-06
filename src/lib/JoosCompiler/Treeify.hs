module JoosCompiler.Treeify where

import           Data.List
import           Data.Maybe
import           Data.Tree
import           JoosCompiler.TokenTypeConstants
import           JoosCompiler.TreeUtils

type ClassName = String

type UntaggedToken = String

type JoosTokens = String

type JoosSource = String

type UntaggedParseTree = Tree UntaggedToken

type TaggedParseTree = Tree TaggedToken

type Token = (String, Int, Int)

data TaggedToken = TaggedToken
  { tokenName   :: String
  , tokenString :: String
  , tokenStart  :: Int
  , tokenEnd    :: Int
  }

instance Show TaggedToken where
  show (TaggedToken name str start end) =
    name ++
    (if name == str
       then ""
       else " \"" ++ str ++ "\"") ++
    (if start == end
       then ""
       else " (" ++ show start ++ "," ++ show end ++ ")")

-- Parse a single production rule into a tree with a single parent node.
parseProduction :: String -> UntaggedParseTree
parseProduction line = Node rhs (map singleNode lhs)
  where
    rhs:lhs = words line

-- Convert the bottom-up parse to a Haskell datatype. This is unable to handle
-- a rule with two symbols with the same name. Fortunately, our grammar has no
-- such case.
treeify :: String -> UntaggedParseTree
treeify x =
  case foldl treeify' [] (map parseProduction . lines $ x) of
    [t] -> t
    _   -> error "Could not create tree"

-- Run this for each rule added to the tree.
treeify' :: [UntaggedParseTree] -> UntaggedParseTree -> [UntaggedParseTree]
treeify' forest rule = rule' : forest'
  where
    lhsEq x y = lhs x == lhs y
    rule' =
      Node (lhs rule) [fromMaybe x $ find (lhsEq x) forest | x <- rhs rule]
    forest' = deleteFirstsBy lhsEq forest (rhs rule)

singleNode :: a -> Tree a
singleNode x = Node x []

lhs :: Tree t -> t
lhs (Node x _) = x

rhs :: Tree t -> [Tree t]
rhs (Node _ x) = x

-- Parse the tokens from joos_tokens.txt.
parseTokens :: String -> [Token]
parseTokens = map ((\[x, y, z] -> (x, (read y), (read z))) . words) . lines

-- Convert an untagged tree to a tagged tree with the tokens file.
tagTree'' :: UntaggedParseTree -> [Token] -> TaggedParseTree
tagTree'' utree tokens =
  if remainingTokens == []
    then taggedTree
    else error "Remaining tokens after tagging tree"
  where
    (taggedTree, remainingTokens) = tagTree' utree tokens

-- Recursively convert the tree and return any extra tokens.
-- TODO: Tag inner nodes with (start, end) of their children.
tagTree' :: UntaggedParseTree -> [Token] -> (TaggedParseTree, [Token])
tagTree' (Node x []) [] = ((Node (TaggedToken x x 0 0) []), [])
tagTree' (Node x []) a@((name, start, end):ts)
  | x == name = ((Node (TaggedToken x x start end) []), ts)
  | otherwise = ((Node (TaggedToken x x 0 0) []), a)
tagTree' (Node x xs) ts =
  ((Node (TaggedToken x x 0 0) taggedChildren), remainingTokens)
  where
    mapChildren ::
         [Tree UntaggedToken] -> [Token] -> ([Tree TaggedToken], [Token])
    mapChildren [] ts = ([], ts)
    mapChildren (n@(Node x xs):ns) ts = (taggedChild : ns', ts')
      where
        (taggedChild, remainingTokens) = tagTree' n ts
        (ns', ts') = mapChildren ns remainingTokens
    (taggedChildren, remainingTokens) = mapChildren xs ts

-- Fill the tree with
insertTokenStrings :: TaggedParseTree -> String -> TaggedParseTree
insertTokenStrings tree source = fmap mapToken tree
  where
    mapToken t@(TaggedToken name str start end) =
      if start == end
        then t
        else (TaggedToken name (drop start . take end $ source) start end)

tagTree :: UntaggedParseTree -> JoosTokens -> JoosSource -> TaggedParseTree
tagTree tree tokens source =
  insertTokenStrings (tagTree'' tree (parseTokens tokens)) source

getTypeNameFromDeclaration :: TaggedParseTree -> ClassName
getTypeNameFromDeclaration tree = tokenString $ rootLabel identifierNode
  where
    identifierNode = head $ findChildrenByTokenName kIdentifier tree

findChildrenByTokenName :: String -> TaggedParseTree -> [TaggedParseTree]
findChildrenByTokenName name t = findChildren predicate t
  where
    predicate = ((==) name) . tokenName

findChildrenByTokenName1 :: String -> [TaggedParseTree] -> [TaggedParseTree]
findChildrenByTokenName1 name ts =
  mconcat $ map (findChildrenByTokenName name) ts

findChildByTokenName1 :: String -> [TaggedParseTree] -> TaggedParseTree
findChildByTokenName1 name ts
  | length matches > 1 = error "Too many children"
  | otherwise = head matches
  where
    matches = findChildrenByTokenName1 name ts

findDirectChildrenByTokenName ::
     String -> String -> TaggedParseTree -> [TaggedParseTree]
findDirectChildrenByTokenName childName indirectName t =
  findDirectChildren childPred indirectPred t
  where
    childPred = ((==) childName) . tokenName
    indirectPred = ((==) indirectName) . tokenName

findDirectChildByTokenName ::
     String -> String -> TaggedParseTree -> TaggedParseTree
findDirectChildByTokenName childName indirectName t
  | length matches > 1 = error "Too many matches"
  | otherwise = head matches
  where
    matches = findDirectChildrenByTokenName childName indirectName t

findDirectChildrenByTokenName1 ::
     String -> String -> [TaggedParseTree] -> [TaggedParseTree]
findDirectChildrenByTokenName1 childName indirectName ts =
  mconcat $ map (findDirectChildren childPred indirectPred) ts
  where
    childPred = ((==) childName) . tokenName
    indirectPred = ((==) indirectName) . tokenName
