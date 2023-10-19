module Parser where
import Data.Typeable

-- Data types
data Token = Program | End | Assign | Read | Write | If | While | Greater | Lesser | GrEqual | LsEqual | Equal | Sum | Sub | Mult | Div | Error
    deriving (Eq, Show)

data TokenTree token =
                    Nil |
                    Node2 Token (TokenTree token) (TokenTree token) |
                    Leaf Token |
                    ErrorLeaf String |
                    Init Token String (TokenTree token) |
                    LeafS String |
                    NodeS String (TokenTree token) |
                    Node1 Token (TokenTree token) |
                    NonParsed String |
                    OpNode Token (TokenTree token, TokenTree token) |
                    FuncReturn (TokenTree token) [String] |
                    Node3 Token (TokenTree token) (TokenTree token) (TokenTree token)
                    deriving (Eq, Show)

cop :: String -> Bool 
cop y = y `elem` [">", "<", ">=", "<=", "!=", "=="]
eop :: String -> Bool 
eop y = y `elem` ["+", "-"]
top :: String -> Bool 
top y = y `elem` ["*", "/"]

comp :: [String] -> TokenTree a
comp s1 = sequen expr cop s1
expr :: [String] -> TokenTree a
expr s1 = sequen term eop s1
term :: [String] -> TokenTree a
term s1 = sequen fact top s1 

stat :: [String] -> TokenTree a
stat (t:s) =
    case t of
        "if" -> let c = comp s
                    c1 = getNonParsedString c
                    c2 = returnFunctionToNode c in
                    if (c1 !! 0 /= "then") then
                        Node2 If c2 (ErrorLeaf "'then' expected")
                    else
                        Node2 If c2 (stat (tail c1))

        "while" ->
            let c = comp s
                c1 = getNonParsedString c
                c2 = returnFunctionToNode c in
                if (c1 !! 0 /= "do") then
                    Node2 While c2 (ErrorLeaf "'do' expected")
                else
                    Node2 While c2 (stat (tail c1))

        "read" -> Node2 Read (LeafS (s !! 0)) (stat (tail s))

        "write" -> Node2 Write (LeafS (s !! 0)) (stat (tail s))

        "end" -> Leaf End

        _ -> if (isIdent t) then
                if (s !! 0 == ":=") then
                  if (s !! 2 == "+") then
                    Node2 Assign (NodeS t (OpNode Sum (LeafS (s !! 1), LeafS (s !! 3)))) (stat (drop 4 s))
                  else if (s !! 2 == "-") then
                    Node2 Assign (NodeS t (OpNode Sub (LeafS (s !! 1), LeafS (s !! 3)))) (stat (drop 4 s))
                  else if (s !! 2 == "*") then
                    Node2 Assign (NodeS t (OpNode Mult (LeafS (s !! 1), LeafS (s !! 3)))) (stat (drop 4 s))
                  else if (s !! 2 == "/") then
                    Node2 Assign (NodeS t (OpNode Div (LeafS (s !! 1), LeafS (s !! 3)))) (stat (drop 4 s))
                  else 
                     Node2 Assign (NodeS t (LeafS (s !! 1))) (stat (drop 2 s))
                else 
                  ErrorLeaf "Invalid assign symbol: ':=' expected"
              else
                ErrorLeaf "Invalid token"

stat _ = ErrorLeaf "Unexpected end of program"


prog :: [String] -> TokenTree a
prog (h:t) = if (h == "program") then
                if (t !! 1 /= ";") then ErrorLeaf "';' expected"
                else
                    Init Program (t !! 0) (stat (drop 2 t))
              else
                ErrorLeaf "'program' expected"
--[ ; x :=  x + 1 end]
sequen :: ([String] -> TokenTree a) -> (String -> Bool) -> [String] -> TokenTree a
sequen nonterm sep s1 =
  if (sep t) then
    let s3 = tail nonParsedString
        x2 = sequen nonterm sep s3
        r1 = nonParsedToLeaf x1
        r2 = nonParsedToLeaf x2
    in
    FuncReturn (OpNode (checkToken t) (r1, r2)) (tail s3)
  else
    x1
  where x1 = nonterm s1
        nonParsedString = getNonParsedString x1
        t = nonParsedString !! 0


fact :: [String] -> TokenTree a
fact s1 =
    if t == "(" then do
        let e = expr (tail s1)
        let rest = tail s1
        FuncReturn e (splitOn rest ")")
    else
        FuncReturn (NonParsed t) (tail s1)
    where t = s1 !! 0

isIdent :: Typeable a => a -> Bool
isIdent a = (typeOf a == typeOf "String")

checkToken :: String -> Token 
checkToken s = case s of
    ">" -> Greater
    ">=" -> GrEqual
    "<" -> Lesser
    "<=" -> LsEqual
    "==" -> Equal
    "+" -> Sum
    "*" -> Mult
    "/" -> Div
    "-" -> Sub
    _ -> Error

getNonParsedValue :: TokenTree a -> String
getNonParsedValue (FuncReturn (NonParsed value) _) = value
getNonParsedValue _ = ""

getNonParsedString :: TokenTree a -> [String]
getNonParsedString (FuncReturn _ s) = s
getNonParsedString _ = []

splitOn :: [String] -> String -> [String]
splitOn (s:t) c = if s == [] then []
                            else if s == c then t
                            else splitOn t c

nonParsedToLeaf :: TokenTree a -> TokenTree a
nonParsedToLeaf a = LeafS (getNonParsedValue a)

returnFunctionToNode :: TokenTree a -> TokenTree a
returnFunctionToNode (FuncReturn tree _) = tree
returnFunctionToNode tree = tree